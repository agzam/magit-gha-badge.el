;;; magit-gha-badge.el --- GitHub Actions status badge in the magit status buffer -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2026 Ag Ibragimov
;;
;; Author: Ag Ibragimov <agzam.ibragimov@gmail.com>
;; Maintainer: Ag Ibragimov <agzam.ibragimov@gmail.com>
;; Created: August 18, 2026
;; Version: 0.3.0
;; Keywords: vc tools
;; Homepage: https://github.com/agzam/magit-gha-badge.el
;; Package-Requires: ((emacs "29.1") (magit "4.0.0") (ghub "4.0.0"))
;;
;; SPDX-License-Identifier: GPL-3.0-or-later
;;
;; This file is not part of GNU Emacs.

;;; Commentary:

;; A README-style GitHub Actions status badge as a one-line section at
;; the top of the magit status buffer, fetched asynchronously from the
;; GitHub API (via `ghub') on every status refresh.  RET or mouse-1 on
;; the badge - and `magit-gha-badge-browse-run' from any buffer in the
;; repo - refetches and opens the newest run page.  In a fork the
;; upstream repo is asked too, since that is where the runs of a pull
;; request from the fork live.  No steady-state timers: a bounded poll
;; runs only while a run is pending, or while a pushed commit has no
;; run yet, and the status buffer stays displayed.
;;
;; Setup:
;;
;;   (magit-add-section-hook 'magit-status-sections-hook
;;                           #'magit-gha-badge-insert
;;                           'magit-insert-status-headers
;;                           'append)
;;
;; Auth: requests go through `ghub-request' with `magit-gha-badge-auth'
;; (default `ghub'), i.e. the generic ghub token: an authinfo entry like
;;
;;   machine api.github.com login USER^ghub password TOKEN
;;
;; plus `github.user' set in git config.

;;; Code:

(require 'magit)
(require 'ghub)
(require 'seq)

(defgroup magit-gha-badge nil
  "GitHub Actions status badge in the magit status buffer."
  :group 'magit-extensions
  :prefix "magit-gha-badge-")

(defcustom magit-gha-badge-poll-interval 30
  "Seconds between re-fetches while a workflow run is pending."
  :type 'natnum)

(defcustom magit-gha-badge-auth 'ghub
  "AUTH argument `ghub-request' is called with.
A symbol names an authinfo token entry (login \"USER^SYMBOL\"), a
string is used as the token itself."
  :type '(choice symbol string))

(defconst magit-gha-badge--wait-ticks 3
  "Fetches spent waiting for GitHub to create a run for the pushed commit.")

(defconst magit-gha-badge--run-keys
  '(:status :conclusion :url :sha :repo :created :workflow :title :no-runs :error)
  "State keys a fetch result owns.  Merging a result replaces all of them.")

(defvar magit-gha-badge--cache (make-hash-table :test #'equal)
  "Per-repo GHA state keyed by repo toplevel.
Value is a plist of the run fields in `magit-gha-badge--run-keys' plus
bookkeeping (:slug :parent :pushed :waits :fetching :timer :section
:buffer).")

;;;; Pure parts

(defun magit-gha-badge--slug (url)
  "Extract \"owner/repo\" from remote URL, or nil for a non-github URL."
  (and url
       (string-match (concat "\\`\\(?:git@github\\.com:"
                             "\\|\\(?:ssh://git@\\|https://\\)github\\.com/\\)"
                             "\\([^/]+/[^/]+?\\)\\(?:\\.git\\)?/?\\'")
                     url)
       (match-string 1 url)))

(defun magit-gha-badge--same-slug-p (a b)
  "Return non-nil when slugs A and B name the same repo.
Case-insensitively: a remote URL keeps whatever case it was typed with,
the API answers with the canonical spelling."
  (and a b (string-equal-ignore-case a b)))

(defun magit-gha-badge--parse-payload (payload &optional head-slug)
  "Turn an actions/runs response PAYLOAD alist into a result plist.
HEAD-SLUG, when given, keeps only runs whose head repo is HEAD-SLUG: the
run list of an upstream repo also holds that repo's own runs for a
branch of the same name.
A payload without the `workflow_runs' key is not an answer (error
document), so it reads as an error, not as an empty branch."
  (if (assq 'workflow_runs payload)
      (if-let* ((run (seq-find
                      (lambda (run)
                        (or (null head-slug)
                            (magit-gha-badge--same-slug-p
                             head-slug
                             (alist-get 'full_name
                                        (alist-get 'head_repository run)))))
                      (alist-get 'workflow_runs payload))))
          (list :status (alist-get 'status run)
                :conclusion (alist-get 'conclusion run)
                :url (alist-get 'html_url run)
                :sha (alist-get 'head_sha run)
                :repo (alist-get 'full_name (alist-get 'repository run))
                :created (alist-get 'created_at run)
                :workflow (alist-get 'name run)
                :title (alist-get 'display_title run))
        '(:no-runs t))
    '(:error t)))

(defun magit-gha-badge--parse-repo (payload)
  "Return the upstream slug of a repo response PAYLOAD, nil when not a fork."
  (alist-get 'full_name (alist-get 'parent payload)))

(defun magit-gha-badge--combine (results)
  "Reduce fetch RESULTS to the newest run, else an error, else no-runs.
A repo and its upstream answer separately, so an error from one must not
hide the run the other did return."
  (or (car (seq-sort (lambda (a b)
                       (string< (plist-get b :created) (plist-get a :created)))
                     (seq-filter (lambda (result) (plist-get result :created))
                                 results)))
      (and (seq-some (lambda (result) (plist-get result :error)) results)
           '(:error t))
      '(:no-runs t)))

(defun magit-gha-badge--glyph (status conclusion)
  "Return the badge glyph for STATUS, then CONCLUSION once completed.
An unknown non-completed STATUS reads as pending: GitHub adds statuses
over time and they all mean \"not done yet\"."
  (pcase status
    ((or "queued" "waiting" "requested" "pending") "🟣")
    ("in_progress" "🟡")
    ("completed"
     (pcase conclusion
       ("success" "🟢")
       ((or "failure" "timed_out" "startup_failure") "🔴")
       ((or "cancelled" "skipped" "neutral" "stale") "⚪")
       ("action_required" "🟠")
       (_ "⚪")))
    (_ "🟣")))

(defun magit-gha-badge--merge-result (state result)
  "Merge fetch RESULT into STATE, preserving bookkeeping fields.
An error keeps the previous run fields so the badge shows the last
known truth instead of going blank on a transient fetch failure."
  (let ((state (copy-sequence state)))
    (if (plist-get result :error)
        (plist-put state :error t)
      (dolist (key magit-gha-badge--run-keys)
        (setq state (plist-put state key (plist-get result key))))
      state)))

(defun magit-gha-badge--waiting-p (state)
  "Return non-nil while STATE waits for a run of the pushed commit.
GitHub creates a run seconds after the push lands, so the fetch that a
push triggers still answers with the previous run.  The wait is bounded
by `magit-gha-badge--wait-ticks': a pushed commit no workflow matches
would otherwise poll for as long as the status buffer lives."
  (let ((pushed (plist-get state :pushed)))
    (and pushed
         (not (plist-get state :error))
         (not (equal pushed (plist-get state :sha)))
         (< (or (plist-get state :waits) 0) magit-gha-badge--wait-ticks))))

(defun magit-gha-badge--count-wait (state)
  "Return STATE with the wait counter advanced, or reset once the run lands.
Counting answered fetches, not poll ticks, keeps a manual refresh from
extending the wait a push opened."
  (let ((state (copy-sequence state))
        (pushed (plist-get state :pushed)))
    (plist-put state :waits
               (if (or (null pushed) (equal pushed (plist-get state :sha)))
                   0
                 (1+ (or (plist-get state :waits) 0))))))

(defun magit-gha-badge--pending-p (state)
  "Return non-nil when STATE earns another poll.
Either the run it holds is still going, or the pushed commit has no run
of its own yet."
  (and (not (plist-get state :error))
       (or (and (plist-get state :status)
                (not (equal (plist-get state :status) "completed")))
           (magit-gha-badge--waiting-p state))))

(defun magit-gha-badge--foreign-repo (state)
  "Return STATE's run repo when it is not the repo the branch pushes to.
A pull request from a fork runs upstream, and a badge showing someone
else's run has to say whose it is."
  (let ((repo (plist-get state :repo))
        (slug (plist-get state :slug)))
    (and repo slug
         (not (magit-gha-badge--same-slug-p repo slug))
         repo)))

(defun magit-gha-badge--age (created)
  "Return CREATED, an API timestamp, as a phrase like \"3 days ago\".
Nil without a timestamp.  The units are magit's own, the ones its log
margin counts commit ages in."
  (when created
    (pcase-let ((`(,count ,unit) (magit--age (float-time (date-to-time created)))))
      (format "%s %s ago" count unit))))

(defun magit-gha-badge--badge (state)
  "Return glyph + workflow + short sha + age for STATE, nil when nothing to show.
The run's repo follows the sha when the run did not happen in the repo
the branch is pushed to, and how long ago the run started follows that.
A trailing ellipsis marks a run older than the pushed commit, whose own
run GitHub has yet to create.  Nil for a cached no-runs branch, and for
a fetch failure with no previously fetched truth to keep showing.  No
result at all renders a placeholder, so a cold status buffer shows the
fetch is underway."
  (let ((sha (plist-get state :sha)))
    (cond
     ((plist-get state :status)
      (string-join
       (delq nil (list (magit-gha-badge--glyph (plist-get state :status)
                                               (plist-get state :conclusion))
                       (plist-get state :workflow)
                       (and sha (substring sha 0 (min 7 (length sha))))
                       (when-let* ((foreign (magit-gha-badge--foreign-repo state)))
                         (format "(%s)" foreign))
                       (when-let* ((age (magit-gha-badge--age
                                         (plist-get state :created))))
                         (concat "- " age))
                       (and (magit-gha-badge--waiting-p state) "⋯")))
       " "))
     ((plist-get state :error) nil)
     ((magit-gha-badge--waiting-p state) "⋯")
     ((plist-get state :no-runs) nil)
     (t "⋯"))))

(defun magit-gha-badge--line (state)
  "Return the full propertized badge line for STATE, nil when omitted."
  (when-let* ((badge (magit-gha-badge--badge state)))
    (concat (propertize "Checks"
                        'face 'magit-section-heading
                        'font-lock-face 'magit-section-heading)
            "   "
            (if-let* ((title (plist-get state :title)))
                (propertize badge 'help-echo title)
              badge))))

(defun magit-gha-badge--browse-target (state)
  "Return (URL . MESSAGE) for browsing STATE's run.  Either half can be nil.
A stale URL still beats no URL, so a failed fetch - and a push GitHub has
no run for yet - opens the previous run and says so."
  (let ((url (plist-get state :url)))
    (cond
     ((and url (plist-get state :error))
      (cons url "Fetching workflow runs failed - opening the last known run"))
     (url (cons url (and (magit-gha-badge--waiting-p state)
                         (concat "No run for the pushed commit yet"
                                 " - opening the previous one"))))
     ((plist-get state :error) (cons nil "Fetching workflow runs failed"))
     (t (cons nil "No workflow runs for this branch")))))

;;;; Cache

(defun magit-gha-badge--state (repo)
  "Return the cached state plist for REPO."
  (gethash repo magit-gha-badge--cache))

(defun magit-gha-badge--set (repo &rest props)
  "Set PROPS (key value ...) in REPO's cached state."
  (let ((state (gethash repo magit-gha-badge--cache)))
    (while props
      (setq state (plist-put state (pop props) (pop props))))
    (puthash repo state magit-gha-badge--cache)))

;;;; Async fetch

(defun magit-gha-badge--remote-url (branch)
  "Return the URL of BRANCH's push-remote, upstream, or origin."
  (magit-get "remote"
             (or (magit-get-push-remote branch)
                 (magit-get-remote branch)
                 "origin")
             "url"))

(defun magit-gha-badge--pushed-sha (branch)
  "Return the sha BRANCH points at on the remote it is pushed to, or nil.
The remote-tracking ref, not HEAD: a run can only exist for a commit
that left the machine."
  (magit-rev-verify (or (magit-get-push-branch branch)
                        (magit-get-upstream-branch branch)
                        (concat "origin/" branch))))

(defun magit-gha-badge--parent-slug (repo slug callback)
  "Call CALLBACK with the upstream slug of SLUG, nil when SLUG is no fork.
Cached per REPO, since fork parentage does not change.  A failed lookup
is not cached, so the next fetch asks again."
  (let ((cached (plist-get (magit-gha-badge--state repo) :parent)))
    (if (equal (car cached) slug)
        (funcall callback (cdr cached))
      (ghub-request "GET" (format "/repos/%s" slug) nil
                    :auth magit-gha-badge-auth
                    :callback (lambda (payload &rest _)
                                (let ((parent (magit-gha-badge--parse-repo payload)))
                                  (magit-gha-badge--set repo :parent (cons slug parent))
                                  (funcall callback parent)))
                    :errorback (lambda (&rest _) (funcall callback nil))))))

(defun magit-gha-badge--request-runs (branch slug parent callback)
  "Ask SLUG and PARENT for BRANCH's newest run, pass the newest to CALLBACK.
PARENT, non-nil only for a fork, is asked with a wider page and its
answer is narrowed to runs whose head repo is SLUG: the runs it has for
a branch of that name may well be its own."
  (let* ((slugs (delq nil (list slug parent)))
         (pending (length slugs))
         (results nil)
         (report (lambda (result)
                   (push result results)
                   (when (zerop (setq pending (1- pending)))
                     (funcall callback (magit-gha-badge--combine results))))))
    (dolist (each slugs)
      (let ((head (unless (equal each slug) slug)))
        (ghub-request "GET" (format "/repos/%s/actions/runs" each)
                      `((branch . ,branch)
                        (per_page . ,(if head "10" "1"))
                        (exclude_pull_requests . "true"))
                      :auth magit-gha-badge-auth
                      :callback (lambda (payload &rest _)
                                  (funcall report (magit-gha-badge--parse-payload
                                                   payload head)))
                      :errorback (lambda (&rest _)
                                   (funcall report '(:error t))))))))

(defun magit-gha-badge--fetch (repo &optional callback force)
  "Start an async API fetch of REPO's latest run unless one is in flight.
FORCE runs regardless: a fetch a user asked for must not answer with the
result of a request that went out before whatever the user just did.
CALLBACK, when given, receives the updated state once the fetch lands."
  (let ((started (plist-get (magit-gha-badge--state repo) :fetching)))
    ;; the in-flight guard expires: url.el can drop a callback, and a
    ;; permanently stuck flag would freeze the badge on ⋯ forever
    (when (or force (not started) (< 30 (- (float-time) started)))
      (let* ((default-directory repo)
             (branch (magit-get-current-branch))
             (slug (and branch
                        (magit-gha-badge--slug
                         (magit-gha-badge--remote-url branch)))))
        (when slug
          (let ((state (magit-gha-badge--state repo))
                (pushed (magit-gha-badge--pushed-sha branch)))
            (magit-gha-badge--set repo
                                  :fetching (float-time)
                                  :slug slug
                                  :pushed pushed
                                  ;; a new push opens a new wait
                                  :waits (if (equal pushed (plist-get state :pushed))
                                             (plist-get state :waits)
                                           0)))
          (magit-gha-badge--parent-slug
           repo slug
           (lambda (parent)
             (magit-gha-badge--request-runs
              branch slug parent
              (lambda (result)
                (magit-gha-badge--on-fetch repo result callback))))))))))

(defun magit-gha-badge--on-fetch (repo result &optional callback)
  "Cache fetch RESULT for REPO, redraw the badge, manage the poll.
CALLBACK, when given, receives the updated state."
  (magit-gha-badge--set repo :fetching nil)
  (let ((state (magit-gha-badge--count-wait
                (magit-gha-badge--merge-result (magit-gha-badge--state repo) result))))
    (puthash repo state magit-gha-badge--cache)
    (let ((section (plist-get state :section))
          (buffer (plist-get state :buffer)))
      (cond
       (section
        (oset section value (plist-get state :url))
        (magit-gha-badge--swap state))
       ;; the last render omitted the badge line (cached no-runs or
       ;; error), so there is nothing to swap the fresh result into;
       ;; re-running the inserters is the only way to bring it back
       ((and (buffer-live-p buffer) (magit-gha-badge--line state))
        (with-current-buffer buffer (magit-refresh-buffer)))))
    (when (magit-gha-badge--pending-p state)
      (magit-gha-badge--arm repo))
    (when callback
      (funcall callback state))))

(defun magit-gha-badge--swap (state)
  "Replace the badge line in place from STATE, bypassing a full refresh.
Skips silently unless the section's start marker still points at the
badge line (the buffer may be killed, or erased by a refresh whose
inserter has not run yet - writing there would corrupt the buffer).
The start marker's insertion-type is t, so it is flipped for the insert
to keep the new text inside the section span."
  (let* ((section (plist-get state :section))
         (buffer (plist-get state :buffer))
         (start (and section (oref section start))))
    (when (and (markerp start)
               (buffer-live-p buffer)
               (eq (marker-buffer start) buffer))
      (with-current-buffer buffer
        (when (eq (get-text-property start 'magit-section) section)
          (save-excursion
            (goto-char start)
            (let ((inhibit-read-only t)
                  (text (magit-gha-badge--line state))
                  (map (oref section keymap)))
              (delete-region start (line-end-position))
              (when text
                (set-marker-insertion-type start nil)
                (goto-char start)
                (insert (apply #'propertize text
                               'magit-section section
                               (and map (list 'keymap (symbol-value map)))))
                (set-marker-insertion-type start t)))))))))

;;;; Bounded poll

(defun magit-gha-badge--arm (repo)
  "Arm one poll tick for REPO unless one is already pending."
  (unless (plist-get (magit-gha-badge--state repo) :timer)
    (magit-gha-badge--set repo :timer
                          (run-with-timer magit-gha-badge-poll-interval nil
                                          #'magit-gha-badge--tick repo))))

(defun magit-gha-badge--tick (repo)
  "Re-fetch REPO while its status buffer is still displayed.
The timer dies here unless the callback of the new fetch re-arms it."
  (magit-gha-badge--set repo :timer nil)
  (let ((buffer (plist-get (magit-gha-badge--state repo) :buffer)))
    (when (and (buffer-live-p buffer)
               (get-buffer-window buffer 'visible))
      (magit-gha-badge--fetch repo))))

(defun magit-gha-badge--cleanup ()
  "Cancel poll timers owned by the dying status buffer."
  (let ((buffer (current-buffer)))
    (maphash (lambda (repo state)
               (when (eq (plist-get state :buffer) buffer)
                 (when-let* ((timer (plist-get state :timer)))
                   (cancel-timer timer))
                 (magit-gha-badge--set repo :timer nil :section nil :buffer nil)))
             magit-gha-badge--cache)))

;;;; Section and commands

;; magit resolves a section's keymap from its type by name
(defvar-keymap magit-gha-badge-section-map
  :doc "Keymap for `gha-badge' sections."
  "RET" #'magit-gha-badge-browse-run
  "<mouse-1>" #'magit-gha-badge-browse-run)

;;;###autoload
(defun magit-gha-badge-insert ()
  "Insert the GitHub Actions badge line for the current branch.
Meant for `magit-status-sections-hook'.  Renders instantly from cache
and kicks a fire-and-forget refetch; the refresh itself never waits on
the network."
  (when-let* ((repo (magit-toplevel)))
    (if-let* ((branch (magit-get-current-branch))
              ((magit-gha-badge--slug (magit-gha-badge--remote-url branch))))
        (let* ((state (magit-gha-badge--state repo))
               (line (magit-gha-badge--line state))
               (section (and line
                             (magit-insert-section (gha-badge (plist-get state :url))
                               (insert line "\n")))))
          (magit-gha-badge--set repo
                                :section section
                                :buffer (current-buffer))
          (add-hook 'kill-buffer-hook #'magit-gha-badge--cleanup nil t)
          (magit-gha-badge--fetch repo))
      ;; foreign remote or detached HEAD: drop stale render targets so a
      ;; late fetch callback cannot write into a refreshed buffer
      (magit-gha-badge--set repo :section nil :buffer nil))))

;;;###autoload
(defun magit-gha-badge-browse-run ()
  "Open the newest GitHub Actions run page for the current branch.
Fetches first, and browses what that answers with: the badge - and the
run URL it carries - can predate the push that just triggered a run.
Falls back to the last known URL when the fetch fails."
  (interactive)
  (let ((repo (magit-toplevel)))
    (unless repo
      (user-error "Not inside a git repository"))
    (let* ((default-directory repo)
           (branch (magit-get-current-branch)))
      (unless branch
        (user-error "Detached HEAD - no branch to look up runs for"))
      (unless (magit-gha-badge--slug (magit-gha-badge--remote-url branch))
        (user-error "No github remote to look up runs on")))
    (message "Fetching the latest workflow run...")
    (magit-gha-badge--fetch
     repo
     (lambda (state)
       (let ((target (magit-gha-badge--browse-target state)))
         (when (cdr target)
           (message "%s" (cdr target)))
         (when (car target)
           (browse-url (car target)))))
     'force)))

(provide 'magit-gha-badge)
;;; magit-gha-badge.el ends here
