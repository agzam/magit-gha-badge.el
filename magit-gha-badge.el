;;; magit-gha-badge.el --- GitHub Actions status badge in the magit status buffer -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2026 Ag Ibragimov
;;
;; Author: Ag Ibragimov <agzam.ibragimov@gmail.com>
;; Maintainer: Ag Ibragimov <agzam.ibragimov@gmail.com>
;; Created: August 18, 2026
;; Version: 0.1.0
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
;; the badge opens the exact workflow run page;
;; `magit-gha-badge-browse-run' does the same from any buffer in the
;; repo.  No steady-state timers: a bounded poll runs only while a run
;; is pending and the status buffer stays displayed.
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

(defvar magit-gha-badge--cache (make-hash-table :test #'equal)
  "Per-repo GHA state keyed by repo toplevel.
Value is a plist of run fields (:status :conclusion :url :sha :workflow
:title :no-runs :error) plus bookkeeping (:fetching :timer :section
:buffer :callback).")

;;;; Pure parts

(defun magit-gha-badge--slug (url)
  "Extract \"owner/repo\" from remote URL, or nil for a non-github URL."
  (and url
       (string-match (concat "\\`\\(?:git@github\\.com:"
                             "\\|\\(?:ssh://git@\\|https://\\)github\\.com/\\)"
                             "\\([^/]+/[^/]+?\\)\\(?:\\.git\\)?/?\\'")
                     url)
       (match-string 1 url)))

(defun magit-gha-badge--parse-payload (payload)
  "Turn an actions/runs response PAYLOAD alist into a result plist.
A payload without the `workflow_runs' key is not an answer (error
document), so it reads as an error, not as an empty branch."
  (if (assq 'workflow_runs payload)
      (if-let* ((run (car (alist-get 'workflow_runs payload))))
          (list :status (alist-get 'status run)
                :conclusion (alist-get 'conclusion run)
                :url (alist-get 'html_url run)
                :sha (alist-get 'head_sha run)
                :workflow (alist-get 'name run)
                :title (alist-get 'display_title run))
        '(:no-runs t))
    '(:error t)))

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
      (dolist (key '(:status :conclusion :url :sha :workflow :title :no-runs :error))
        (setq state (plist-put state key (plist-get result key))))
      state)))

(defun magit-gha-badge--pending-p (state)
  "Return non-nil when STATE holds a fresh, still-running result."
  (and (plist-get state :status)
       (not (equal (plist-get state :status) "completed"))
       (not (plist-get state :error))
       (not (plist-get state :no-runs))))

(defun magit-gha-badge--badge (state)
  "Return glyph + workflow + short sha for STATE, nil when nothing to show.
Nil for a cached no-runs branch, and for a fetch failure with no
previously fetched truth to keep showing.  No result at all renders a
placeholder, so a cold status buffer shows the fetch is underway."
  (let ((sha (plist-get state :sha)))
    (cond
     ((plist-get state :status)
      (string-join
       (delq nil (list (magit-gha-badge--glyph (plist-get state :status)
                                               (plist-get state :conclusion))
                       (plist-get state :workflow)
                       (and sha (substring sha 0 (min 7 (length sha))))))
       " "))
     ((or (plist-get state :no-runs) (plist-get state :error)) nil)
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

(defun magit-gha-badge--fetch (repo &optional callback)
  "Start an async API fetch of REPO's latest run unless one is in flight.
CALLBACK, when given, receives the updated state once a fetch lands;
with a fetch already in flight it attaches to that one."
  (when callback
    (magit-gha-badge--set repo :callback callback))
  (let ((started (plist-get (magit-gha-badge--state repo) :fetching)))
    ;; the in-flight guard expires: url.el can drop a callback, and a
    ;; permanently stuck flag would freeze the badge on ⋯ forever
    (unless (and started (< (- (float-time) started) 30))
      (let* ((default-directory repo)
             (branch (magit-get-current-branch))
             (slug (and branch
                        (magit-gha-badge--slug
                         (magit-gha-badge--remote-url branch)))))
        (when slug
          (magit-gha-badge--set repo :fetching (float-time))
          (ghub-request "GET" (format "/repos/%s/actions/runs" slug)
                        `((branch . ,branch) (per_page . "1"))
                        :auth magit-gha-badge-auth
                        :callback (lambda (payload &rest _)
                                    (magit-gha-badge--on-fetch
                                     repo (magit-gha-badge--parse-payload payload)))
                        :errorback (lambda (&rest _)
                                     (magit-gha-badge--on-fetch repo '(:error t)))))))))

(defun magit-gha-badge--on-fetch (repo result)
  "Cache fetch RESULT for REPO, redraw the badge, manage the poll."
  (magit-gha-badge--set repo :fetching nil)
  (let ((state (magit-gha-badge--merge-result (magit-gha-badge--state repo) result)))
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
    (when-let* ((callback (plist-get state :callback)))
      (magit-gha-badge--set repo :callback nil)
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
On the badge section itself this uses its exact run URL; elsewhere the
cached URL when there is one, else it fetches first and browses on
arrival."
  (interactive)
  (if-let* ((url (magit-section-value-if 'gha-badge)))
      (browse-url url)
    (let ((repo (magit-toplevel)))
      (unless repo
        (user-error "Not inside a git repository"))
      (if-let* ((url (plist-get (magit-gha-badge--state repo) :url)))
          (browse-url url)
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
           (cond
            ((plist-get state :url) (browse-url (plist-get state :url)))
            ((plist-get state :error) (message "Fetching workflow runs failed"))
            (t (message "No workflow runs for this branch")))))))))

(provide 'magit-gha-badge)
;;; magit-gha-badge.el ends here
