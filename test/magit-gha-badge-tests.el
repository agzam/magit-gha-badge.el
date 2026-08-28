;;; test/magit-gha-badge-tests.el --- magit-gha-badge specs -*- lexical-binding: t; -*-

;;; Commentary:
;; Pure-function coverage: parsing, glyph mapping, merge/re-arm decisions,
;; line rendering, cache updates, plus the fan-out over a fork and its
;; upstream against a faked `ghub-request'.  No network, no live magit
;; buffers - marker/timer integration is not replica-testable (buffers
;; without real sections prove nothing).

;;; Code:

(require 'buttercup)
(require 'magit-gha-badge)

;; shape of GET /repos/OWNER/REPO/actions/runs as ghub parses it
;; (alist keys, arrays as lists, json null as nil)
(defvar magit-gha-badge-tests--payload
  '((total_count . 1)
    (workflow_runs
     . (((status . "completed")
         (conclusion . "success")
         (html_url . "https://github.com/agzam/.emacs.d/actions/runs/123")
         (head_sha . "0ecbb23deadbeef0123456789abcdef012345678")
         (name . "CI")
         (display_title . "Fix parser crash")
         (created_at . "2026-08-18T10:00:00Z")
         (repository . ((full_name . "agzam/.emacs.d")))
         (head_repository . ((full_name . "agzam/.emacs.d"))))))))

;; what the upstream repo answers for a branch name a fork also uses: its
;; own newer run first, the fork's pull request run second
(defvar magit-gha-badge-tests--upstream-payload
  '((total_count . 2)
    (workflow_runs
     . (((status . "completed")
         (conclusion . "failure")
         (html_url . "https://github.com/cli/cli/actions/runs/999")
         (head_sha . "beef")
         (name . "Lint")
         (created_at . "2026-08-19T12:00:00Z")
         (repository . ((full_name . "cli/cli")))
         (head_repository . ((full_name . "cli/cli"))))
        ((status . "in_progress")
         (conclusion . nil)
         (html_url . "https://github.com/cli/cli/actions/runs/1000")
         (head_sha . "cafe")
         (name . "Tests")
         (created_at . "2026-08-19T11:00:00Z")
         (repository . ((full_name . "cli/cli")))
         (head_repository . ((full_name . "agzam/cli"))))))))

(defvar magit-gha-badge-tests--answers nil
  "Alist of API resource to the payload the `ghub-request' stub replies with.
A resource absent from it answers through the errorback.")

(defvar magit-gha-badge-tests--calls nil
  "Alist of API resource to params, in reverse order of request.")

(defun magit-gha-badge-tests--ghub (_method resource &optional params &rest args)
  "Answer a request for RESOURCE with PARAMS from the canned answers.
ARGS carries the `ghub-request' keywords, of which the callbacks are used."
  (push (cons resource params) magit-gha-badge-tests--calls)
  (if-let* ((answer (assoc resource magit-gha-badge-tests--answers)))
      (funcall (plist-get args :callback) (cdr answer))
    (funcall (plist-get args :errorback))))

(defun magit-gha-badge-tests--params (resource)
  "Return the params RESOURCE was requested with."
  (cdr (assoc resource magit-gha-badge-tests--calls)))

(defun magit-gha-badge-tests--ago (seconds)
  "Return an API-shaped timestamp SECONDS before now.
Now is truncated to the whole second, so the age an expectation sees is
SECONDS plus a sub-second remainder: only offsets of a minute and up
round to a stable count."
  (format-time-string "%FT%TZ" (- (time-convert nil 'integer) seconds) t))

(describe "magit-gha-badge--slug"
  (it "parses ssh remotes"
    (expect (magit-gha-badge--slug "git@github.com:agzam/.emacs.d.git")
            :to-equal "agzam/.emacs.d"))
  (it "parses https remotes with and without .git"
    (expect (magit-gha-badge--slug "https://github.com/magit/magit.git")
            :to-equal "magit/magit")
    (expect (magit-gha-badge--slug "https://github.com/magit/magit")
            :to-equal "magit/magit"))
  (it "parses ssh:// remotes"
    (expect (magit-gha-badge--slug "ssh://git@github.com/owner/repo.git")
            :to-equal "owner/repo"))
  (it "rejects non-github remotes"
    (expect (magit-gha-badge--slug "git@gitlab.com:owner/repo.git") :to-be nil)
    (expect (magit-gha-badge--slug "https://git.sr.ht/~owner/repo") :to-be nil)
    (expect (magit-gha-badge--slug nil) :to-be nil)))

(describe "magit-gha-badge--parse-payload"
  (it "maps a completed run's fields"
    (let ((result (magit-gha-badge--parse-payload magit-gha-badge-tests--payload)))
      (expect (plist-get result :status) :to-equal "completed")
      (expect (plist-get result :conclusion) :to-equal "success")
      (expect (plist-get result :url)
              :to-equal "https://github.com/agzam/.emacs.d/actions/runs/123")
      (expect (plist-get result :sha)
              :to-equal "0ecbb23deadbeef0123456789abcdef012345678")
      (expect (plist-get result :repo) :to-equal "agzam/.emacs.d")
      (expect (plist-get result :created) :to-equal "2026-08-18T10:00:00Z")
      (expect (plist-get result :workflow) :to-equal "CI")
      (expect (plist-get result :title) :to-equal "Fix parser crash")))
  (it "keeps a null conclusion nil while a run is in progress"
    (let ((result (magit-gha-badge--parse-payload
                   '((total_count . 1)
                     (workflow_runs . (((status . "in_progress")
                                       (conclusion . nil))))))))
      (expect (plist-get result :status) :to-equal "in_progress")
      (expect (plist-get result :conclusion) :to-be nil)))
  (it "picks the fork's run out of an upstream run list"
    (let ((result (magit-gha-badge--parse-payload
                   magit-gha-badge-tests--upstream-payload "agzam/cli")))
      (expect (plist-get result :workflow) :to-equal "Tests")
      (expect (plist-get result :repo) :to-equal "cli/cli")))
  (it "picks the fork's run whatever case the remote spells it in"
    (expect (plist-get (magit-gha-badge--parse-payload
                        magit-gha-badge-tests--upstream-payload "Agzam/CLI")
                       :workflow)
            :to-equal "Tests"))
  (it "reports no runs when an upstream list holds none of the fork's"
    (expect (magit-gha-badge--parse-payload
             magit-gha-badge-tests--upstream-payload "someone/cli")
            :to-equal '(:no-runs t)))
  (it "turns an empty run list into the no-runs sentinel"
    (expect (magit-gha-badge--parse-payload '((total_count . 0) (workflow_runs . ())))
            :to-equal '(:no-runs t)))
  (it "treats a payload without workflow_runs as an error"
    (expect (magit-gha-badge--parse-payload '((message . "Not Found")))
            :to-equal '(:error t))
    (expect (magit-gha-badge--parse-payload nil) :to-equal '(:error t))))

(describe "magit-gha-badge--parse-repo"
  (it "returns the upstream slug of a fork"
    (expect (magit-gha-badge--parse-repo '((full_name . "agzam/cli")
                                           (parent . ((full_name . "cli/cli")))))
            :to-equal "cli/cli"))
  (it "returns nil for a repo that is nobody's fork"
    (expect (magit-gha-badge--parse-repo '((full_name . "cli/cli") (parent . nil)))
            :to-be nil)
    (expect (magit-gha-badge--parse-repo nil) :to-be nil)))

(describe "magit-gha-badge--combine"
  (it "keeps the newest run of the answers"
    (expect (plist-get (magit-gha-badge--combine
                        '((:created "2026-08-19T10:00:00Z" :url "old")
                          (:created "2026-08-19T12:00:00Z" :url "new")))
                       :url)
            :to-equal "new"))
  (it "prefers a run over an error or an empty answer"
    (expect (plist-get (magit-gha-badge--combine
                        '((:error t)
                          (:no-runs t)
                          (:created "2026-08-19T10:00:00Z" :url "u")))
                       :url)
            :to-equal "u"))
  (it "reports an error only when no answer held a run"
    (expect (magit-gha-badge--combine '((:error t) (:no-runs t)))
            :to-equal '(:error t)))
  (it "reports no runs when every answer was empty"
    (expect (magit-gha-badge--combine '((:no-runs t) (:no-runs t)))
            :to-equal '(:no-runs t))))

(describe "magit-gha-badge--glyph"
  (it "shows pending for queued-family statuses"
    (dolist (status '("queued" "waiting" "requested" "pending"))
      (expect (magit-gha-badge--glyph status nil) :to-equal "🟣")))
  (it "shows pending for statuses it does not know"
    (expect (magit-gha-badge--glyph "daydreaming" nil) :to-equal "🟣"))
  (it "shows in-progress"
    (expect (magit-gha-badge--glyph "in_progress" nil) :to-equal "🟡"))
  (it "shows success"
    (expect (magit-gha-badge--glyph "completed" "success") :to-equal "🟢"))
  (it "shows failure for the failure family"
    (dolist (conclusion '("failure" "timed_out" "startup_failure"))
      (expect (magit-gha-badge--glyph "completed" conclusion) :to-equal "🔴")))
  (it "shows neutral for the cancelled family and unknown conclusions"
    (dolist (conclusion '("cancelled" "skipped" "neutral" "stale" "whatever"))
      (expect (magit-gha-badge--glyph "completed" conclusion) :to-equal "⚪")))
  (it "shows action-required"
    (expect (magit-gha-badge--glyph "completed" "action_required") :to-equal "🟠")))

(describe "magit-gha-badge--waiting-p"
  (it "waits while the newest run is older than the pushed commit"
    (expect (magit-gha-badge--waiting-p '(:pushed "new" :sha "old")) :to-be-truthy)
    (expect (magit-gha-badge--waiting-p '(:pushed "new" :no-runs t)) :to-be-truthy))
  (it "stops once a run covers the pushed commit"
    (expect (magit-gha-badge--waiting-p '(:pushed "new" :sha "new")) :to-be nil))
  (it "stops when the branch was never pushed"
    (expect (magit-gha-badge--waiting-p '(:sha "old")) :to-be nil))
  (it "stops on a fetch error"
    (expect (magit-gha-badge--waiting-p '(:pushed "new" :sha "old" :error t)) :to-be nil))
  (it "gives up once the wait budget is spent"
    (expect (magit-gha-badge--waiting-p
             (list :pushed "new" :sha "old" :waits magit-gha-badge--wait-ticks))
            :to-be nil)))

(describe "magit-gha-badge--count-wait"
  (it "counts every fetch that answered with an older run"
    (expect (plist-get (magit-gha-badge--count-wait '(:pushed "new" :sha "old"))
                       :waits)
            :to-equal 1)
    (expect (plist-get (magit-gha-badge--count-wait '(:pushed "new" :sha "old" :waits 2))
                       :waits)
            :to-equal 3))
  (it "resets once a run covers the pushed commit"
    (expect (plist-get (magit-gha-badge--count-wait '(:pushed "new" :sha "new" :waits 2))
                       :waits)
            :to-equal 0))
  (it "resets when the branch was never pushed"
    (expect (plist-get (magit-gha-badge--count-wait '(:sha "old" :waits 2)) :waits)
            :to-equal 0))
  (it "does not mutate the input state"
    (let ((state (list :pushed "new" :sha "old")))
      (magit-gha-badge--count-wait state)
      (expect state :to-equal '(:pushed "new" :sha "old")))))

(describe "magit-gha-badge--pending-p"
  (it "re-arms while a fresh result is still running"
    (expect (magit-gha-badge--pending-p '(:status "in_progress")) :to-be-truthy)
    (expect (magit-gha-badge--pending-p '(:status "queued")) :to-be-truthy))
  (it "re-arms after a push whose run GitHub has not created yet"
    (expect (magit-gha-badge--pending-p
             '(:status "completed" :conclusion "success" :sha "old" :pushed "new"))
            :to-be-truthy))
  (it "does not re-arm on a completed result"
    (expect (magit-gha-badge--pending-p '(:status "completed" :conclusion "success"))
            :to-be nil))
  (it "does not re-arm once the completed run covers the pushed commit"
    (expect (magit-gha-badge--pending-p
             '(:status "completed" :conclusion "success" :sha "new" :pushed "new"))
            :to-be nil))
  (it "does not re-arm on a fetch error, even with a stale pending status"
    (expect (magit-gha-badge--pending-p '(:status "in_progress" :error t)) :to-be nil))
  (it "does not re-arm when the branch has no runs"
    (expect (magit-gha-badge--pending-p '(:no-runs t)) :to-be nil))
  (it "does not re-arm before any result arrived"
    (expect (magit-gha-badge--pending-p nil) :to-be nil)))

(describe "magit-gha-badge--merge-result"
  (it "replaces run fields and clears stale error/no-runs flags"
    (let ((merged (magit-gha-badge--merge-result
                   '(:status "in_progress" :error t :timer tm)
                   '(:status "completed" :conclusion "success" :url "u"))))
      (expect (plist-get merged :status) :to-equal "completed")
      (expect (plist-get merged :url) :to-equal "u")
      (expect (plist-get merged :error) :to-be nil)
      (expect (plist-get merged :no-runs) :to-be nil)))
  (it "preserves bookkeeping fields"
    (let ((merged (magit-gha-badge--merge-result
                   '(:timer tm :section sec :buffer buf :fetching now
                     :slug "agzam/cli" :parent ("agzam/cli" . "cli/cli")
                     :pushed "cafe" :waits 2)
                   '(:status "completed"))))
      (expect (plist-get merged :timer) :to-be 'tm)
      (expect (plist-get merged :section) :to-be 'sec)
      (expect (plist-get merged :buffer) :to-be 'buf)
      (expect (plist-get merged :fetching) :to-be 'now)
      (expect (plist-get merged :slug) :to-equal "agzam/cli")
      (expect (plist-get merged :parent) :to-equal '("agzam/cli" . "cli/cli"))
      (expect (plist-get merged :pushed) :to-equal "cafe")
      (expect (plist-get merged :waits) :to-equal 2)))
  (it "keeps the previous truth on a fetch error"
    (let ((merged (magit-gha-badge--merge-result
                   '(:status "completed" :conclusion "success" :url "old")
                   '(:error t))))
      (expect (plist-get merged :url) :to-equal "old")
      (expect (plist-get merged :status) :to-equal "completed")
      (expect (plist-get merged :error) :to-be t)))
  (it "clears run fields when the branch has no runs anymore"
    (let ((merged (magit-gha-badge--merge-result
                   '(:status "completed" :url "old" :repo "cli/cli"
                     :created "2026-08-19T10:00:00Z")
                   '(:no-runs t))))
      (expect (plist-get merged :status) :to-be nil)
      (expect (plist-get merged :url) :to-be nil)
      (expect (plist-get merged :repo) :to-be nil)
      (expect (plist-get merged :created) :to-be nil)
      (expect (plist-get merged :no-runs) :to-be t)))
  (it "does not mutate the input state"
    (let ((state (list :status "completed" :url "old")))
      (magit-gha-badge--merge-result state '(:error t))
      (magit-gha-badge--merge-result state '(:status "queued"))
      (expect state :to-equal '(:status "completed" :url "old")))))

(describe "magit-gha-badge--foreign-repo"
  (it "names the repo a run happened in when the branch pushes elsewhere"
    (expect (magit-gha-badge--foreign-repo '(:repo "cli/cli" :slug "agzam/cli"))
            :to-equal "cli/cli"))
  (it "stays quiet about the branch's own repo, whatever case it is spelled in"
    (expect (magit-gha-badge--foreign-repo '(:repo "cli/cli" :slug "cli/cli")) :to-be nil)
    (expect (magit-gha-badge--foreign-repo '(:repo "cli/cli" :slug "CLI/cli")) :to-be nil))
  (it "stays quiet before a fetch settled the slug"
    (expect (magit-gha-badge--foreign-repo '(:repo "cli/cli")) :to-be nil)))

(describe "magit-gha-badge--age"
  (it "counts in the largest unit that fits"
    (expect (magit-gha-badge--age (magit-gha-badge-tests--ago 300))
            :to-equal "5 minutes ago")
    (expect (magit-gha-badge--age (magit-gha-badge-tests--ago (* 3 24 60 60)))
            :to-equal "3 days ago")
    (expect (magit-gha-badge--age (magit-gha-badge-tests--ago (* 5 2629746)))
            :to-equal "5 months ago"))
  (it "keeps the unit singular for one of it"
    (expect (magit-gha-badge--age (magit-gha-badge-tests--ago 3600))
            :to-equal "1 hour ago"))
  (it "counts a run that just started in seconds"
    (expect (magit-gha-badge--age (magit-gha-badge-tests--ago 5))
            :to-match "\\`[0-9]+ seconds ago\\'"))
  (it "says nothing without a timestamp"
    (expect (magit-gha-badge--age nil) :to-be nil)))

(describe "magit-gha-badge--line"
  (it "renders a placeholder before any result arrives"
    (expect (magit-gha-badge--line nil) :to-equal "Checks   ⋯"))
  (it "renders glyph, workflow and 7-char sha"
    (expect (magit-gha-badge--line
             '(:status "completed" :conclusion "success" :workflow "CI"
               :sha "0ecbb23deadbeef"))
            :to-equal "Checks   🟢 CI 0ecbb23"))
  (it "survives a sha shorter than 7 chars"
    (expect (magit-gha-badge--line '(:status "in_progress" :workflow "CI" :sha "abc"))
            :to-equal "Checks   🟡 CI abc"))
  (it "names the upstream repo a fork's pull request ran in"
    (expect (magit-gha-badge--line
             '(:status "completed" :conclusion "failure" :workflow "Lint"
               :sha "0ecbb23" :repo "cli/cli" :slug "agzam/cli"))
            :to-equal "Checks   🔴 Lint 0ecbb23 (cli/cli)"))
  (it "marks a run that is older than the pushed commit"
    (expect (magit-gha-badge--line
             '(:status "completed" :conclusion "success" :workflow "CI"
               :sha "0ecbb23" :pushed "deadbee"))
            :to-equal "Checks   🟢 CI 0ecbb23 ⋯"))
  (it "tells how long ago the run started"
    (expect (magit-gha-badge--line
             (list :status "completed" :conclusion "success" :workflow "CI"
                   :sha "0ecbb23" :created (magit-gha-badge-tests--ago 300)))
            :to-equal "Checks   🟢 CI 0ecbb23 - 5 minutes ago"))
  (it "puts the age after the repo and before the wait marker"
    (expect (magit-gha-badge--line
             (list :status "completed" :conclusion "failure" :workflow "Lint"
                   :sha "0ecbb23" :repo "cli/cli" :slug "agzam/cli"
                   :pushed "deadbee" :created (magit-gha-badge-tests--ago 3600)))
            :to-equal "Checks   🔴 Lint 0ecbb23 (cli/cli) - 1 hour ago ⋯"))
  (it "renders a placeholder while a pushed commit has no run at all"
    (expect (magit-gha-badge--line '(:no-runs t :pushed "deadbee"))
            :to-equal "Checks   ⋯"))
  (it "carries the run title as help-echo"
    (let ((line (magit-gha-badge--line '(:status "completed" :conclusion "success"
                                         :workflow "CI" :sha "0ecbb23"
                                         :title "tidy up"))))
      (expect (get-text-property (1- (length line)) 'help-echo line)
              :to-equal "tidy up")))
  (it "renders nothing for a branch with no runs"
    (expect (magit-gha-badge--line '(:no-runs t)) :to-be nil))
  (it "renders nothing for an error with no previous truth"
    (expect (magit-gha-badge--line '(:error t)) :to-be nil))
  (it "keeps showing the previous truth on an error"
    (expect (magit-gha-badge--line
             '(:status "completed" :conclusion "failure" :workflow "CI"
               :sha "0ecbb23" :error t))
            :to-equal "Checks   🔴 CI 0ecbb23")))

(describe "magit-gha-badge--browse-target"
  (it "opens the fetched run without a word"
    (let ((target (magit-gha-badge--browse-target
                   '(:status "completed" :url "u" :sha "new" :pushed "new"))))
      (expect (car target) :to-equal "u")
      (expect (cdr target) :to-be nil)))
  (it "says so when the run it opens predates the pushed commit"
    (let ((target (magit-gha-badge--browse-target '(:url "u" :sha "old" :pushed "new"))))
      (expect (car target) :to-equal "u")
      (expect (cdr target) :to-match "pushed commit")))
  (it "falls back to the last known run when the fetch failed"
    (let ((target (magit-gha-badge--browse-target '(:url "u" :error t))))
      (expect (car target) :to-equal "u")
      (expect (cdr target) :to-match "failed")))
  (it "reports a failure with nothing to fall back on"
    (let ((target (magit-gha-badge--browse-target '(:error t))))
      (expect (car target) :to-be nil)
      (expect (cdr target) :to-match "failed")))
  (it "reports a branch with no runs"
    (let ((target (magit-gha-badge--browse-target '(:no-runs t))))
      (expect (car target) :to-be nil)
      (expect (cdr target) :to-match "No workflow runs"))))

(describe "magit-gha-badge--request-runs"
  (before-each
    (setq magit-gha-badge-tests--calls nil
          magit-gha-badge-tests--answers nil)
    (spy-on 'ghub-request :and-call-fake #'magit-gha-badge-tests--ghub))

  (it "asks the branch's own repo only, when it is nobody's fork"
    (let ((magit-gha-badge-tests--answers
           (list (cons "/repos/agzam/.emacs.d/actions/runs"
                       magit-gha-badge-tests--payload)))
          result)
      (magit-gha-badge--request-runs "main" "agzam/.emacs.d" nil
                                     (lambda (r) (setq result r)))
      (expect (length magit-gha-badge-tests--calls) :to-equal 1)
      (expect (plist-get result :workflow) :to-equal "CI")))

  (it "finds a fork's pull request run in the upstream repo"
    (let ((magit-gha-badge-tests--answers
           (list (cons "/repos/agzam/cli/actions/runs"
                       '((total_count . 0) (workflow_runs . ())))
                 (cons "/repos/cli/cli/actions/runs"
                       magit-gha-badge-tests--upstream-payload)))
          (calls 0)
          result)
      (magit-gha-badge--request-runs "fix" "agzam/cli" "cli/cli"
                                     (lambda (r) (setq calls (1+ calls) result r)))
      (expect calls :to-equal 1)
      (expect (plist-get result :url)
              :to-equal "https://github.com/cli/cli/actions/runs/1000")
      (expect (plist-get result :repo) :to-equal "cli/cli")))

  (it "asks the upstream repo for a page wide enough to look past its own runs"
    (let ((magit-gha-badge-tests--answers
           (list (cons "/repos/cli/cli/actions/runs"
                       magit-gha-badge-tests--upstream-payload))))
      (magit-gha-badge--request-runs "fix" "agzam/cli" "cli/cli" #'ignore)
      (expect (alist-get 'branch (magit-gha-badge-tests--params
                                  "/repos/cli/cli/actions/runs"))
              :to-equal "fix")
      (expect (alist-get 'per_page (magit-gha-badge-tests--params
                                    "/repos/agzam/cli/actions/runs"))
              :to-equal "1")
      (expect (alist-get 'per_page (magit-gha-badge-tests--params
                                    "/repos/cli/cli/actions/runs"))
              :to-equal "10")))

  (it "keeps the fork's own run when the upstream request fails"
    (let ((magit-gha-badge-tests--answers
           (list (cons "/repos/agzam/cli/actions/runs" magit-gha-badge-tests--payload)))
          result)
      (magit-gha-badge--request-runs "fix" "agzam/cli" "cli/cli"
                                     (lambda (r) (setq result r)))
      (expect (plist-get result :workflow) :to-equal "CI")
      (expect (plist-get result :error) :to-be nil)))

  (it "reports an error when neither repo answers"
    (let (result)
      (magit-gha-badge--request-runs "fix" "agzam/cli" "cli/cli"
                                     (lambda (r) (setq result r)))
      (expect result :to-equal '(:error t)))))

(describe "magit-gha-badge--parent-slug"
  (before-each
    (setq magit-gha-badge-tests--calls nil
          magit-gha-badge-tests--answers nil)
    (spy-on 'ghub-request :and-call-fake #'magit-gha-badge-tests--ghub))

  (it "looks the upstream slug up once and answers from cache after that"
    (let ((magit-gha-badge-tests--answers
           (list (cons "/repos/agzam/cli"
                       '((full_name . "agzam/cli")
                         (parent . ((full_name . "cli/cli")))))))
          (repo "/tmp/magit-gha-badge-test-repo/")
          (parents nil))
      (unwind-protect
          (progn
            (magit-gha-badge--parent-slug repo "agzam/cli"
                                          (lambda (p) (push p parents)))
            (magit-gha-badge--parent-slug repo "agzam/cli"
                                          (lambda (p) (push p parents)))
            (expect parents :to-equal '("cli/cli" "cli/cli"))
            (expect (length magit-gha-badge-tests--calls) :to-equal 1))
        (remhash repo magit-gha-badge--cache))))

  (it "caches that a repo is nobody's fork"
    (let ((magit-gha-badge-tests--answers
           (list (cons "/repos/agzam/.emacs.d" '((full_name . "agzam/.emacs.d")))))
          (repo "/tmp/magit-gha-badge-test-repo/")
          (parents nil))
      (unwind-protect
          (progn
            (magit-gha-badge--parent-slug repo "agzam/.emacs.d"
                                          (lambda (p) (push p parents)))
            (magit-gha-badge--parent-slug repo "agzam/.emacs.d"
                                          (lambda (p) (push p parents)))
            (expect parents :to-equal '(nil nil))
            (expect (length magit-gha-badge-tests--calls) :to-equal 1))
        (remhash repo magit-gha-badge--cache))))

  (it "does not cache a failed lookup"
    (let ((repo "/tmp/magit-gha-badge-test-repo/")
          (parents nil))
      (unwind-protect
          (progn
            (magit-gha-badge--parent-slug repo "agzam/cli"
                                          (lambda (p) (push p parents)))
            (magit-gha-badge--parent-slug repo "agzam/cli"
                                          (lambda (p) (push p parents)))
            (expect parents :to-equal '(nil nil))
            (expect (length magit-gha-badge-tests--calls) :to-equal 2))
        (remhash repo magit-gha-badge--cache)))))

(describe "magit-gha-badge--set"
  (it "updates the cached state per repo"
    (let ((repo "/tmp/magit-gha-badge-test-repo/"))
      (unwind-protect
          (progn
            (magit-gha-badge--set repo :status "queued")
            (magit-gha-badge--set repo :timer 'tm :status "completed")
            (expect (plist-get (magit-gha-badge--state repo) :status)
                    :to-equal "completed")
            (expect (plist-get (magit-gha-badge--state repo) :timer) :to-be 'tm))
        (remhash repo magit-gha-badge--cache)))))

;;; magit-gha-badge-tests.el ends here
