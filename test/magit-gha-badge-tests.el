;;; test/magit-gha-badge-tests.el --- magit-gha-badge specs -*- lexical-binding: t; -*-

;;; Commentary:
;; Parsing, glyph mapping, merge/re-arm decisions, line rendering, cache
;; updates, the requests against a faked `ghub-request', and the in-place
;; swap in a buffer of real magit sections.  No network; the poll timer
;; and the status buffer refresh are verified live.

;;; Code:

(require 'buttercup)
(require 'magit-gha-badge)

;; shape of GET /repos/OWNER/REPO/actions/runs as ghub parses it
;; (alist keys, arrays as lists, json null as nil)
(defvar magit-gha-badge-tests--payload
  '((total_count . 1)
    (workflow_runs
     . (((id . 123)
         (run_attempt . 2)
         (status . "completed")
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

;; shape of GET /repos/OWNER/REPO/actions/runs/ID/jobs: a job running, one
;; failed, one passed, and one whose status lags its finished steps
(defvar magit-gha-badge-tests--jobs-payload
  '((total_count . 4)
    (jobs
     . (((name . "Testing (31.1)")
         (status . "in_progress")
         (conclusion . nil)
         (html_url . "https://github.com/agzam/.emacs.d/actions/runs/123/job/1")
         (steps . (((name . "Set up job") (status . "completed") (conclusion . "success"))
                   ((name . "Run tests") (status . "in_progress") (conclusion . nil))
                   ((name . "Complete job") (status . "pending") (conclusion . nil)))))
        ((name . "Testing (29.4)")
         (status . "completed")
         (conclusion . "failure")
         (html_url . "https://github.com/agzam/.emacs.d/actions/runs/123/job/2")
         (steps . (((name . "Set up job") (status . "completed") (conclusion . "success"))
                   ((name . "Run tests") (status . "completed") (conclusion . "failure"))
                   ((name . "Check byte-compilation") (status . "completed")
                    (conclusion . "skipped"))
                   ((name . "Complete job") (status . "completed") (conclusion . "success")))))
        ((name . "Lint")
         (status . "completed")
         (conclusion . "success")
         (html_url . "https://github.com/agzam/.emacs.d/actions/runs/123/job/3")
         (steps . (((name . "Set up job") (status . "completed") (conclusion . "success"))
                   ((name . "Complete job") (status . "completed") (conclusion . "success")))))
        ((name . "Docs")
         (status . "in_progress")
         (conclusion . nil)
         (html_url . "https://github.com/agzam/.emacs.d/actions/runs/123/job/4")
         (steps . (((name . "Set up job") (status . "completed") (conclusion . "success"))
                   ((name . "Complete job") (status . "completed")
                    (conclusion . "success")))))))))

(defconst magit-gha-badge-tests--jobs-resource "/repos/agzam/.emacs.d/actions/runs/123/jobs")

(defconst magit-gha-badge-tests--running
  '(:id 123 :attempt 1 :status "in_progress" :repo "agzam/.emacs.d" :workflow "CI")
  "A fetch result for a run still going.")

(defconst magit-gha-badge-tests--failed
  '(:id 123 :attempt 1 :status "completed" :conclusion "failure" :repo "agzam/.emacs.d"
    :workflow "CI")
  "A fetch result for a run that failed.")

(defvar magit-gha-badge-tests--answers nil
  "Alist of API resource to the payload the `ghub-request' stub replies with.
A resource absent from it answers through the errorback.")

(defvar magit-gha-badge-tests--calls nil
  "Alist of API resource to params, in reverse order of request.")

;; what ghub hands an errorback for an HTTP error: url.el's error with
;; the parsed error document appended
(defconst magit-gha-badge-tests--not-found
  '(error http 404 ((message . "Not Found")
                    (documentation_url . "https://docs.github.com/rest")
                    (status . "404"))))

(defconst magit-gha-badge-tests--no-token
  (concat "Required Github token (\"agzam^ghub\" for \"api.github.com\") does not exist.\n"
          "See https://docs.magit.vc/ghub/Getting-Started.html")
  "What `ghub-request' signals, before any request, when the token is missing.")

(defun magit-gha-badge-tests--ghub (_method resource &optional params &rest args)
  "Answer a request for RESOURCE with PARAMS from the canned answers.
ARGS carries the `ghub-request' keywords, of which the callbacks are used."
  (push (cons resource params) magit-gha-badge-tests--calls)
  (if-let* ((answer (assoc resource magit-gha-badge-tests--answers)))
      (funcall (plist-get args :callback) (cdr answer))
    (funcall (plist-get args :errorback) magit-gha-badge-tests--not-found)))

(defun magit-gha-badge-tests--ghub-signal (_method resource &rest _)
  "Record a request for RESOURCE, then signal like ghub without a token."
  (push (cons resource nil) magit-gha-badge-tests--calls)
  (error "%s" magit-gha-badge-tests--no-token))

(defvar magit-gha-badge-tests--timers nil
  "Functions with their args the `run-at-time' stub queued, newest first.")

(defun magit-gha-badge-tests--queue-timer (_time _repeat function &rest args)
  "Queue FUNCTION with ARGS instead of arming a timer."
  (push (cons function args) magit-gha-badge-tests--timers))

(defun magit-gha-badge-tests--run-timers ()
  "Run the queued timers oldest first, including the ones they queue."
  (when-let* ((timers (reverse magit-gha-badge-tests--timers)))
    (setq magit-gha-badge-tests--timers nil)
    (dolist (timer timers)
      (apply (car timer) (cdr timer)))
    (magit-gha-badge-tests--run-timers)))

(defun magit-gha-badge-tests--params (resource)
  "Return the params RESOURCE was requested with."
  (cdr (assoc resource magit-gha-badge-tests--calls)))

(defun magit-gha-badge-tests--ago (seconds)
  "Return an API-shaped timestamp SECONDS before now.
Now is truncated to the whole second, so the age an expectation sees is
SECONDS plus a sub-second remainder: only offsets of a minute and up
round to a stable count."
  (format-time-string "%FT%TZ" (- (time-convert nil 'integer) seconds) t))

(defun magit-gha-badge-tests--job (name status &optional conclusion step)
  "Return a parsed job NAME in STATUS, with CONCLUSION, at STEP."
  (list :name name :status status :conclusion conclusion :step step
        :url (concat "https://github.com/agzam/.emacs.d/actions/runs/123/job/" name)))

(defun magit-gha-badge-tests--fetch-jobs (repo result)
  "Run RESULT through the jobs request for REPO, as a fetch does.
Merge what it hands on into REPO's state, and return that."
  (let (handed)
    (magit-gha-badge--request-jobs repo result (lambda (r) (setq handed r)))
    (puthash repo (magit-gha-badge--merge-result (magit-gha-badge--state repo) handed)
             magit-gha-badge--cache)
    handed))

(defun magit-gha-badge-tests--count (resource)
  "Return how often RESOURCE was requested."
  (seq-count (lambda (call) (equal (car call) resource)) magit-gha-badge-tests--calls))

(defun magit-gha-badge-tests--child (type)
  "Return the top-level section of TYPE in the current buffer."
  (seq-find (lambda (section) (eq (oref section type) type))
            (oref magit-root-section children)))

(defun magit-gha-badge-tests--render (state &optional first)
  "Insert STATE's badge section between two others in a new buffer.
FIRST leaves out the one before, as magit does when no header has
anything to show.  Return STATE with the section and the buffer, as the
inserter records them."
  (let ((buffer (generate-new-buffer " *magit-gha-badge-tests*")))
    (with-current-buffer buffer
      (magit-section-mode)
      (let ((inhibit-read-only t))
        (magit-insert-section (root)
          (unless first
            (magit-insert-section (head)
              (insert "Head:     main Fix parser crash\n"))
            (insert "\n"))
          (magit-insert-section (gha-badge (plist-get state :url))
            (magit-insert-heading (magit-gha-badge--line state))
            (insert (magit-gha-badge--body state)))
          (magit-insert-section (untracked)
            (magit-insert-heading "Untracked files")
            (insert "notes.txt\n"))))
      (append (list :section (magit-gha-badge-tests--child 'gha-badge) :buffer buffer)
              state))))

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
      (expect (plist-get result :id) :to-equal 123)
      (expect (plist-get result :attempt) :to-equal 2)
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
  (it "treats a payload without workflow_runs as an error, with its message"
    (expect (magit-gha-badge--parse-payload '((message . "Not Found")))
            :to-equal '(:error "Not Found"))
    (expect (magit-gha-badge--parse-payload nil)
            :to-equal '(:error "Unexpected response from GitHub"))))

(describe "magit-gha-badge--parse-jobs"
  (it "names the step a running job is in"
    (expect (nth 0 (magit-gha-badge--parse-jobs magit-gha-badge-tests--jobs-payload))
            :to-equal '(:name "Testing (31.1)" :status "in_progress" :conclusion nil
                        :step "Run tests"
                        :url "https://github.com/agzam/.emacs.d/actions/runs/123/job/1")))
  (it "names the step a failed job failed at"
    (let ((job (nth 1 (magit-gha-badge--parse-jobs magit-gha-badge-tests--jobs-payload))))
      (expect (plist-get job :conclusion) :to-equal "failure")
      (expect (plist-get job :step) :to-equal "Run tests")))
  (it "names no step for a job that passed"
    (expect (plist-get (nth 2 (magit-gha-badge--parse-jobs
                               magit-gha-badge-tests--jobs-payload))
                       :step)
            :to-be nil))
  (it "names no step for a running job whose steps have all finished"
    (let ((job (nth 3 (magit-gha-badge--parse-jobs magit-gha-badge-tests--jobs-payload))))
      (expect (plist-get job :status) :to-equal "in_progress")
      (expect (plist-get job :step) :to-be nil))))

(describe "magit-gha-badge--error-message"
  (it "turns an HTTP error into its code and GitHub's message, docs on line 2"
    (expect (magit-gha-badge--error-message magit-gha-badge-tests--not-found)
            :to-equal "404 Not Found\nhttps://docs.github.com/rest"))
  (it "names the repo an HTTP error came from when given one"
    (expect (magit-gha-badge--error-message magit-gha-badge-tests--not-found "cli/cli")
            :to-equal "404 Not Found (cli/cli)\nhttps://docs.github.com/rest"))
  (it "survives an HTTP error with an empty body"
    (expect (magit-gha-badge--error-message '(error http 502 nil))
            :to-equal "502 HTTP error"))
  (it "keeps the text of a signaled error, every line of it"
    (expect (magit-gha-badge--error-message (list 'error magit-gha-badge-tests--no-token))
            :to-equal magit-gha-badge-tests--no-token)))

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
                        '((:error "404 Not Found")
                          (:no-runs t)
                          (:created "2026-08-19T10:00:00Z" :url "u")))
                       :url)
            :to-equal "u"))
  (it "reports an error only when no answer held a run"
    (expect (magit-gha-badge--combine '((:no-runs t) (:error "404 Not Found")))
            :to-equal '(:error "404 Not Found")))
  (it "keeps the first error when every answer failed"
    (expect (magit-gha-badge--combine '((:error "Fork failed") (:error "Upstream failed")))
            :to-equal '(:error "Fork failed")))
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
    (expect (magit-gha-badge--waiting-p '(:pushed "new" :sha "old" :error "502 HTTP error"))
            :to-be nil))
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
    (expect (magit-gha-badge--pending-p '(:status "in_progress" :error "502 HTTP error"))
            :to-be nil))
  (it "does not re-arm when the branch has no runs"
    (expect (magit-gha-badge--pending-p '(:no-runs t)) :to-be nil))
  (it "does not re-arm before any result arrived"
    (expect (magit-gha-badge--pending-p nil) :to-be nil)))

(describe "magit-gha-badge--merge-result"
  (it "replaces run fields and clears stale error/no-runs flags"
    (let ((merged (magit-gha-badge--merge-result
                   '(:status "in_progress" :error "401 Bad credentials" :timer tm)
                   '(:status "completed" :conclusion "success" :url "u"))))
      (expect (plist-get merged :status) :to-equal "completed")
      (expect (plist-get merged :url) :to-equal "u")
      (expect (plist-get merged :error) :to-be nil)
      (expect (plist-get merged :no-runs) :to-be nil)))
  (it "drops the jobs of the previous run with it"
    (let ((merged (magit-gha-badge--merge-result
                   (list :status "completed" :conclusion "failure" :id 123 :attempt 1
                         :jobs (list (magit-gha-badge-tests--job "Test" "completed" "failure"))
                         :jobs-run '(123 . 1))
                   '(:status "completed" :conclusion "success" :id 124 :attempt 1))))
      (expect (plist-get merged :jobs) :to-be nil)
      (expect (plist-get merged :jobs-run) :to-be nil)))
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
  (it "keeps the previous truth on a fetch error, and the error's text"
    (let ((merged (magit-gha-badge--merge-result
                   '(:status "completed" :conclusion "success" :url "old")
                   '(:error "401 Bad credentials"))))
      (expect (plist-get merged :url) :to-equal "old")
      (expect (plist-get merged :status) :to-equal "completed")
      (expect (plist-get merged :error) :to-equal "401 Bad credentials")))
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
      (magit-gha-badge--merge-result state '(:error "401 Bad credentials"))
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
  (it "shows the error's summary when there is no previous truth"
    (expect (magit-gha-badge--line
             '(:error "401 Bad credentials\nhttps://docs.github.com/rest"))
            :to-equal "Checks   ❗ 401 Bad credentials"))
  (it "keeps showing the previous truth on an error, the summary after it"
    (expect (magit-gha-badge--line
             '(:status "completed" :conclusion "failure" :workflow "CI"
               :sha "0ecbb23" :error "401 Bad credentials\nhttps://docs.github.com/rest"))
            :to-equal "Checks   🔴 CI 0ecbb23 ❗ 401 Bad credentials"))
  (it "carries the run title and the full error as help-echo"
    (let ((line (magit-gha-badge--line
                 '(:status "completed" :conclusion "success" :workflow "CI"
                   :sha "0ecbb23" :title "tidy up"
                   :error "401 Bad credentials\nhttps://docs.github.com/rest"))))
      (expect (get-text-property (1- (length line)) 'help-echo line)
              :to-equal "tidy up\n401 Bad credentials\nhttps://docs.github.com/rest")))
  (it "renders an error cached as t, as a cache from an in-place upgrade holds it"
    (expect (magit-gha-badge--line '(:error t))
            :to-equal "Checks   ❗ Fetching workflow runs failed")))

(describe "magit-gha-badge--job-lines"
  (it "lists failed jobs, then running ones, each in GitHub's order, under the glyph"
    (expect (magit-gha-badge--job-lines
             (list (magit-gha-badge-tests--job "a" "in_progress" nil "Build")
                   (magit-gha-badge-tests--job "b" "completed" "failure" "Test")
                   (magit-gha-badge-tests--job "c" "in_progress" nil "Lint")
                   (magit-gha-badge-tests--job "d" "completed" "timed_out" "Test")))
            :to-equal '("         🔴 b - Test"
                        "         🔴 d - Test"
                        "         🟡 a - Build"
                        "         🟡 c - Lint")))
  (it "hides jobs that neither run nor failed"
    (expect (magit-gha-badge--job-lines
             (list (magit-gha-badge-tests--job "passed" "completed" "success")
                   (magit-gha-badge-tests--job "stopped" "completed" "cancelled")
                   (magit-gha-badge-tests--job "skipped" "completed" "skipped")
                   (magit-gha-badge-tests--job "next" "queued")
                   (magit-gha-badge-tests--job "gated" "waiting")))
            :to-be nil))
  (it "pads job names to the longest one shown, and leaves a job without a step bare"
    (expect (magit-gha-badge--job-lines
             (list (magit-gha-badge-tests--job "Testing (29.4)" "in_progress" nil "Run tests")
                   (magit-gha-badge-tests--job "Lint" "in_progress" nil "Check")
                   (magit-gha-badge-tests--job "Docs" "in_progress")))
            :to-equal '("         🟡 Testing (29.4) - Run tests"
                        "         🟡 Lint           - Check"
                        "         🟡 Docs")))
  (it "lines the steps up by display width when a name holds an emoji"
    (expect (seq-uniq
             (mapcar (lambda (line)
                       (string-width (substring line 0 (string-search " - " line))))
                     (magit-gha-badge--job-lines
                      (list (magit-gha-badge-tests--job "🧪 Tests" "in_progress" nil "Run")
                            (magit-gha-badge-tests--job "Lint" "in_progress" nil "Check")))))
            :to-equal (list (string-width "         🟡 🧪 Tests"))))
  (it "shows five jobs, and past five shows four and counts the rest"
    (let ((jobs (mapcar (lambda (n)
                          (magit-gha-badge-tests--job (format "job %s" n) "in_progress"
                                                      nil "Run"))
                        (number-sequence 1 7))))
      (expect (length (magit-gha-badge--job-lines (seq-take jobs 5))) :to-equal 5)
      (expect (magit-gha-badge--job-lines jobs)
              :to-equal '("         🟡 job 1 - Run"
                          "         🟡 job 2 - Run"
                          "         🟡 job 3 - Run"
                          "         🟡 job 4 - Run"
                          "         +3 more"))))
  (it "carries the job's page on its line, and none on the count"
    (let ((lines (magit-gha-badge--job-lines
                  (mapcar (lambda (n)
                            (magit-gha-badge-tests--job (format "job%s" n) "in_progress"))
                          (number-sequence 1 6)))))
      (expect (get-text-property 3 'magit-gha-badge-url (car lines))
              :to-equal "https://github.com/agzam/.emacs.d/actions/runs/123/job/job1")
      (expect (get-text-property 3 'magit-gha-badge-url (car (last lines))) :to-be nil))))

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
  (it "falls back to the last known run when the fetch failed, and says why"
    (let ((target (magit-gha-badge--browse-target
                   '(:url "u" :error "401 Bad credentials"))))
      (expect (car target) :to-equal "u")
      (expect (cdr target)
              :to-equal (concat "Fetching workflow runs failed, opening the last"
                                " known run: 401 Bad credentials"))))
  (it "reports a failure with nothing to fall back on, and says why"
    (let ((target (magit-gha-badge--browse-target '(:error "401 Bad credentials"))))
      (expect (car target) :to-be nil)
      (expect (cdr target)
              :to-equal "Fetching workflow runs failed: 401 Bad credentials")))
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

  (it "reports the fork's own error when neither repo answers"
    (let (result)
      (magit-gha-badge--request-runs "fix" "agzam/cli" "cli/cli"
                                     (lambda (r) (setq result r)))
      (expect result
              :to-equal '(:error "404 Not Found\nhttps://docs.github.com/rest"))))

  (it "names the upstream when only its request failed"
    (let ((magit-gha-badge-tests--answers
           (list (cons "/repos/agzam/cli/actions/runs"
                       '((total_count . 0) (workflow_runs . ())))))
          result)
      (magit-gha-badge--request-runs "fix" "agzam/cli" "cli/cli"
                                     (lambda (r) (setq result r)))
      (expect (plist-get result :error)
              :to-equal "404 Not Found (cli/cli)\nhttps://docs.github.com/rest")))

  (it "reports the fork's own error whichever answer lands first"
    (let ((errorbacks nil)
          result)
      (spy-on 'ghub-request
              :and-call-fake (lambda (_method _resource &optional _params &rest args)
                               (push (plist-get args :errorback) errorbacks)))
      (magit-gha-badge--request-runs "fix" "agzam/cli" "cli/cli"
                                     (lambda (r) (setq result r)))
      ;; newest first: the upstream answers before the fork
      (dolist (errorback errorbacks)
        (funcall errorback magit-gha-badge-tests--not-found))
      (expect (plist-get result :error)
              :to-equal "404 Not Found\nhttps://docs.github.com/rest"))))

(describe "magit-gha-badge--request-jobs"
  :var (repo)
  (before-each
    (setq repo "/tmp/magit-gha-badge-test-repo/"
          magit-gha-badge-tests--calls nil
          magit-gha-badge-tests--answers
          (list (cons magit-gha-badge-tests--jobs-resource
                      magit-gha-badge-tests--jobs-payload)))
    (spy-on 'ghub-request :and-call-fake #'magit-gha-badge-tests--ghub))
  (after-each
    (remhash repo magit-gha-badge--cache))

  (it "asks for the jobs of the latest attempt on every fetch while the run is going"
    (let ((result (magit-gha-badge-tests--fetch-jobs repo magit-gha-badge-tests--running)))
      (magit-gha-badge-tests--fetch-jobs repo magit-gha-badge-tests--running)
      (expect (magit-gha-badge-tests--count magit-gha-badge-tests--jobs-resource)
              :to-equal 2)
      (expect (magit-gha-badge-tests--params magit-gha-badge-tests--jobs-resource)
              :to-equal '((filter . "latest") (per_page . "100")))
      (expect (plist-get result :jobs)
              :to-equal (magit-gha-badge--parse-jobs magit-gha-badge-tests--jobs-payload))))

  (it "asks for the jobs of a failed run once per attempt"
    (magit-gha-badge-tests--fetch-jobs repo magit-gha-badge-tests--failed)
    (let ((result (magit-gha-badge-tests--fetch-jobs repo magit-gha-badge-tests--failed)))
      (expect (magit-gha-badge-tests--count magit-gha-badge-tests--jobs-resource)
              :to-equal 1)
      (expect (length (plist-get result :jobs)) :to-equal 4))
    (magit-gha-badge-tests--fetch-jobs repo (append '(:attempt 2)
                                                    magit-gha-badge-tests--failed))
    (expect (magit-gha-badge-tests--count magit-gha-badge-tests--jobs-resource)
            :to-equal 2))

  (it "asks again once a run it watched going has failed"
    (magit-gha-badge-tests--fetch-jobs repo magit-gha-badge-tests--running)
    (magit-gha-badge-tests--fetch-jobs repo magit-gha-badge-tests--failed)
    (expect (magit-gha-badge-tests--count magit-gha-badge-tests--jobs-resource)
            :to-equal 2))

  (it "never asks for the jobs of a run that passed"
    (let ((result (magit-gha-badge-tests--fetch-jobs
                   repo '(:id 123 :attempt 1 :status "completed" :conclusion "success"
                          :repo "agzam/.emacs.d"))))
      (expect magit-gha-badge-tests--calls :to-be nil)
      (expect (plist-get result :jobs) :to-be nil)))

  (it "asks the repo the run happened in, the upstream for a fork"
    (magit-gha-badge-tests--fetch-jobs repo (append '(:repo "cli/cli")
                                                    magit-gha-badge-tests--running))
    (expect (caar magit-gha-badge-tests--calls)
            :to-equal "/repos/cli/cli/actions/runs/123/jobs"))

  (it "hands the run on without jobs or an error when the request fails, then asks again"
    (let ((magit-gha-badge-tests--answers nil))
      (let ((result (magit-gha-badge-tests--fetch-jobs repo magit-gha-badge-tests--failed)))
        (expect (plist-get result :workflow) :to-equal "CI")
        (expect (plist-get result :jobs) :to-be nil)
        (expect (plist-get result :error) :to-be nil))
      (magit-gha-badge-tests--fetch-jobs repo magit-gha-badge-tests--failed)
      (expect (length magit-gha-badge-tests--calls) :to-equal 2))))

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
                                          (lambda (p) (push p parents)) #'ignore)
            (magit-gha-badge--parent-slug repo "agzam/cli"
                                          (lambda (p) (push p parents)) #'ignore)
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
                                          (lambda (p) (push p parents)) #'ignore)
            (magit-gha-badge--parent-slug repo "agzam/.emacs.d"
                                          (lambda (p) (push p parents)) #'ignore)
            (expect parents :to-equal '(nil nil))
            (expect (length magit-gha-badge-tests--calls) :to-equal 1))
        (remhash repo magit-gha-badge--cache))))

  (it "hands a failed lookup's error to the errorback, and does not cache it"
    (let ((repo "/tmp/magit-gha-badge-test-repo/")
          (parents nil)
          (failures nil))
      (unwind-protect
          (progn
            (magit-gha-badge--parent-slug repo "agzam/cli"
                                          (lambda (p) (push p parents))
                                          (lambda (e) (push e failures)))
            (magit-gha-badge--parent-slug repo "agzam/cli"
                                          (lambda (p) (push p parents))
                                          (lambda (e) (push e failures)))
            (expect parents :to-be nil)
            (expect failures
                    :to-equal (make-list 2 "404 Not Found\nhttps://docs.github.com/rest"))
            (expect (length magit-gha-badge-tests--calls) :to-equal 2))
        (remhash repo magit-gha-badge--cache)))))

(describe "a signal from ghub-request"
  (before-each
    (setq magit-gha-badge-tests--calls nil
          magit-gha-badge-tests--timers nil)
    (spy-on 'ghub-request :and-call-fake #'magit-gha-badge-tests--ghub-signal)
    (spy-on 'run-at-time :and-call-fake #'magit-gha-badge-tests--queue-timer))

  (it "reaches the callback of a runs request after the caller returned"
    (let (result)
      (magit-gha-badge--request-runs "fix" "agzam/cli" "cli/cli"
                                     (lambda (r) (setq result r)))
      (expect (length magit-gha-badge-tests--calls) :to-equal 2)
      (expect result :to-be nil)
      (magit-gha-badge-tests--run-timers)
      (expect result :to-equal (list :error magit-gha-badge-tests--no-token))))

  (it "never escapes a fetch, which runs inside the magit refresh"
    (spy-on 'message)
    (spy-on 'magit-get-current-branch :and-return-value "fix")
    (spy-on 'magit-gha-badge--remote-url
            :and-return-value "git@github.com:agzam/cli.git")
    (spy-on 'magit-gha-badge--pushed-sha :and-return-value nil)
    (let ((repo "/tmp/magit-gha-badge-test-repo/"))
      (unwind-protect
          (progn
            (expect (magit-gha-badge--fetch repo) :not :to-throw)
            (expect (plist-get (magit-gha-badge--state repo) :fetching)
                    :to-be-truthy)
            (magit-gha-badge-tests--run-timers)
            (expect (plist-get (magit-gha-badge--state repo) :fetching) :to-be nil)
            (expect (plist-get (magit-gha-badge--state repo) :error)
                    :to-equal magit-gha-badge-tests--no-token)
            ;; the failed parent lookup ends the fetch
            (expect (length magit-gha-badge-tests--calls) :to-equal 1)
            (expect (spy-calls-all-args 'message)
                    :to-equal (list (list "magit-gha-badge: %s"
                                          magit-gha-badge-tests--no-token))))
        (remhash repo magit-gha-badge--cache)))))

(describe "magit-gha-badge--on-fetch"
  (before-each
    (spy-on 'message))

  (it "echoes an error once, and again only when it changes"
    (let ((repo "/tmp/magit-gha-badge-test-repo/"))
      (unwind-protect
          (progn
            (magit-gha-badge--on-fetch repo '(:error "401 Bad credentials"))
            (magit-gha-badge--on-fetch repo '(:error "401 Bad credentials"))
            (expect (spy-calls-all-args 'message)
                    :to-equal '(("magit-gha-badge: %s" "401 Bad credentials")))
            (magit-gha-badge--on-fetch repo '(:error "404 Not Found"))
            (expect (spy-calls-count 'message) :to-equal 2))
        (remhash repo magit-gha-badge--cache))))

  (it "echoes a recurring error again once a fetch succeeded in between"
    (let ((repo "/tmp/magit-gha-badge-test-repo/"))
      (unwind-protect
          (progn
            (magit-gha-badge--on-fetch repo '(:error "401 Bad credentials"))
            (magit-gha-badge--on-fetch repo '(:no-runs t))
            (magit-gha-badge--on-fetch repo '(:error "401 Bad credentials"))
            (expect (spy-calls-count 'message) :to-equal 2))
        (remhash repo magit-gha-badge--cache))))

  (it "leaves the report to a callback"
    (let ((repo "/tmp/magit-gha-badge-test-repo/")
          (states nil))
      (unwind-protect
          (progn
            (magit-gha-badge--on-fetch repo '(:error "401 Bad credentials")
                                       (lambda (state) (push state states)))
            (expect (spy-calls-count 'message) :to-equal 0)
            (expect (plist-get (car states) :error) :to-equal "401 Bad credentials"))
        (remhash repo magit-gha-badge--cache))))

  (it "removes the badge, newline and all, when the answer leaves nothing to show"
    (let* ((repo "/tmp/magit-gha-badge-test-repo/")
           (state (magit-gha-badge-tests--render nil)))
      (unwind-protect
          (progn
            (puthash repo state magit-gha-badge--cache)
            (magit-gha-badge--on-fetch repo '(:no-runs t))
            (expect (with-current-buffer (plist-get state :buffer) (buffer-string))
                    :to-equal "Head:     main Fix parser crash\n\nUntracked files\nnotes.txt\n")
            ;; no text left to swap into: a later run comes back by a refresh
            (expect (plist-get (magit-gha-badge--state repo) :section) :to-be nil))
        (kill-buffer (plist-get state :buffer))
        (remhash repo magit-gha-badge--cache)))))

(describe "magit-gha-badge--swap"
  (it "replaces the badge and its job lines, and keeps the sections around it"
    (let* ((state (magit-gha-badge-tests--render
                   (list :status "in_progress" :workflow "CI" :sha "0ecbb23"
                         :jobs (list (magit-gha-badge-tests--job "Lint" "in_progress" nil "Check")
                                     (magit-gha-badge-tests--job "Test" "in_progress" nil
                                                                 "Run tests")))))
           (section (plist-get state :section)))
      (unwind-protect
          (with-current-buffer (plist-get state :buffer)
            (magit-gha-badge--swap
             (append (list :status "completed" :conclusion "failure"
                           :jobs (list (magit-gha-badge-tests--job "Test" "completed" "failure"
                                                                   "Run tests")))
                     state))
            (expect (buffer-string)
                    :to-equal (concat "Head:     main Fix parser crash\n\n"
                                      "Checks   🔴 CI 0ecbb23\n"
                                      "         🔴 Test - Run tests\n"
                                      "Untracked files\nnotes.txt\n"))
            (expect (buffer-substring (oref section content) (oref section end))
                    :to-equal "         🔴 Test - Run tests\n")
            (expect (marker-position (oref (magit-gha-badge-tests--child 'untracked) start))
                    :to-equal (marker-position (oref section end)))
            (expect (get-text-property (1- (oref section end)) 'magit-section)
                    :to-be section))
        (kill-buffer (plist-get state :buffer)))))

  (it "leaves a badge on top of the buffer on top, and a point below it below"
    (let* ((state (magit-gha-badge-tests--render
                   (list :status "in_progress" :workflow "CI" :sha "0ecbb23")
                   'first))
           (section (plist-get state :section)))
      (unwind-protect
          (with-current-buffer (plist-get state :buffer)
            (goto-char (oref section end))
            (magit-gha-badge--swap
             (append (list :jobs (list (magit-gha-badge-tests--job "Lint" "in_progress" nil
                                                                   "Check")))
                     state))
            (expect (marker-position (oref magit-root-section start)) :to-equal 1)
            (expect (marker-position (oref section start)) :to-equal 1)
            (expect (buffer-substring (point) (pos-eol)) :to-equal "Untracked files"))
        (kill-buffer (plist-get state :buffer)))))

  (it "keeps a collapsed badge collapsed, its new job lines hidden"
    (let* ((state (magit-gha-badge-tests--render
                   (list :status "in_progress" :workflow "CI" :sha "0ecbb23"
                         :jobs (list (magit-gha-badge-tests--job "Lint" "in_progress" nil
                                                                 "Check")))))
           (section (plist-get state :section)))
      (unwind-protect
          (with-current-buffer (plist-get state :buffer)
            (magit-section-hide section)
            (magit-gha-badge--swap
             (append (list :jobs (list (magit-gha-badge-tests--job "Lint" "in_progress" nil
                                                                   "Run linter")))
                     state))
            (expect (invisible-p (oref section start)) :to-be nil)
            (expect (invisible-p (oref section content)) :to-be-truthy)
            (expect (buffer-substring (oref section content) (oref section end))
                    :to-equal "         🟡 Lint - Run linter\n"))
        (kill-buffer (plist-get state :buffer))))))

(describe "magit-gha-badge-browse-run"
  (it "opens the page of the job on the line at point, without a fetch"
    (spy-on 'browse-url)
    (spy-on 'magit-gha-badge--fetch)
    (with-temp-buffer
      (insert (car (magit-gha-badge--job-lines
                    (list (magit-gha-badge-tests--job "Lint" "in_progress" nil "Check"))))
              "\n")
      (goto-char (point-min))
      (end-of-line)
      (magit-gha-badge-browse-run)
      (expect 'browse-url :to-have-been-called-with
              "https://github.com/agzam/.emacs.d/actions/runs/123/job/Lint")
      (expect 'magit-gha-badge--fetch :not :to-have-been-called))))

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
