;;; test/magit-gha-badge-tests.el --- magit-gha-badge specs -*- lexical-binding: t; -*-

;;; Commentary:
;; Pure-function coverage: parsing, glyph mapping, merge/re-arm decisions,
;; line rendering, cache updates.  No network, no live magit buffers -
;; marker/timer integration is not replica-testable (buffers without real
;; sections prove nothing).

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
         (display_title . "Fix parser crash"))))))

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
      (expect (plist-get result :workflow) :to-equal "CI")
      (expect (plist-get result :title) :to-equal "Fix parser crash")))
  (it "keeps a null conclusion nil while a run is in progress"
    (let ((result (magit-gha-badge--parse-payload
                   '((total_count . 1)
                     (workflow_runs . (((status . "in_progress")
                                       (conclusion . nil))))))))
      (expect (plist-get result :status) :to-equal "in_progress")
      (expect (plist-get result :conclusion) :to-be nil)))
  (it "turns an empty run list into the no-runs sentinel"
    (expect (magit-gha-badge--parse-payload '((total_count . 0) (workflow_runs . ())))
            :to-equal '(:no-runs t)))
  (it "treats a payload without workflow_runs as an error"
    (expect (magit-gha-badge--parse-payload '((message . "Not Found")))
            :to-equal '(:error t))
    (expect (magit-gha-badge--parse-payload nil) :to-equal '(:error t))))

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

(describe "magit-gha-badge--pending-p"
  (it "re-arms while a fresh result is still running"
    (expect (magit-gha-badge--pending-p '(:status "in_progress")) :to-be-truthy)
    (expect (magit-gha-badge--pending-p '(:status "queued")) :to-be-truthy))
  (it "does not re-arm on a completed result"
    (expect (magit-gha-badge--pending-p '(:status "completed" :conclusion "success"))
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
                   '(:timer tm :section sec :buffer buf :fetching now)
                   '(:status "completed"))))
      (expect (plist-get merged :timer) :to-be 'tm)
      (expect (plist-get merged :section) :to-be 'sec)
      (expect (plist-get merged :buffer) :to-be 'buf)
      (expect (plist-get merged :fetching) :to-be 'now)))
  (it "keeps the previous truth on a fetch error"
    (let ((merged (magit-gha-badge--merge-result
                   '(:status "completed" :conclusion "success" :url "old")
                   '(:error t))))
      (expect (plist-get merged :url) :to-equal "old")
      (expect (plist-get merged :status) :to-equal "completed")
      (expect (plist-get merged :error) :to-be t)))
  (it "clears run fields when the branch has no runs anymore"
    (let ((merged (magit-gha-badge--merge-result
                   '(:status "completed" :url "old")
                   '(:no-runs t))))
      (expect (plist-get merged :status) :to-be nil)
      (expect (plist-get merged :url) :to-be nil)
      (expect (plist-get merged :no-runs) :to-be t)))
  (it "does not mutate the input state"
    (let ((state (list :status "completed" :url "old")))
      (magit-gha-badge--merge-result state '(:error t))
      (magit-gha-badge--merge-result state '(:status "queued"))
      (expect state :to-equal '(:status "completed" :url "old")))))

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

(provide 'magit-gha-badge-tests)
;;; magit-gha-badge-tests.el ends here
