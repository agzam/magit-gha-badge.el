;;; test/magit-gha-badge-runs-tests.el --- magit-gha-badge-runs specs -*- lexical-binding: t; -*-

;;; Commentary:
;; The buffer of runs: its helpers, its drawing, the following of a run in
;; progress, the command and the request function.  Every question to
;; GitHub goes to a fake `magit-gha-badge-runs--ask' that answers at once
;; from a table.  No network.

;;; Code:

(require 'buttercup)
(require 'magit-gha-badge-runs)

;;; Helpers

(defvar magit-gha-badge-runs-tests--answers nil
  "Alist of (REGEXP . ANSWER) that the fake `magit-gha-badge-runs--ask' answers from.
ANSWER is the value, a function of the endpoint that returns it,
\(error STATUS MESSAGE) for a failure, `pending' for no answer at all, or
`held' for one that comes when the spec releases it.")

(defvar magit-gha-badge-runs-tests--asked nil
  "The endpoints the fake `magit-gha-badge-runs--ask' was asked for, newest first.")

(defvar magit-gha-badge-runs-tests--held nil
  "The questions whose answer is held: (ENDPOINT CALLBACK ERRBACK) each.")

(defun magit-gha-badge-runs-tests--release (regexp answer)
  "Answer the held question whose endpoint matches REGEXP with ANSWER.
ANSWER is the value, or (error STATUS MESSAGE) for a failure."
  (pcase-let ((`(,_ ,callback ,errback)
               (seq-find (lambda (held) (string-match-p regexp (car held)))
                         magit-gha-badge-runs-tests--held)))
    (setq magit-gha-badge-runs-tests--held
          (seq-remove (lambda (held) (string-match-p regexp (car held)))
                      magit-gha-badge-runs-tests--held))
    (pcase answer
      (`(error ,status ,message) (funcall errback status message))
      (_ (funcall callback answer)))))

(defun magit-gha-badge-runs-tests--ask (endpoint callback &optional errback _text)
  "Answer ENDPOINT at once from `magit-gha-badge-runs-tests--answers' to CALLBACK or ERRBACK.
An endpoint the table lacks fails with status 599, so a spec shows it."
  (push endpoint magit-gha-badge-runs-tests--asked)
  (pcase (cdr (seq-find (lambda (entry) (string-match-p (car entry) endpoint))
                        magit-gha-badge-runs-tests--answers))
    ('pending nil)
    ('held (push (list endpoint callback errback) magit-gha-badge-runs-tests--held))
    (`(error ,status ,message) (when errback (funcall errback status message)))
    ('nil (when errback (funcall errback 599 (concat "no fake answer for " endpoint))))
    ((and (pred functionp) answer) (funcall callback (funcall answer endpoint)))
    (answer (funcall callback answer))))

(defun magit-gha-badge-runs-tests--asked (regexp)
  "Return the endpoints matching REGEXP that were asked for, oldest first."
  (seq-filter (lambda (endpoint) (string-match-p regexp endpoint))
              (reverse magit-gha-badge-runs-tests--asked)))

(defun magit-gha-badge-runs-tests--step (number name status conclusion &optional started completed)
  "Return step NUMBER called NAME in STATUS with CONCLUSION, as the API has it.
STARTED and COMPLETED are its times."
  `((name . ,name) (status . ,status) (conclusion . ,conclusion) (number . ,number)
    (started_at . ,started) (completed_at . ,completed)))

(defun magit-gha-badge-runs-tests--job (id name status conclusion steps &optional started completed)
  "Return the job ID called NAME in STATUS with CONCLUSION and STEPS.
STARTED and COMPLETED are its times."
  `((id . ,id) (run_id . 464) (name . ,name) (status . ,status) (conclusion . ,conclusion)
    (started_at . ,started) (completed_at . ,completed)
    (html_url . ,(format "https://github.com/o/r/actions/runs/464/job/%d" id))
    (steps . ,steps)))

(defun magit-gha-badge-runs-tests--run (&rest changes)
  "Return a run that failed on main, with CHANGES, an alist, put over it."
  (append changes
          '((id . 464) (name . "Run Tests") (display_title . "Release 2.9.1")
            (run_number . 464) (event . "push") (status . "completed")
            (conclusion . "failure") (head_branch . "main")
            (head_sha . "3fccf0c0aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa")
            (created_at . "2026-11-15T22:22:57Z")
            (run_started_at . "2026-11-15T22:22:57Z")
            (updated_at . "2026-11-15T22:24:54Z")
            (actor . ((login . "agzam")))
            (html_url . "https://github.com/o/r/actions/runs/464"))))

(defconst magit-gha-badge-runs-tests--jobs
  (list (magit-gha-badge-runs-tests--job
         11 "integration-tests (29.4)" "completed" "success"
         (list (magit-gha-badge-runs-tests--step 1 "Set up job" "completed" "success"
                                                 "2026-11-15T22:23:00Z" "2026-11-15T22:23:02Z")
               (magit-gha-badge-runs-tests--step 2 "Run integration tests" "completed" "success"
                                                 "2026-11-15T22:23:02Z" "2026-11-15T22:23:35Z"))
         "2026-11-15T22:23:00Z" "2026-11-15T22:23:35Z")
        (magit-gha-badge-runs-tests--job
         12 "unit-tests (29.4)" "completed" "failure"
         (list (magit-gha-badge-runs-tests--step 1 "Set up job" "completed" "success"
                                                 "2026-11-15T22:23:00Z" "2026-11-15T22:23:02Z")
               (magit-gha-badge-runs-tests--step 2 "Check out" "completed" "success"
                                                 "2026-11-15T22:23:02Z" "2026-11-15T22:23:05Z")
               (magit-gha-badge-runs-tests--step 3 "Run unit tests" "completed" "failure"
                                                 "2026-11-15T22:23:05Z" "2026-11-15T22:23:18Z")
               (magit-gha-badge-runs-tests--step 4 "Lint" "completed" "skipped"
                                                 "2026-11-15T22:23:18Z" "2026-11-15T22:23:18Z"))
         "2026-11-15T22:23:00Z" "2026-11-15T22:24:53Z"))
  "Two jobs of run 464: one that passed and one whose third step failed.")

(defconst magit-gha-badge-runs-tests--log
  (concat "\uFEFF2026-11-15T22:23:05.1000000Z ##[group]Run make test\n"
          "2026-11-15T22:23:05.2000000Z \e[36;1mmake test\e[0m\n"
          "2026-11-15T22:23:05.3000000Z shell: /usr/bin/bash -e {0}\n"
          "2026-11-15T22:23:05.4000000Z ##[endgroup]\n"
          "2026-11-15T22:23:06.0000000Z Running 3 specs.\n"
          "2026-11-15T22:23:18.0000000Z ##[error]Process completed with exit code 2.\n")
  "The log of a failed step as GitHub serves it.")

(defun magit-gha-badge-runs-tests--answers (&rest more)
  "Return the answers of a failed run on main, MORE entries before them."
  (append more
          `(("actions/runs\\?branch=main"
             . ((workflow_runs . (,(magit-gha-badge-runs-tests--run)))))
            ("actions/runs/464/jobs" . ((jobs . ,magit-gha-badge-runs-tests--jobs)))
            ("actions/runs/464\\'" . ,(magit-gha-badge-runs-tests--run))
            ("actions/jobs/12/steps/3/logs" . ,magit-gha-badge-runs-tests--log))))

(defvar magit-gha-badge-runs-tests--buffer nil
  "The buffer of runs the spec in progress opened.")

(defvar magit-gha-badge-runs-tests--now (encode-time (iso8601-parse "2026-11-23T22:22:57Z"))
  "The present of the specs: eight days after the run of the fixtures.
A spec of a run in progress binds it to a time during that run.")

(defun magit-gha-badge-runs-tests--open (answers &optional target focus)
  "Open the runs of o/r under ANSWERS, drawn; return the buffer.
TARGET and FOCUS are as `magit-gha-badge-runs' takes them: TARGET is the branch
main by default, and `newest' stands for no target."
  (setq magit-gha-badge-runs-tests--answers answers
        magit-gha-badge-runs-tests--asked nil
        magit-gha-badge-runs-tests--held nil)
  (spy-on 'magit-gha-badge-runs--ask :and-call-fake #'magit-gha-badge-runs-tests--ask)
  (spy-on 'magit-gha-badge-runs--now :and-call-fake (lambda () magit-gha-badge-runs-tests--now))
  (setq magit-gha-badge-runs-tests--buffer
        (save-window-excursion
          (magit-gha-badge-runs "o" "r" (if (eq target 'newest) nil (or target '(ref . "main")))
                                focus)))
  (magit-gha-badge-runs-tests--draw)
  magit-gha-badge-runs-tests--buffer)

(defun magit-gha-badge-runs-tests--draw ()
  "Draw the spec's buffer until nothing waits to be drawn."
  (with-current-buffer magit-gha-badge-runs-tests--buffer
    ;; No sequence to map: drawing may ask for a log, whose answer draws again.
    (while (timerp magit-gha-badge-runs--draw-timer)
      (magit-gha-badge-runs--flush))))

(defun magit-gha-badge-runs-tests--shown ()
  "Return the lines of the spec's buffer that show, without the folded ones.
Trailing blanks go, so a heading reads without the space that ends it."
  (with-current-buffer magit-gha-badge-runs-tests--buffer
    (let ((position (point-min))
          (parts nil))
      ;; No sequence to map: the stretches that show are found in the buffer.
      (while (< position (point-max))
        (let ((next (next-single-char-property-change position 'invisible)))
          (unless (invisible-p position)
            (push (buffer-substring-no-properties position next) parts))
          (setq position next)))
      (mapcar #'string-trim-right
              (split-string (string-trim-right (apply #'concat (reverse parts))) "\n")))))

(defun magit-gha-badge-runs-tests--goto (text)
  "Move point in the spec's buffer to the start of the line that holds TEXT."
  (goto-char (point-min))
  (search-forward text)
  (beginning-of-line))

(defmacro magit-gha-badge-runs-tests--in-buffer (&rest body)
  "Run BODY in the spec's buffer."
  (declare (indent 0))
  `(with-current-buffer magit-gha-badge-runs-tests--buffer ,@body))

(defun magit-gha-badge-runs-tests--close ()
  "Kill the spec's buffer, and its timers with it."
  (when (buffer-live-p magit-gha-badge-runs-tests--buffer)
    (kill-buffer magit-gha-badge-runs-tests--buffer))
  (setq magit-gha-badge-runs-tests--buffer nil))

;;; Durations, ages and states

(describe "magit-gha-badge-runs--duration"
  (it "writes seconds, minutes and hours as GitHub does"
    (expect (magit-gha-badge-runs--duration 0) :to-equal "0s")
    (expect (magit-gha-badge-runs--duration 35) :to-equal "35s")
    (expect (magit-gha-badge-runs--duration 113) :to-equal "1m 53s")
    (expect (magit-gha-badge-runs--duration 3723) :to-equal "1h 2m 3s"))

  (it "rounds to the second"
    (expect (magit-gha-badge-runs--duration 59.6) :to-equal "1m 0s")))

(describe "magit-gha-badge-runs--age"
  (let ((then (encode-time (iso8601-parse "2026-11-15T12:00:00Z"))))
    (it "says now within a minute"
      (expect (magit-gha-badge-runs--age then (time-add then 30)) :to-equal "now"))

    (it "counts minutes, hours and days, one of them without the s"
      (expect (magit-gha-badge-runs--age then (time-add then 60)) :to-equal "1 minute ago")
      (expect (magit-gha-badge-runs--age then (time-add then 300)) :to-equal "5 minutes ago")
      (expect (magit-gha-badge-runs--age then (time-add then 3600)) :to-equal "1 hour ago")
      (expect (magit-gha-badge-runs--age then (time-add then (* 8 86400))) :to-equal "8 days ago"))

    (it "gives the date past 30 days"
      (expect (magit-gha-badge-runs--age then (time-add then (* 40 86400)))
              :to-equal "on Nov 15, 2026"))))

(describe "magit-gha-badge-runs--mark"
  (it "marks what is over by its conclusion"
    (expect (magit-gha-badge-runs--mark '((status . "completed") (conclusion . "success")))
            :to-equal "✓")
    (expect (get-text-property 0 'font-lock-face
                               (magit-gha-badge-runs--mark '((status . "completed")
                                                             (conclusion . "failure"))))
            :to-be 'magit-gha-badge-runs-failure)
    (expect (magit-gha-badge-runs--mark '((status . "completed") (conclusion . "skipped")))
            :to-equal "⊘"))

  (it "marks what is not over by its status"
    (expect (magit-gha-badge-runs--mark '((status . "in_progress") (conclusion))) :to-equal "●")
    (expect (magit-gha-badge-runs--mark '((status . "queued") (conclusion))) :to-equal "○")
    (expect (magit-gha-badge-runs--mark '((status . "waiting"))) :to-equal "○")))

(describe "magit-gha-badge-runs--time"
  (it "reads a timestamp of the API, and is nil for what is none"
    (expect (format-time-string "%F %T" (magit-gha-badge-runs--time "2026-11-15T22:23:05Z") t)
            :to-equal "2026-11-15 22:23:05")
    (expect (magit-gha-badge-runs--time "soon") :to-be nil)
    (expect (magit-gha-badge-runs--time "") :to-be nil)
    (expect (magit-gha-badge-runs--time nil) :to-be nil)))

(describe "magit-gha-badge-runs--elapsed"
  (it "is the time between the start and the end of what is over"
    (expect (magit-gha-badge-runs--elapsed (cadr magit-gha-badge-runs-tests--jobs)) :to-equal 113.0)
    (expect (magit-gha-badge-runs--elapsed (magit-gha-badge-runs-tests--run)) :to-equal 117.0))

  (it "runs to now for what is in progress"
    (let ((step (magit-gha-badge-runs-tests--step 3 "Run" "in_progress" nil
                                                  "2026-11-15T22:23:05Z")))
      (expect (magit-gha-badge-runs--elapsed
               step (encode-time (iso8601-parse "2026-11-15T22:23:17Z")))
              :to-equal 12.0)))

  (it "is nil for what has not started"
    (expect (magit-gha-badge-runs--elapsed
             (magit-gha-badge-runs-tests--step 5 "Later" "queued" nil "2026-11-15T22:23:05Z"))
            :to-be nil)))

;;; Logs

(describe "magit-gha-badge-runs--parse-line"
  (it "splits the time from the text"
    (expect (magit-gha-badge-runs--parse-line "2026-11-15T22:23:06.0000000Z Running 3 specs.")
            :to-equal '(nil "2026-11-15T22:23:06.0000000Z" "Running 3 specs.")))

  (it "keeps a line without a time as its text"
    (expect (magit-gha-badge-runs--parse-line "no time here") :to-equal '(nil nil "no time here")))

  (it "drops the carriage return of a Windows runner"
    (expect (nth 2 (magit-gha-badge-runs--parse-line "2026-11-15T22:23:06.0000000Z text\r"))
            :to-equal "text"))

  (it "turns color sequences into faces"
    (let ((text (nth 2 (magit-gha-badge-runs--parse-line
                        "2026-11-15T22:23:05.2000000Z \e[36;1mmake test\e[0m"))))
      (expect (substring-no-properties text) :to-equal "make test")
      (expect (get-text-property 0 'font-lock-face text) :to-be-truthy)))

  (it "words an error, a warning and a notice as GitHub's page does"
    (pcase-let ((`(,kind ,_ ,text)
                 (magit-gha-badge-runs--parse-line
                  "2026-11-15T22:23:18.0000000Z ##[error]Process completed with exit code 2.")))
      (expect kind :to-be 'error)
      (expect (substring-no-properties text)
              :to-equal "Error: Process completed with exit code 2.")
      (expect (get-text-property 0 'font-lock-face text) :to-be 'magit-gha-badge-runs-error-line))
    (expect (substring-no-properties
             (nth 2 (magit-gha-badge-runs--parse-line
                     "2026-11-15T22:23:18.0000000Z ##[warning]Old node")))
            :to-equal "Warning: Old node")
    (expect (substring-no-properties
             (nth 2 (magit-gha-badge-runs--parse-line
                     "2026-11-15T22:23:18.0000000Z ##[notice]Moving")))
            :to-equal "Notice: Moving"))

  (it "names a group by the text after its marker"
    (expect (magit-gha-badge-runs--parse-line "2026-11-15T22:23:05.1000000Z ##[group]Run make test")
            :to-equal '(group "2026-11-15T22:23:05.1000000Z" "Run make test"))
    (expect (car (magit-gha-badge-runs--parse-line "2026-11-15T22:23:05.4000000Z ##[endgroup]"))
            :to-be 'endgroup))

  (it "shows a command without its marker"
    (pcase-let ((`(,kind ,_ ,text)
                 (magit-gha-badge-runs--parse-line
                  "2026-11-15T22:23:05.0000000Z [command]/usr/bin/git version")))
      (expect kind :to-be 'command)
      (expect (substring-no-properties text) :to-equal "/usr/bin/git version")))

  (it "hides the lines that mark where an action starts and ends"
    (expect (car (magit-gha-badge-runs--parse-line
                  "2026-11-23T22:51:20.1307773Z ##[start-action display=Run x;id=__self.x]"))
            :to-be 'hidden)
    (expect (car (magit-gha-badge-runs--parse-line
                  "2026-11-23T22:51:21.7010731Z ##[end-action id=__self.x;outcome=success]"))
            :to-be 'hidden)))

(describe "magit-gha-badge-runs--parse-log"
  (it "reads each line, without the byte order mark and without a last empty line"
    (let ((lines (magit-gha-badge-runs--parse-log magit-gha-badge-runs-tests--log)))
      (expect (mapcar #'car lines) :to-equal '(group nil nil endgroup nil error))
      (expect (nth 1 (car lines)) :to-equal "2026-11-15T22:23:05.1000000Z")))

  (it "leaves out the hidden lines"
    (expect (mapcar #'car
                    (magit-gha-badge-runs--parse-log
                     (concat "2026-11-23T22:51:20.1Z ##[start-action display=x;id=y]\n"
                             "2026-11-23T22:51:20.2Z kept\n")))
            :to-equal '(nil)))

  (it "reads an empty log as no lines"
    (expect (magit-gha-badge-runs--parse-log "") :to-be nil)))

(describe "magit-gha-badge-runs--blocks"
  (it "numbers the lines and gathers a group, whose end has no number"
    (let ((blocks (magit-gha-badge-runs--blocks
                   (magit-gha-badge-runs--parse-log magit-gha-badge-runs-tests--log))))
      (expect (length blocks) :to-equal 3)
      (pcase-let ((`(group ,head . ,members) (car blocks)))
        (expect (car head) :to-equal 1)
        (expect (nth 3 head) :to-equal "Run make test")
        (expect (mapcar #'car members) :to-equal '(2 3)))
      (expect (car (nth 1 blocks)) :to-equal 4)
      (expect (seq-take (nth 2 blocks) 2) :to-equal '(5 error))))

  (it "closes a group that never ends, and one that another group interrupts"
    (let ((blocks (magit-gha-badge-runs--blocks '((group nil "a") (nil nil "one")
                                                  (group nil "b") (nil nil "two")))))
      (expect (mapcar #'car blocks) :to-equal '(group group))
      (expect (mapcar (lambda (block) (mapcar #'car (cdr block))) blocks)
              :to-equal '((1 2) (3 4)))))

  (it "is nil for no lines"
    (expect (magit-gha-badge-runs--blocks nil) :to-be nil)))

(describe "magit-gha-badge-runs--stamp"
  (it "writes the time of a log line as GitHub's page does"
    (expect (magit-gha-badge-runs--stamp "2026-11-15T22:23:22.8573381Z")
            :to-equal "Sun, 15 Nov 2026 22:23:22 GMT")
    (expect (magit-gha-badge-runs--line-time "2026-11-15T22:23:22.8573381Z")
            :to-equal "Sun, 15 Nov 2026 22:23:22 GMT")))

;;; The buffer of runs

(describe "A buffer of runs"
  (after-each (magit-gha-badge-runs-tests--close))

  (it "shows a run that failed: its jobs, the failed one open at its failed step and log"
    (magit-gha-badge-runs-tests--open (magit-gha-badge-runs-tests--answers))
    (expect (magit-gha-badge-runs-tests--shown)
            :to-equal
            '("o/r  main  3fccf0c"
              ""
              "✗ Release 2.9.1  Run Tests #464 1m 57s"
              "  Triggered via push 8 days ago · agzam pushed 3fccf0c main · Status Failure"
              "  ✓ integration-tests (29.4) 35s"
              "  ✗ unit-tests (29.4) 1m 53s"
              "    ✓ Set up job 2s"
              "    ✓ Check out 3s"
              "    ✗ Run unit tests 13s"
              "Run make test"
              "Running 3 specs."
              "Error: Process completed with exit code 2."
              "    ⊘ Lint 0s")))

  (it "unfolds every job, step and group on the command, and asks for the log of each step"
    (magit-gha-badge-runs-tests--open
     (append (magit-gha-badge-runs-tests--answers)
             '(("/logs" . "2026-11-15T22:23:00.5Z A line\n"))))
    (magit-gha-badge-runs-tests--in-buffer
      (magit-gha-badge-runs-unfold-all)
      (magit-gha-badge-runs-tests--draw))
    (expect (magit-gha-badge-runs-tests--shown)
            :to-equal
            '("o/r  main  3fccf0c"
              ""
              "✗ Release 2.9.1  Run Tests #464 1m 57s"
              "  Triggered via push 8 days ago · agzam pushed 3fccf0c main · Status Failure"
              "  ✓ integration-tests (29.4) 35s"
              "    ✓ Set up job 2s"
              "A line"
              "    ✓ Run integration tests 33s"
              "A line"
              "  ✗ unit-tests (29.4) 1m 53s"
              "    ✓ Set up job 2s"
              "A line"
              "    ✓ Check out 3s"
              "A line"
              "    ✗ Run unit tests 13s"
              "Run make test"
              "make test"
              "shell: /usr/bin/bash -e {0}"
              "Running 3 specs."
              "Error: Process completed with exit code 2."
              "    ⊘ Lint 0s"))
    (expect (magit-gha-badge-runs-tests--asked "/logs")
            :to-equal '("repos/o/r/actions/jobs/12/steps/3/logs"
                        "repos/o/r/actions/jobs/11/steps/1/logs"
                        "repos/o/r/actions/jobs/11/steps/2/logs"
                        "repos/o/r/actions/jobs/12/steps/1/logs"
                        "repos/o/r/actions/jobs/12/steps/2/logs")))

  (it "opens with every section unfolded where the command is in the hook of the mode"
    (let ((magit-gha-badge-runs-mode-hook (list #'magit-gha-badge-runs-unfold-all)))
      (magit-gha-badge-runs-tests--open
       (append (magit-gha-badge-runs-tests--answers)
               '(("/logs" . "2026-11-15T22:23:00.5Z A line\n")))))
    (expect (seq-count (lambda (line) (equal line "A line"))
                       (magit-gha-badge-runs-tests--shown))
            :to-equal 4)
    (expect (magit-gha-badge-runs-tests--shown) :to-contain "shell: /usr/bin/bash -e {0}")
    (magit-gha-badge-runs-tests--in-buffer
      (expect (buffer-substring-no-properties (point) (pos-eol))
              :to-equal "Error: Process completed with exit code 2.")))

  (it "keeps a job folded that the user folds once every section is unfolded"
    (magit-gha-badge-runs-tests--open (magit-gha-badge-runs-tests--answers))
    (magit-gha-badge-runs-tests--in-buffer
      (magit-gha-badge-runs--commanded)
      (magit-gha-badge-runs-unfold-all)
      (magit-gha-badge-runs-tests--draw)
      (magit-gha-badge-runs-tests--goto "integration-tests")
      (magit-section-toggle (magit-current-section))
      (magit-gha-badge-runs--changed)
      (magit-gha-badge-runs-tests--draw)
      (expect (magit-gha-badge-runs-tests--shown)
              :not :to-contain "    ✓ Run integration tests 33s")
      (expect (magit-gha-badge-runs-tests--shown)
              :to-contain "    ✓ Check out 3s")))

  (it "unfolds on the command what the user folded before it"
    (magit-gha-badge-runs-tests--open (magit-gha-badge-runs-tests--answers))
    (magit-gha-badge-runs-tests--in-buffer
      (magit-gha-badge-runs--commanded)
      (magit-gha-badge-runs-tests--goto "unit-tests")
      (magit-section-toggle (magit-current-section))
      (magit-gha-badge-runs--changed)
      (magit-gha-badge-runs-tests--draw)
      (expect (magit-gha-badge-runs-tests--shown) :not :to-contain "    ✓ Check out 3s")
      (magit-gha-badge-runs-unfold-all)
      (magit-gha-badge-runs-tests--draw)
      (expect (magit-gha-badge-runs-tests--shown) :to-contain "    ✓ Check out 3s")))

  (it "asks for 2 logs at a time where that is the limit, the next as each answer comes"
    (let ((magit-gha-badge-runs--log-limit 2))
      (magit-gha-badge-runs-tests--open
       (magit-gha-badge-runs-tests--answers '("/logs" . held)))
      (magit-gha-badge-runs-tests--in-buffer
        (magit-gha-badge-runs-unfold-all)
        (magit-gha-badge-runs-tests--draw)
        (expect (magit-gha-badge-runs-tests--asked "/logs")
                :to-equal '("repos/o/r/actions/jobs/12/steps/3/logs"
                            "repos/o/r/actions/jobs/11/steps/1/logs"))
        (expect magit-gha-badge-runs--waiting :to-equal '((11 . 2) (12 . 1) (12 . 2)))
        (expect (seq-count (lambda (line) (string-match-p "Loading the log…" line))
                           (magit-gha-badge-runs-tests--shown))
                :to-equal 5)
        (magit-gha-badge-runs-tests--release
         "jobs/11/steps/1/logs" "2026-11-15T22:23:00.5Z Runner ready\n")
        (magit-gha-badge-runs-tests--draw)
        (expect (magit-gha-badge-runs-tests--shown) :to-contain "Runner ready")
        (expect (last (magit-gha-badge-runs-tests--asked "/logs"))
                :to-equal '("repos/o/r/actions/jobs/11/steps/2/logs"))
        (magit-gha-badge-runs-tests--release
         "jobs/12/steps/3/logs" '(error 404 "GitHub answered 404 Not Found"))
        (magit-gha-badge-runs-tests--draw)
        (expect (last (magit-gha-badge-runs-tests--asked "/logs"))
                :to-equal '("repos/o/r/actions/jobs/12/steps/1/logs"))
        (expect magit-gha-badge-runs--waiting :to-equal '((12 . 2))))))

  (it "draws half a second after a log comes while others wait, and at once when none does"
    (let ((magit-gha-badge-runs--log-limit 2))
      (magit-gha-badge-runs-tests--open
       (magit-gha-badge-runs-tests--answers '("/logs" . held)))
      (magit-gha-badge-runs-tests--in-buffer
        (magit-gha-badge-runs-unfold-all)
        (magit-gha-badge-runs-tests--draw)
        (spy-on 'run-with-timer :and-call-through)
        (dolist (log '("jobs/11/steps/1/logs" "jobs/12/steps/3/logs" "jobs/11/steps/2/logs"))
          (magit-gha-badge-runs-tests--release log "2026-11-15T22:23:00.5Z A line\n")
          (magit-gha-badge-runs-tests--draw))
        (expect (mapcar (lambda (call) (seq-take call 3))
                        (spy-calls-all-args 'run-with-timer))
                :to-equal '((0.5 nil magit-gha-badge-runs--flush)
                            (0.5 nil magit-gha-badge-runs--flush)
                            (0 nil magit-gha-badge-runs--flush))))))

  (it "forgets the logs that wait when it asks GitHub anew"
    (let ((magit-gha-badge-runs--log-limit 2))
      (magit-gha-badge-runs-tests--open
       (magit-gha-badge-runs-tests--answers '("/logs" . held)))
      (magit-gha-badge-runs-tests--in-buffer
        (magit-gha-badge-runs-unfold-all)
        (magit-gha-badge-runs-tests--draw)
        (revert-buffer)
        (magit-gha-badge-runs-tests--draw)
        (expect (last (magit-gha-badge-runs-tests--asked "/logs") 2)
                :to-equal '("repos/o/r/actions/jobs/11/steps/1/logs"
                            "repos/o/r/actions/jobs/11/steps/2/logs"))
        (expect magit-gha-badge-runs--waiting :to-equal '((12 . 1) (12 . 2) (12 . 3))))))

  (it "leaves a buffer of another mode as it is, whichever command of the mode runs there"
    (with-temp-buffer
      (insert "not a buffer of runs")
      (setq magit-section-visibility-cache '((((file . "a")) . hide)))
      (magit-gha-badge-runs-toggle-timestamps)
      (magit-gha-badge-runs-unfold-all)
      ;; No sequence to map: a timer that draws is what must not be there.
      (while (timerp magit-gha-badge-runs--draw-timer)
        (magit-gha-badge-runs--flush))
      (expect (buffer-string) :to-equal "not a buffer of runs")
      (expect magit-section-visibility-cache :to-equal '((((file . "a")) . hide)))))

  (it "asks for the runs of the branch, their jobs, and the log of the failed step only"
    (magit-gha-badge-runs-tests--open (magit-gha-badge-runs-tests--answers))
    (expect (reverse magit-gha-badge-runs-tests--asked)
            :to-equal '("repos/o/r/actions/runs?branch=main&per_page=10"
                        "repos/o/r/actions/runs/464/jobs?per_page=100"
                        "repos/o/r/actions/jobs/12/steps/3/logs")))

  (it "puts point on the first error line, numbered as on GitHub's page"
    (magit-gha-badge-runs-tests--open (magit-gha-badge-runs-tests--answers))
    (magit-gha-badge-runs-tests--in-buffer
      (expect (buffer-substring-no-properties (point) (pos-eol))
              :to-equal "Error: Process completed with exit code 2.")
      (expect (get-text-property (point) 'magit-gha-badge-runs-line) :to-equal 5)
      (expect (substring-no-properties (get-text-property (point) 'line-prefix))
              :to-equal "      5  ")
      (expect (get-text-property (point) 'wrap-prefix) :to-equal "         ")))

  (it "asks for the log of a step when its section first opens, and once only"
    (magit-gha-badge-runs-tests--open
     (magit-gha-badge-runs-tests--answers
      '("actions/jobs/12/steps/1/logs" . "2026-11-15T22:23:00.5Z Runner ready\n")))
    (magit-gha-badge-runs-tests--in-buffer
      (magit-gha-badge-runs-tests--goto "Set up job")
      (magit-section-toggle (magit-current-section))
      (magit-gha-badge-runs-tests--draw)
      (expect (magit-gha-badge-runs-tests--shown) :to-contain "Runner ready")
      (magit-gha-badge-runs-tests--goto "Set up job")
      (magit-section-toggle (magit-current-section))
      (magit-section-toggle (magit-current-section))
      (magit-gha-badge-runs-tests--draw)
      (expect (length (magit-gha-badge-runs-tests--asked "steps/1/logs")) :to-equal 1)))

  (it "folds a group of log lines until it is opened"
    (magit-gha-badge-runs-tests--open (magit-gha-badge-runs-tests--answers))
    (expect (magit-gha-badge-runs-tests--shown) :not :to-contain "make test")
    (magit-gha-badge-runs-tests--in-buffer
      (magit-gha-badge-runs-tests--goto "Run make test")
      (magit-section-toggle (magit-current-section))
      (expect (seq-take (member "Run make test" (magit-gha-badge-runs-tests--shown)) 3)
              :to-equal '("Run make test" "make test" "shell: /usr/bin/bash -e {0}"))))

  (it "opens a group that holds an error line, as a script that fails inside one leaves it"
    (magit-gha-badge-runs-tests--open
     (magit-gha-badge-runs-tests--answers
      (cons "actions/jobs/12/steps/3/logs"
            (concat "\uFEFF2026-11-15T22:23:05.1000000Z ##[group]Run make test\n"
                    "2026-11-15T22:23:05.2000000Z make test\n"
                    "2026-11-15T22:23:05.4000000Z ##[endgroup]\n"
                    "2026-11-15T22:23:06.0000000Z ##[group]Tests\n"
                    "2026-11-15T22:23:07.0000000Z Running 3 specs.\n"
                    "2026-11-15T22:23:18.0000000Z ##[error]Process completed with exit code 2.\n"))))
    (expect (member "Tests" (magit-gha-badge-runs-tests--shown))
            :to-equal '("Tests" "Running 3 specs." "Error: Process completed with exit code 2."
                        "    ⊘ Lint 0s"))
    (expect (magit-gha-badge-runs-tests--shown) :not :to-contain "make test")
    (magit-gha-badge-runs-tests--in-buffer
      (expect (buffer-substring-no-properties (point) (pos-eol))
              :to-equal "Error: Process completed with exit code 2.")
      (expect (invisible-p (point)) :to-be nil)))

  (it "folds on RET at a heading only, by a keymap that the text of a section carries"
    (magit-gha-badge-runs-tests--open (magit-gha-badge-runs-tests--answers))
    (magit-gha-badge-runs-tests--in-buffer
      (magit-gha-badge-runs-tests--goto "Running 3 specs.")
      (expect (lookup-key (get-text-property (point) 'keymap) (kbd "RET"))
              :to-be #'magit-gha-badge-runs-toggle)
      (magit-gha-badge-runs-toggle)
      (expect (magit-gha-badge-runs-tests--shown) :to-contain "Running 3 specs.")
      (magit-gha-badge-runs-tests--goto "Run unit tests")
      (expect (lookup-key (get-text-property (point) 'keymap) (kbd "RET"))
              :to-be #'magit-gha-badge-runs-toggle)
      (magit-gha-badge-runs-toggle)
      (expect (magit-gha-badge-runs-tests--shown) :not :to-contain "Running 3 specs.")
      (magit-gha-badge-runs-toggle)
      (expect (magit-gha-badge-runs-tests--shown) :to-contain "Running 3 specs.")))

  (it "names the option of the token where GitHub denies a log to no token"
    (let ((magit-gha-badge-auth 'none))
      (magit-gha-badge-runs-tests--open
       (magit-gha-badge-runs-tests--answers
        '("steps/3/logs" . (error 403 "GitHub answered 403"))))
      (expect (magit-gha-badge-runs-tests--shown)
              :to-contain
              "         GitHub serves a log to a token only: see magit-gha-badge-auth")))

  (it "says that the token was denied a log"
    (magit-gha-badge-runs-tests--open
     (magit-gha-badge-runs-tests--answers
      '("steps/3/logs" . (error 403 "GitHub answered 403"))))
    (expect (magit-gha-badge-runs-tests--shown)
            :to-contain "         GitHub denied this log to the token (403)"))

  (it "says so where GitHub has no log, or the log has expired"
    (magit-gha-badge-runs-tests--open
     (magit-gha-badge-runs-tests--answers
      '("steps/3/logs" . (error 404 "GitHub answered 404 Not Found"))))
    (expect (magit-gha-badge-runs-tests--shown)
            :to-contain "         GitHub has no log for this step")
    (magit-gha-badge-runs-tests--close)
    (magit-gha-badge-runs-tests--open
     (magit-gha-badge-runs-tests--answers
      '("steps/3/logs" . (error 410 "GitHub answered 410 Gone"))))
    (expect (magit-gha-badge-runs-tests--shown) :to-contain "         The log has expired"))

  (it "shows why the runs did not come"
    (magit-gha-badge-runs-tests--open
     '(("actions/runs\\?branch=main" . (error 404 "GitHub answered 404 Not Found"))))
    (expect (magit-gha-badge-runs-tests--shown)
            :to-equal '("o/r  main"
                        ""
                        "GitHub has no such repository or run, or none this token may read.")))

  (it "says that a branch has no runs"
    (magit-gha-badge-runs-tests--open '(("actions/runs\\?branch=main" . ((workflow_runs)))))
    (expect (magit-gha-badge-runs-tests--shown) :to-equal '("o/r  main" "" "No workflow runs.")))

  (it "shows only the runs of the newest commit of a branch"
    (magit-gha-badge-runs-tests--open
     `(("actions/runs\\?branch=main"
        . ((workflow_runs . (,(magit-gha-badge-runs-tests--run)
                             ,(magit-gha-badge-runs-tests--run '(id . 465) '(name . "Lint")
                                                               '(conclusion . "success"))
                             ,(magit-gha-badge-runs-tests--run
                               '(id . 400)
                               '(head_sha . "0000000000000000000000000000000000000000"))))))
       ("actions/runs/[0-9]+/jobs" . ((jobs)))))
    (expect (seq-filter (lambda (line) (string-match-p "Release" line))
                        (magit-gha-badge-runs-tests--shown))
            :to-equal '("✗ Release 2.9.1  Run Tests #464 1m 57s"
                        "✓ Release 2.9.1  Lint #464 1m 57s"))
    (expect (magit-gha-badge-runs-tests--asked "/jobs")
            :to-equal '("repos/o/r/actions/runs/464/jobs?per_page=100"
                        "repos/o/r/actions/runs/465/jobs?per_page=100")))

  (it "asks for every run of a commit that fills the first page"
    (magit-gha-badge-runs-tests--open
     `(("actions/runs\\?branch=main"
        . ((workflow_runs . ,(mapcar (lambda (id) (magit-gha-badge-runs-tests--run (cons 'id id)))
                                     (number-sequence 1 10)))))
       ("actions/runs\\?head_sha=" . ((workflow_runs . (,(magit-gha-badge-runs-tests--run)))))
       ("actions/runs/[0-9]+/jobs" . ((jobs)))))
    (expect (magit-gha-badge-runs-tests--asked "head_sha")
            :to-equal (list (concat "repos/o/r/actions/runs?head_sha="
                                    magit-gha-badge-runs-tests--sha "&per_page=100")))
    (expect (length (magit-gha-badge-runs-tests--asked "/jobs")) :to-equal 1))

  (it "asks for one run, for the runs of a commit, or for those of a branch with a slash"
    (magit-gha-badge-runs-tests--open '() '(run . 464))
    (expect (car magit-gha-badge-runs-tests--asked) :to-equal "repos/o/r/actions/runs/464")
    (magit-gha-badge-runs-tests--close)
    (magit-gha-badge-runs-tests--open '() '(sha . "3fccf0c0aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"))
    (expect (car magit-gha-badge-runs-tests--asked)
            :to-equal (concat "repos/o/r/actions/runs?head_sha="
                              magit-gha-badge-runs-tests--sha "&per_page=100"))
    (magit-gha-badge-runs-tests--close)
    (magit-gha-badge-runs-tests--open '() '(ref . "feat/x"))
    (expect (car magit-gha-badge-runs-tests--asked)
            :to-equal "repos/o/r/actions/runs?branch=feat%2Fx&per_page=10"))

  (it "opens the job that a link named"
    (magit-gha-badge-runs-tests--open (magit-gha-badge-runs-tests--answers) nil 11)
    (expect (magit-gha-badge-runs-tests--shown) :to-contain "    ✓ Run integration tests 33s"))

  (it "shows the time of each log line when asked"
    (magit-gha-badge-runs-tests--open (magit-gha-badge-runs-tests--answers))
    (magit-gha-badge-runs-tests--in-buffer
      (magit-gha-badge-runs-toggle-timestamps)
      (magit-gha-badge-runs-tests--draw)
      (magit-gha-badge-runs-tests--goto "Running 3 specs.")
      (expect (substring-no-properties (get-text-property (point) 'line-prefix))
              :to-equal "      4  Sun, 15 Nov 2026 22:23:06 GMT  ")))

  (it "gives the address of GitHub's page for a run, a job, a step and a log line"
    (magit-gha-badge-runs-tests--open (magit-gha-badge-runs-tests--answers))
    (magit-gha-badge-runs-tests--in-buffer
      (magit-gha-badge-runs-tests--goto "Release 2.9.1")
      (expect (magit-gha-badge-runs--url) :to-equal "https://github.com/o/r/actions/runs/464")
      (magit-gha-badge-runs-tests--goto "integration-tests")
      (expect (magit-gha-badge-runs--url)
              :to-equal "https://github.com/o/r/actions/runs/464/job/11")
      (magit-gha-badge-runs-tests--goto "Run unit tests")
      (expect (magit-gha-badge-runs--url)
              :to-equal "https://github.com/o/r/actions/runs/464/job/12#step:3:1")
      (magit-gha-badge-runs-tests--goto "Error: Process")
      (expect (magit-gha-badge-runs--url)
              :to-equal "https://github.com/o/r/actions/runs/464/job/12#step:3:5")
      (goto-char (point-min))
      (expect (magit-gha-badge-runs--url) :to-equal "https://github.com/o/r/actions")))

  (it "keeps point and folding when it is drawn again"
    (magit-gha-badge-runs-tests--open (magit-gha-badge-runs-tests--answers))
    (magit-gha-badge-runs-tests--in-buffer
      (magit-gha-badge-runs--commanded)
      (magit-gha-badge-runs-tests--goto "Run unit tests")
      (magit-section-toggle (magit-current-section))
      (magit-gha-badge-runs-tests--goto "integration-tests")
      (forward-char 4)
      (magit-gha-badge-runs--changed)
      (magit-gha-badge-runs-tests--draw)
      (expect (magit-gha-badge-runs-tests--shown) :not :to-contain "Running 3 specs.")
      (expect (buffer-substring-no-properties (point) (pos-eol))
              :to-equal "integration-tests (29.4) 35s ")))

  (it "keeps folding when it is drawn again, whatever Magit is set to keep"
    (let ((magit-section-cache-visibility nil))
      (magit-gha-badge-runs-tests--open (magit-gha-badge-runs-tests--answers))
      (magit-gha-badge-runs-tests--in-buffer
        (magit-gha-badge-runs-tests--goto "Run unit tests")
        (magit-section-toggle (magit-current-section))
        (magit-gha-badge-runs--changed)
        (magit-gha-badge-runs-tests--draw)
        (expect (magit-gha-badge-runs-tests--shown) :not :to-contain "Running 3 specs."))))

  (it "keeps the line a window starts at, and the point of that window"
    (magit-gha-badge-runs-tests--open (magit-gha-badge-runs-tests--answers))
    (magit-gha-badge-runs-tests--in-buffer
      (magit-gha-badge-runs--commanded)
      (save-window-excursion
        (set-window-buffer (selected-window) (current-buffer))
        (magit-gha-badge-runs-tests--goto "Check out")
        (set-window-start (selected-window) (point))
        (magit-gha-badge-runs-tests--goto "Running 3 specs.")
        (set-window-point (selected-window) (point))
        (magit-gha-badge-runs-toggle-timestamps)
        (magit-gha-badge-runs-tests--draw)
        (expect (save-excursion
                  (goto-char (window-start))
                  (buffer-substring-no-properties (point) (pos-eol)))
                :to-equal "    ✓ Check out 3s ")
        (expect (save-excursion
                  (goto-char (window-point))
                  (buffer-substring-no-properties (point) (pos-eol)))
                :to-equal "Running 3 specs."))))

  (it "opens and copies the address of what point is on"
    (spy-on 'browse-url)
    (magit-gha-badge-runs-tests--open (magit-gha-badge-runs-tests--answers))
    (magit-gha-badge-runs-tests--in-buffer
      (magit-gha-badge-runs-tests--goto "integration-tests")
      (magit-gha-badge-runs-browse)
      (expect 'browse-url :to-have-been-called-with
              "https://github.com/o/r/actions/runs/464/job/11")
      (let ((kill-ring nil)
            (inhibit-message t))
        (magit-gha-badge-runs-copy-url)
        (expect (car kill-ring) :to-equal "https://github.com/o/r/actions/runs/464/job/11"))))

  (it "stays at the end of the buffer as it grows"
    (magit-gha-badge-runs-tests--open (magit-gha-badge-runs-tests--answers))
    (magit-gha-badge-runs-tests--in-buffer
      (magit-gha-badge-runs--commanded)
      (goto-char (point-max))
      (magit-gha-badge-runs-toggle-timestamps)
      (magit-gha-badge-runs-tests--draw)
      (expect (point) :to-equal (point-max))))

  (it "asks GitHub anew on a revert, and keeps the logs it has"
    (magit-gha-badge-runs-tests--open (magit-gha-badge-runs-tests--answers))
    (magit-gha-badge-runs-tests--in-buffer
      (revert-buffer)
      (magit-gha-badge-runs-tests--draw)
      (expect (length (magit-gha-badge-runs-tests--asked "runs\\?branch=main")) :to-equal 2)
      (expect (length (magit-gha-badge-runs-tests--asked "steps/3/logs")) :to-equal 1)
      (expect (magit-gha-badge-runs-tests--shown)
              :to-contain "Error: Process completed with exit code 2."))))

;;; Following a run in progress

(defun magit-gha-badge-runs-tests--running-run ()
  "Return the run of the fixtures while it is in progress."
  (magit-gha-badge-runs-tests--run '(status . "in_progress") '(conclusion)))

(defun magit-gha-badge-runs-tests--running-job (steps &optional id name)
  "Return the job ID called NAME in progress with STEPS.
By default job 12, the unit tests."
  (magit-gha-badge-runs-tests--job (or id 12) (or name "unit-tests (29.4)") "in_progress" nil steps
                                   "2026-11-15T22:23:00Z"))

(defun magit-gha-badge-runs-tests--steps-at (running)
  "Return the four steps of a job while step RUNNING is in progress.
Each step before it succeeded, each after it waits; 5 has them all over."
  (let ((times '("2026-11-15T22:23:00Z" "2026-11-15T22:23:02Z" "2026-11-15T22:23:05Z"
                 "2026-11-15T22:23:18Z" "2026-11-15T22:23:20Z")))
    (seq-map-indexed
     (lambda (name index)
       (let ((number (1+ index)))
         (cond ((< number running)
                (magit-gha-badge-runs-tests--step number name "completed" "success"
                                                  (nth index times) (nth number times)))
               ((= number running)
                (magit-gha-badge-runs-tests--step number name "in_progress" nil (nth index times)))
               (t (magit-gha-badge-runs-tests--step number name "queued" nil)))))
     '("Set up job" "Check out" "Run unit tests" "Lint"))))

(describe "A buffer of a run in progress"
  :var (run jobs)
  (before-each
    (setq run (magit-gha-badge-runs-tests--running-run)
          jobs (list (magit-gha-badge-runs-tests--running-job
                      (magit-gha-badge-runs-tests--steps-at 3)))
          magit-gha-badge-runs-tests--now (encode-time (iso8601-parse "2026-11-15T22:23:17Z")))
    (magit-gha-badge-runs-tests--open
     `(("actions/runs\\?branch=main" . ,(lambda (_) `((workflow_runs . (,run)))))
       ("actions/runs/464/jobs" . ,(lambda (_) `((jobs . ,jobs))))
       ("actions/runs/464\\'" . ,(lambda (_) run))
       ("steps/2/logs" . "2026-11-15T22:23:03.0Z Checked out\n")
       ("steps/3/logs" . ,magit-gha-badge-runs-tests--log)
       ("steps/4/logs" . "2026-11-15T22:23:19.0Z Linted\n"))))
  (after-each
    (magit-gha-badge-runs-tests--close)
    (setq magit-gha-badge-runs-tests--now (encode-time (iso8601-parse "2026-11-23T22:22:57Z"))))

  (it "shows the job that runs open, with the time each item has run"
    (expect (magit-gha-badge-runs-tests--shown)
            :to-equal
            '("o/r  main  3fccf0c"
              ""
              "● Release 2.9.1  Run Tests #464 20s"
              "  Triggered via push now · agzam pushed 3fccf0c main · Status In progress"
              "  ● unit-tests (29.4) 17s"
              "    ✓ Set up job 2s"
              "    ✓ Check out 3s"
              "    ● Run unit tests 12s"
              "    ○ Lint")))

  (it "says under a running step that GitHub serves its log once it ends"
    (magit-gha-badge-runs-tests--in-buffer
      (magit-gha-badge-runs-tests--goto "Run unit tests")
      (magit-section-toggle (magit-current-section))
      (expect (magit-gha-badge-runs-tests--shown)
              :to-contain "         This step is running. GitHub serves its log once it ends.")
      (expect (magit-gha-badge-runs-tests--asked "/logs") :to-be nil)))

  (it "unfolds the groups of a log that comes once every section is unfolded"
    (magit-gha-badge-runs-tests--in-buffer
      (magit-gha-badge-runs--commanded)
      (magit-gha-badge-runs-unfold-all)
      (magit-gha-badge-runs-tests--draw)
      (expect (magit-gha-badge-runs-tests--shown) :to-contain "Checked out")
      (setq jobs (list (magit-gha-badge-runs-tests--running-job
                        (magit-gha-badge-runs-tests--steps-at 4))))
      (magit-gha-badge-runs--poll (current-buffer))
      (magit-gha-badge-runs-tests--draw)
      (expect (seq-take (member "Run make test" (magit-gha-badge-runs-tests--shown)) 3)
              :to-equal '("Run make test" "make test" "shell: /usr/bin/bash -e {0}"))))

  (it "renews the durations in place, and keeps the sections whole"
    (magit-gha-badge-runs-tests--in-buffer
      (let ((root magit-root-section))
        (magit-gha-badge-runs--tick-clocks (encode-time (iso8601-parse "2026-11-15T22:23:22Z")))
        (expect (seq-filter (lambda (line) (string-match-p "Run Tests\\|unit" line))
                            (magit-gha-badge-runs-tests--shown))
                :to-equal
                '("● Release 2.9.1  Run Tests #464 25s"
                  "  ● unit-tests (29.4) 22s"
                  "    ● Run unit tests 17s"))
        (expect magit-root-section :to-be root)
        (magit-gha-badge-runs-tests--goto "Run unit tests")
        (expect (oref (magit-current-section) value) :to-equal 3)
        (magit-gha-badge-runs-tests--goto "Check out")
        (expect (buffer-substring-no-properties (point) (pos-eol))
                :to-equal "    ✓ Check out 3s "))))

  (it "waits to ask again, and asks each sixth time only while no window shows the buffer"
    (magit-gha-badge-runs-tests--in-buffer
      (expect (timerp magit-gha-badge-runs--poll-timer) :to-be t)
      (expect (timerp magit-gha-badge-runs--clock-timer) :to-be t)
      (spy-on 'run-with-timer :and-call-through)
      (dotimes (_ 5)
        (cancel-timer magit-gha-badge-runs--poll-timer)
        (magit-gha-badge-runs--poll-due (current-buffer)))
      (expect (length (magit-gha-badge-runs-tests--asked "/jobs")) :to-equal 1)
      (expect (seq-take (spy-calls-args-for 'run-with-timer 4) 3)
              :to-equal '(5 nil magit-gha-badge-runs--poll-due))
      (cancel-timer magit-gha-badge-runs--poll-timer)
      (magit-gha-badge-runs--poll-due (current-buffer))
      (expect (length (magit-gha-badge-runs-tests--asked "/jobs")) :to-equal 2)
      (save-window-excursion
        (set-window-buffer (selected-window) (current-buffer))
        (cancel-timer magit-gha-badge-runs--poll-timer)
        (magit-gha-badge-runs--poll-due (current-buffer)))
      (expect (length (magit-gha-badge-runs-tests--asked "/jobs")) :to-equal 3)))

  (it "draws nothing again while GitHub answers the same"
    (magit-gha-badge-runs-tests--in-buffer
      (magit-gha-badge-runs--poll (current-buffer))
      (expect (timerp magit-gha-badge-runs--draw-timer) :to-be nil)
      (expect (magit-gha-badge-runs-tests--asked "/jobs")
              :to-equal '("repos/o/r/actions/runs/464/jobs?per_page=100"
                          "repos/o/r/actions/runs/464/jobs?per_page=100"))))

  (it "shows by itself the log of a step that ended in the only job in progress"
    (setq jobs (list (magit-gha-badge-runs-tests--running-job
                      (magit-gha-badge-runs-tests--steps-at 2))))
    (magit-gha-badge-runs-tests--in-buffer
      (magit-gha-badge-runs--poll (current-buffer))
      (magit-gha-badge-runs-tests--draw)
      (expect (magit-gha-badge-runs-tests--asked "/logs") :to-be nil)
      (setq jobs (list (magit-gha-badge-runs-tests--running-job
                        (magit-gha-badge-runs-tests--steps-at 3))))
      (magit-gha-badge-runs--poll (current-buffer))
      (magit-gha-badge-runs-tests--draw)
      (expect (magit-gha-badge-runs-tests--asked "/logs")
              :to-equal '("repos/o/r/actions/jobs/12/steps/2/logs"))
      (expect (magit-gha-badge-runs-tests--shown) :to-contain "Checked out")))

  (it "leaves the steps of a folded job alone, also once the job is unfolded"
    (setq jobs (list (magit-gha-badge-runs-tests--running-job
                      (magit-gha-badge-runs-tests--steps-at 2))))
    (magit-gha-badge-runs-tests--in-buffer
      (magit-gha-badge-runs--poll (current-buffer))
      (magit-gha-badge-runs-tests--draw)
      (magit-gha-badge-runs-tests--goto "unit-tests")
      (magit-section-toggle (magit-current-section))
      (setq jobs (list (magit-gha-badge-runs-tests--running-job
                        (magit-gha-badge-runs-tests--steps-at 3))))
      (magit-gha-badge-runs--poll (current-buffer))
      (magit-gha-badge-runs-tests--draw)
      (magit-gha-badge-runs-tests--goto "unit-tests")
      (magit-section-toggle (magit-current-section))
      (magit-gha-badge-runs-tests--draw)
      (expect magit-gha-badge-runs--followed :to-be nil)
      (expect (magit-gha-badge-runs-tests--asked "/logs") :to-be nil)
      (expect (magit-gha-badge-runs-tests--shown) :to-contain "    ✓ Check out 3s")))

  (it "follows no job while several are in progress, until point is in one of them"
    (let ((both (lambda (running)
                  (list (magit-gha-badge-runs-tests--running-job
                         (magit-gha-badge-runs-tests--steps-at running))
                        (magit-gha-badge-runs-tests--running-job
                         (magit-gha-badge-runs-tests--steps-at running)
                         13 "docs")))))
      (setq jobs (funcall both 1))
      (magit-gha-badge-runs-tests--in-buffer
        (magit-gha-badge-runs--commanded)
        (magit-gha-badge-runs--poll (current-buffer))
        (magit-gha-badge-runs-tests--draw)
        (setq jobs (funcall both 2))
        (magit-gha-badge-runs--poll (current-buffer))
        (magit-gha-badge-runs-tests--draw)
        (expect magit-gha-badge-runs--followed :to-be nil)
        (magit-gha-badge-runs-tests--goto "docs")
        (setq jobs (funcall both 3))
        (magit-gha-badge-runs--poll (current-buffer))
        (magit-gha-badge-runs-tests--draw)
        (expect magit-gha-badge-runs--followed :to-equal '((13 . 2)))
        (expect (magit-gha-badge-runs-tests--asked "/logs")
                :to-equal '("repos/o/r/actions/jobs/13/steps/2/logs")))))

  (it "follows the only job in progress while point is in a job that is over"
    (setq jobs (list (car magit-gha-badge-runs-tests--jobs)
                     (magit-gha-badge-runs-tests--running-job
                      (magit-gha-badge-runs-tests--steps-at 2))))
    (magit-gha-badge-runs-tests--in-buffer
      (magit-gha-badge-runs--commanded)
      (magit-gha-badge-runs--poll (current-buffer))
      (magit-gha-badge-runs-tests--draw)
      (magit-gha-badge-runs-tests--goto "integration-tests")
      (setq jobs (list (car magit-gha-badge-runs-tests--jobs)
                       (magit-gha-badge-runs-tests--running-job
                        (magit-gha-badge-runs-tests--steps-at 3))))
      (magit-gha-badge-runs--poll (current-buffer))
      (magit-gha-badge-runs-tests--draw)
      (expect magit-gha-badge-runs--followed :to-equal '((12 . 2)))))

  (it "follows no job while two runs have one in progress each"
    (magit-gha-badge-runs-tests--close)
    (let ((running 2))
      (magit-gha-badge-runs-tests--open
       `(("actions/runs\\?branch=main"
          . ((workflow_runs . (,run ,(magit-gha-badge-runs-tests--run '(id . 465) '(name . "Lint")
                                                                      '(status . "in_progress")
                                                                      '(conclusion))))))
         ("actions/runs/464/jobs"
          . ,(lambda (_)
               `((jobs . (,(magit-gha-badge-runs-tests--running-job
                            (magit-gha-badge-runs-tests--steps-at running)))))))
         ("actions/runs/465/jobs"
          . ,(lambda (_)
               `((jobs . (,(magit-gha-badge-runs-tests--running-job
                            (magit-gha-badge-runs-tests--steps-at running) 13 "docs"))))))))
      (setq running 3)
      (magit-gha-badge-runs-tests--in-buffer
        (magit-gha-badge-runs--poll (current-buffer))
        (magit-gha-badge-runs-tests--draw)
        (expect (magit-gha-badge-runs-tests--shown) :to-contain "    ● Run unit tests 12s")
        (expect magit-gha-badge-runs--followed :to-be nil))))

  (it "keeps a job open that ended well while it was open"
    (setq jobs (list (magit-gha-badge-runs-tests--job
                      12 "unit-tests (29.4)" "completed" "success"
                      (magit-gha-badge-runs-tests--steps-at 5)
                      "2026-11-15T22:23:00Z" "2026-11-15T22:23:20Z")))
    (magit-gha-badge-runs-tests--in-buffer
      (magit-gha-badge-runs--poll (current-buffer))
      (magit-gha-badge-runs-tests--draw)
      (expect (magit-gha-badge-runs-tests--shown) :to-contain "  ✓ unit-tests (29.4) 20s")
      (expect (magit-gha-badge-runs-tests--shown) :to-contain "    ✓ Lint 2s")
      (expect (magit-gha-badge-runs-tests--shown) :to-contain "Linted")))

  (it "keeps a job folded that ended while the user had it folded"
    (magit-gha-badge-runs-tests--in-buffer
      (magit-gha-badge-runs-tests--goto "unit-tests")
      (magit-section-toggle (magit-current-section))
      (setq jobs (list (cadr magit-gha-badge-runs-tests--jobs)))
      (magit-gha-badge-runs--poll (current-buffer))
      (magit-gha-badge-runs-tests--draw)
      (expect (magit-gha-badge-runs-tests--shown) :to-contain "  ✗ unit-tests (29.4) 1m 53s")
      (expect (magit-gha-badge-runs-tests--shown) :not :to-contain "    ✗ Run unit tests 13s")))

  (it "shows the failed step and its log once the job fails, then says how the run ended"
    (spy-on 'message)
    (setq jobs (list (cadr magit-gha-badge-runs-tests--jobs)))
    (magit-gha-badge-runs-tests--in-buffer
      (magit-gha-badge-runs--poll (current-buffer))
      (magit-gha-badge-runs-tests--draw)
      (expect (magit-gha-badge-runs-tests--shown)
              :to-contain "Error: Process completed with exit code 2.")
      (expect (buffer-substring-no-properties (point) (pos-eol))
              :to-equal "Error: Process completed with exit code 2.")
      (expect (magit-gha-badge-runs-tests--asked "actions/runs/464\\'")
              :to-equal '("repos/o/r/actions/runs/464"))
      (expect (timerp magit-gha-badge-runs--poll-timer) :to-be t)
      (expect 'message :not :to-have-been-called)
      (setq run (magit-gha-badge-runs-tests--run))
      (magit-gha-badge-runs--poll (current-buffer))
      (magit-gha-badge-runs-tests--draw)
      (expect 'message :to-have-been-called-with
              "magit-gha-badge: %s #%s %s" "Run Tests" 464 "failed")
      (expect magit-gha-badge-runs--poll-timer :to-be nil)
      (expect magit-gha-badge-runs--clock-timer :to-be nil)
      (expect (car (magit-gha-badge-runs-tests--shown)) :to-equal "o/r  main  3fccf0c")
      (expect (nth 2 (magit-gha-badge-runs-tests--shown))
              :to-equal "✗ Release 2.9.1  Run Tests #464 1m 57s")))

  (it "asks no more once its buffer is killed"
    (let ((timers (magit-gha-badge-runs-tests--in-buffer
                    (list magit-gha-badge-runs--poll-timer magit-gha-badge-runs--clock-timer))))
      (magit-gha-badge-runs-tests--close)
      (expect (seq-some (lambda (timer) (memq timer timer-list)) timers) :to-be nil))))

;;; What the command shows

(defconst magit-gha-badge-runs-tests--sha "3fccf0c0aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
  "A full commit hash.")

(describe "magit-gha-badge-runs--parse-input"
  (it "reads the link of a run, of a job of a run and of an attempt"
    (expect (magit-gha-badge-runs--parse-input
             "https://github.com/agzam/remoto.el/actions/runs/43768979806")
            :to-equal '("agzam" "remoto.el" (run . 43768979806) nil))
    (expect (magit-gha-badge-runs--parse-input
             (concat "https://github.com/agzam/remoto.el/actions/runs/43768979806"
                     "/job/132980372893#step:8:12"))
            :to-equal '("agzam" "remoto.el" (run . 43768979806) 132980372893))
    (expect (magit-gha-badge-runs--parse-input "github.com/o/r/actions/runs/7/attempts/2")
            :to-equal '("o" "r" (run . 7) nil)))

  (it "reads the link of a check as its job"
    (expect (magit-gha-badge-runs--parse-input
             "https://github.com/o/r/runs/132980372893?check_suite_focus=true")
            :to-equal '("o" "r" (job . 132980372893))))

  (it "reads the link of a commit, and of the Actions page of a branch"
    (expect (magit-gha-badge-runs--parse-input
             (concat "https://github.com/o/r/commit/"
                     magit-gha-badge-runs-tests--sha "/checks"))
            :to-equal `("o" "r" (sha . ,magit-gha-badge-runs-tests--sha)))
    (expect (magit-gha-badge-runs--parse-input
             "https://github.com/o/r/actions?query=branch%3Afeat%2Fx")
            :to-equal '("o" "r" (ref . "feat/x"))))

  (it "reads any other link into a repository as the repository"
    (expect (magit-gha-badge-runs--parse-input "https://github.com/o/r/actions")
            :to-equal '("o" "r" nil))
    (expect (magit-gha-badge-runs--parse-input "https://github.com/o/r")
            :to-equal '("o" "r" nil))
    (expect (magit-gha-badge-runs--parse-input " https://github.com/o/r.git ")
            :to-equal '("o" "r" nil)))

  (it "reads OWNER/REPO, with a branch or a commit after @"
    (expect (magit-gha-badge-runs--parse-input "o/r") :to-equal '("o" "r" nil))
    (expect (magit-gha-badge-runs--parse-input "o/r@main")
            :to-equal '("o" "r" (ref . "main")))
    (expect (magit-gha-badge-runs--parse-input "o/r@feat/x")
            :to-equal '("o" "r" (ref . "feat/x")))
    (expect (magit-gha-badge-runs--parse-input
             (concat "o/r@" magit-gha-badge-runs-tests--sha))
            :to-equal `("o" "r" (sha . ,magit-gha-badge-runs-tests--sha))))

  (it "is nil for what names no repository"
    (expect (magit-gha-badge-runs--parse-input "nonsense") :to-be nil)
    (expect (magit-gha-badge-runs--parse-input "a/b/c") :to-be nil)
    (expect (magit-gha-badge-runs--parse-input "https://example.com/o/r") :to-be nil)))

(describe "magit-gha-badge-runs--context"
  (it "is its own in a buffer of runs"
    (magit-gha-badge-runs-tests--open (magit-gha-badge-runs-tests--answers) nil 12)
    (magit-gha-badge-runs-tests--in-buffer
      (expect (magit-gha-badge-runs--context) :to-equal '("o" "r" (ref . "main") 12)))
    (magit-gha-badge-runs-tests--close))

  (it "is nil outside a repository"
    (spy-on 'magit-toplevel :and-return-value nil)
    (with-temp-buffer
      (expect (magit-gha-badge-runs--context) :to-be nil)))

  (describe "in a repository"
    :var ((repo "/tmp/magit-gha-badge-runs-test-repo/") branch url pushed)
    (before-each
      (setq branch "feature"
            url "git@github.com:o/r.git"
            pushed "1111111111111111111111111111111111111111")
      (spy-on 'magit-toplevel :and-return-value repo)
      (spy-on 'magit-get-current-branch :and-call-fake (lambda () branch))
      (spy-on 'magit-gha-badge--remote-url :and-call-fake (lambda (_) url))
      (spy-on 'magit-gha-badge--pushed-sha :and-call-fake (lambda (_) pushed))
      (spy-on 'magit-commit-at-point :and-return-value nil)
      (spy-on 'magit-rev-verify :and-call-fake
              (lambda (rev)
                (and (member rev '("HEAD" "abc1234^{commit}"))
                     magit-gha-badge-runs-tests--sha))))
    (after-each
      (remhash repo magit-gha-badge--cache))

    (it "is the remote of the branch, and the branch"
      (with-temp-buffer
        (expect (magit-gha-badge-runs--context) :to-equal '("o" "r" (ref . "feature")))
        (expect 'magit-gha-badge--remote-url :to-have-been-called-with "feature")))

    (it "is the commit of a detached HEAD"
      (setq branch nil)
      (with-temp-buffer
        (expect (magit-gha-badge-runs--context)
                :to-equal `("o" "r" (sha . ,magit-gha-badge-runs-tests--sha)))))

    (it "is the commit at point in Magit, and in no other buffer"
      (spy-on 'magit-commit-at-point :and-return-value "abc1234")
      (with-temp-buffer
        (expect (magit-gha-badge-runs--context) :to-equal '("o" "r" (ref . "feature")))
        (let ((major-mode 'magit-log-mode))
          (expect (magit-gha-badge-runs--context)
                  :to-equal `("o" "r" (sha . ,magit-gha-badge-runs-tests--sha))))))

    (it "is the run and the job of a job line of the badge, from any column of it"
      (with-temp-buffer
        (insert "Checks   badge\n"
                (propertize "         ✗ unit-tests - Run unit tests"
                            'magit-gha-badge-url
                            "https://github.com/o/r/actions/runs/464/job/12"))
        (expect (magit-gha-badge-runs--context) :to-equal '("o" "r" (run . 464) 12))
        (goto-char (point-min))
        (expect (magit-gha-badge-runs--context) :to-equal '("o" "r" (ref . "feature")))))

    (it "is the commit of the run that the badge has from the upstream of a fork"
      (setq url "git@github.com:me/r.git")
      (puthash repo (list :slug "me/r" :repo "o/r" :pushed pushed
                          :sha magit-gha-badge-runs-tests--sha)
               magit-gha-badge--cache)
      (with-temp-buffer
        (expect (magit-gha-badge-runs--context)
                :to-equal `("o" "r" (sha . ,magit-gha-badge-runs-tests--sha)))))

    (it "is the fork itself when the badge has the run of another branch"
      (setq url "git@github.com:me/r.git")
      (puthash repo (list :slug "me/r" :repo "o/r"
                          :pushed "2222222222222222222222222222222222222222"
                          :sha magit-gha-badge-runs-tests--sha)
               magit-gha-badge--cache)
      (with-temp-buffer
        (expect (magit-gha-badge-runs--context)
                :to-equal '("me" "r" (ref . "feature")))))

    (it "refuses a remote that is not on GitHub"
      (setq url "git@codeberg.org:o/r.git")
      (with-temp-buffer
        (expect (magit-gha-badge-runs--context) :to-throw 'user-error)))))

(describe "magit-gha-badge-runs"
  (after-each (magit-gha-badge-runs-tests--close))

  (it "names its buffer after what it shows"
    (expect (buffer-name (magit-gha-badge-runs-tests--open
                          (magit-gha-badge-runs-tests--answers)))
            :to-equal "*magit-gha: o/r@main*")
    (magit-gha-badge-runs-tests--close)
    (expect (buffer-name (magit-gha-badge-runs-tests--open '() '(run . 464)))
            :to-equal "*magit-gha: o/r#464*")
    (magit-gha-badge-runs-tests--close)
    (expect (buffer-name (magit-gha-badge-runs-tests--open '() 'newest))
            :to-equal "*magit-gha: o/r*"))

  (it "asks for the newest runs of the repository when no ref is named"
    (magit-gha-badge-runs-tests--open '() 'newest)
    (expect (car magit-gha-badge-runs-tests--asked)
            :to-equal "repos/o/r/actions/runs?per_page=10"))

  (it "shows the run of the job that the link of a check names, that job open"
    (spy-on 'ghub-request :and-return-value '((id . 12) (run_id . 464)))
    (magit-gha-badge-runs-tests--open (magit-gha-badge-runs-tests--answers) '(job . 12))
    (expect (seq-take (spy-calls-args-for 'ghub-request 0) 2)
            :to-equal '("GET" "/repos/o/r/actions/jobs/12"))
    (expect (buffer-name magit-gha-badge-runs-tests--buffer)
            :to-equal "*magit-gha: o/r#464*")
    (expect (magit-gha-badge-runs-tests--asked "actions/runs/464\\'")
            :to-equal '("repos/o/r/actions/runs/464"))
    (magit-gha-badge-runs-tests--in-buffer
      (expect magit-gha-badge-runs--focus :to-equal 12)))

  (it "shows the same buffer again, asked anew, for the same target"
    (let ((first (magit-gha-badge-runs-tests--open
                  (magit-gha-badge-runs-tests--answers))))
      (expect (magit-gha-badge-runs-tests--open (magit-gha-badge-runs-tests--answers))
              :to-be first)
      (expect (magit-gha-badge-runs-tests--shown)
              :to-contain "Error: Process completed with exit code 2.")))

  (it "keeps the logs it has when it opens a buffer again"
    (magit-gha-badge-runs-tests--open (magit-gha-badge-runs-tests--answers))
    (magit-gha-badge-runs-tests--open (magit-gha-badge-runs-tests--answers))
    (expect (magit-gha-badge-runs-tests--asked "/logs") :to-be nil)
    (expect (magit-gha-badge-runs-tests--shown)
            :to-contain "Error: Process completed with exit code 2."))

  (it "runs the hook of the mode each time it opens a buffer, one that exists too"
    (let* ((times 0)
           (magit-gha-badge-runs-mode-hook (list (lambda () (setq times (1+ times))))))
      (magit-gha-badge-runs-tests--open (magit-gha-badge-runs-tests--answers))
      (magit-gha-badge-runs-tests--open (magit-gha-badge-runs-tests--answers))
      (expect times :to-equal 2)))

  (it "unfolds every section of a buffer that was open before the command was in the hook"
    (let ((answers (append (magit-gha-badge-runs-tests--answers)
                           '(("/logs" . "2026-11-15T22:23:00.5Z A line\n")))))
      (magit-gha-badge-runs-tests--open answers)
      (magit-gha-badge-runs-tests--in-buffer
        (magit-gha-badge-runs-tests--goto "unit-tests")
        (magit-section-toggle (magit-current-section)))
      (expect (magit-gha-badge-runs-tests--shown) :not :to-contain "A line")
      (expect (magit-gha-badge-runs-tests--shown) :not :to-contain "    ✓ Check out 3s")
      (let ((magit-gha-badge-runs-mode-hook (list #'magit-gha-badge-runs-unfold-all)))
        (magit-gha-badge-runs-tests--open answers))
      (expect (seq-count (lambda (line) (equal line "A line"))
                         (magit-gha-badge-runs-tests--shown))
              :to-equal 4)
      (expect (magit-gha-badge-runs-tests--shown) :to-contain "    ✓ Check out 3s")
      (expect (magit-gha-badge-runs-tests--shown)
              :to-contain "shell: /usr/bin/bash -e {0}")))

  (it "cancels the timers of a run in progress when it opens its buffer again"
    (let* ((run (magit-gha-badge-runs-tests--running-run))
           (jobs (list (magit-gha-badge-runs-tests--running-job
                        (magit-gha-badge-runs-tests--steps-at 3))))
           (answers `(("actions/runs\\?branch=main" . ((workflow_runs . (,run))))
                      ("actions/runs/464/jobs" . ((jobs . ,jobs)))
                      ("actions/runs/464\\'" . ,run)))
           (magit-gha-badge-runs-tests--now
            (encode-time (iso8601-parse "2026-11-15T22:23:17Z"))))
      (magit-gha-badge-runs-tests--open answers)
      (let ((before (magit-gha-badge-runs-tests--in-buffer
                      (list magit-gha-badge-runs--poll-timer
                            magit-gha-badge-runs--clock-timer))))
        (expect (seq-every-p (lambda (timer) (memq timer timer-list)) before) :to-be t)
        (magit-gha-badge-runs-tests--open answers)
        (expect (seq-some (lambda (timer) (memq timer timer-list)) before) :to-be nil)
        (magit-gha-badge-runs-tests--in-buffer
          (expect (timerp magit-gha-badge-runs--poll-timer) :to-be t)
          (expect (timerp magit-gha-badge-runs--clock-timer) :to-be t)))))

  (it "drops an answer to a question it asked before it opened the buffer again"
    (magit-gha-badge-runs-tests--open
     `(("actions/runs\\?branch=main"
        . ((workflow_runs . (,(magit-gha-badge-runs-tests--run)))))
       ("actions/runs/464/jobs" . held)))
    (let ((held magit-gha-badge-runs-tests--held))
      (magit-gha-badge-runs-tests--open (magit-gha-badge-runs-tests--answers))
      (setq magit-gha-badge-runs-tests--held held)
      (magit-gha-badge-runs-tests--release "runs/464/jobs" '((jobs)))
      (magit-gha-badge-runs-tests--draw)
      (expect (magit-gha-badge-runs-tests--shown)
              :to-contain "  ✗ unit-tests (29.4) 1m 53s"))))

(describe "magit-gha-badge-runs--read"
  (it "takes the buffer at hand when it names a repository"
    (spy-on 'magit-gha-badge-runs--context :and-return-value '("o" "r" (ref . "main")))
    (spy-on 'read-string)
    (expect (magit-gha-badge-runs--read nil) :to-equal '("o" "r" (ref . "main")))
    (expect 'read-string :not :to-have-been-called))

  (it "reads a repository or a link when asked to, or when the buffer names none"
    (spy-on 'magit-gha-badge-runs--context :and-return-value '("o" "r" (ref . "main")))
    (spy-on 'read-string :and-return-value "agzam/remoto.el@dev")
    (expect (magit-gha-badge-runs--read t)
            :to-equal '("agzam" "remoto.el" (ref . "dev")))
    (spy-on 'magit-gha-badge-runs--context :and-return-value nil)
    (expect (magit-gha-badge-runs--read nil)
            :to-equal '("agzam" "remoto.el" (ref . "dev"))))

  (it "refuses what names no repository"
    (spy-on 'magit-gha-badge-runs--context :and-return-value nil)
    (spy-on 'read-string :and-return-value "nonsense")
    (expect (magit-gha-badge-runs--read nil) :to-throw 'user-error)))

;;; Asking GitHub

(describe "magit-gha-badge-runs--ask"
  :var (timers got failed)
  (before-each
    (setq timers nil got nil failed nil)
    (spy-on 'magit-gha-badge--get)
    (let ((arm (symbol-function 'run-at-time)))
      (spy-on 'run-at-time :and-call-fake
              (lambda (&rest args)
                (car (setq timers (cons (apply arm args) timers)))))))
  (after-each
    (mapc #'cancel-timer timers))

  (it "asks for JSON, and for a log with the reader of text"
    (magit-gha-badge-runs--ask "repos/o/r/actions/runs/1" #'ignore)
    (magit-gha-badge-runs--ask "repos/o/r/actions/jobs/1/steps/2/logs" #'ignore nil t)
    (pcase-let ((`(,resource ,params ,_ ,_ ,reader)
                 (spy-calls-args-for 'magit-gha-badge--get 0)))
      (expect (list resource params reader)
              :to-equal '("/repos/o/r/actions/runs/1" nil nil)))
    (expect (nth 4 (spy-calls-args-for 'magit-gha-badge--get 1))
            :to-be #'magit-gha-badge-runs--read-text))

  (it "passes an answer on once"
    (magit-gha-badge-runs--ask "repos/o/r" (lambda (answer) (push answer got)))
    (let ((answer (nth 2 (spy-calls-args-for 'magit-gha-badge--get 0))))
      (funcall answer '((id . 1)))
      (funcall answer '((id . 2))))
    (expect got :to-equal '(((id . 1))))
    (expect (memq (car timers) timer-list) :to-be nil))

  (it "gives the status of an HTTP error, with GitHub's words when it has them"
    (magit-gha-badge-runs--ask "repos/o/r" #'ignore
                               (lambda (&rest args) (push args failed)))
    (funcall (nth 3 (spy-calls-args-for 'magit-gha-badge--get 0))
             '(error http 404 ((message . "Not Found"))))
    (expect failed :to-equal '((404 "GitHub answered 404: Not Found"))))

  (it "gives the status alone for a log, whose error document is text"
    (magit-gha-badge-runs--ask "repos/o/r/actions/jobs/1/steps/2/logs" #'ignore
                               (lambda (&rest args) (push args failed)) t)
    (funcall (nth 3 (spy-calls-args-for 'magit-gha-badge--get 0))
             '(error http 410 "{\"message\":\"Gone\"}"))
    (expect failed :to-equal '((410 "GitHub answered 410"))))

  (it "gives the text of a signal, with no status"
    (magit-gha-badge-runs--ask "repos/o/r" #'ignore
                               (lambda (&rest args) (push args failed)))
    (funcall (nth 3 (spy-calls-args-for 'magit-gha-badge--get 0))
             '(error "Required Github token does not exist"))
    (expect failed :to-equal '((nil "Required Github token does not exist"))))

  (it "fails a question with no answer by the deadline, and drops a late answer"
    (magit-gha-badge-runs--ask "repos/o/r" (lambda (answer) (push answer got))
                               (lambda (&rest args) (push args failed)))
    (expect (timer--time (car timers)) :not :to-be nil)
    (timer-event-handler (car timers))
    (expect failed :to-equal '((nil "GitHub did not answer in 30 seconds")))
    (funcall (nth 2 (spy-calls-args-for 'magit-gha-badge--get 0)) '((id . 1)))
    (funcall (nth 3 (spy-calls-args-for 'magit-gha-badge--get 0)) '(error http 500 nil))
    (expect got :to-be nil)
    (expect (length failed) :to-equal 1)))

(describe "magit-gha-badge-runs--read-text"
  (it "returns the body from point, as ghub leaves a response, in its characters"
    (with-temp-buffer
      (set-buffer-multibyte nil)
      (insert "HTTP/1.1 200 OK\n\n")
      (let ((body (point)))
        (insert (encode-coding-string "2026-11-15T22:23:05Z ✓ fertig, “gut”\n" 'utf-8))
        (set-buffer-multibyte t)
        (goto-char body)
        (expect (magit-gha-badge-runs--read-text 200)
                :to-equal "2026-11-15T22:23:05Z ✓ fertig, “gut”\n"))))

  (it "returns an empty body as an empty text"
    (with-temp-buffer
      (expect (magit-gha-badge-runs--read-text 200) :to-equal ""))))

;;; magit-gha-badge-runs-tests.el ends here
