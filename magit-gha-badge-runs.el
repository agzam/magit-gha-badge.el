;;; magit-gha-badge-runs.el --- GitHub Actions runs, jobs and logs  -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2026 Ag Ibragimov
;;
;; Author: Ag Ibragimov <agzam.ibragimov@gmail.com>
;; Assisted-by: ECA:claude-fable-5-2
;; Maintainer: Ag Ibragimov <agzam.ibragimov@gmail.com>
;;
;; SPDX-License-Identifier: GPL-3.0-or-later
;;
;; This file is not part of GNU Emacs.

;;; Commentary:

;; The workflow runs of a commit in one buffer, as GitHub's pages show
;; them: runs, their jobs, the steps of a job and the log of a step, each
;; a section that folds.  A run in progress is followed: the log of a
;; step comes when the step ends, which is when GitHub first serves it.

;;; Code:

(require 'ansi-color)
(require 'cl-lib)
(require 'iso8601)
(require 'magit-section)
(require 'text-property-search)
(require 'magit-gha-badge)

(eval-when-compile
  (require 'subr-x))

;;;; Options

(defcustom magit-gha-badge-runs-poll-interval 5
  "Seconds between two questions about a run in progress, in its buffer.
A buffer that no window shows asks six times less often."
  :type 'number
  :group 'magit-gha-badge)

(defface magit-gha-badge-runs-success '((t :inherit success))
  "Face of the mark of a run, job or step that succeeded."
  :group 'magit-gha-badge)

(defface magit-gha-badge-runs-failure '((t :inherit error))
  "Face of the mark of a run, job or step that failed."
  :group 'magit-gha-badge)

(defface magit-gha-badge-runs-pending '((t :inherit warning))
  "Face of the mark of a run, job or step that waits or runs."
  :group 'magit-gha-badge)

(defface magit-gha-badge-runs-muted '((t :inherit shadow))
  "Face of durations, notes and what was skipped."
  :group 'magit-gha-badge)

(defface magit-gha-badge-runs-title '((t :inherit magit-section-heading))
  "Face of the title of a run and of the name of a job."
  :group 'magit-gha-badge)

(defface magit-gha-badge-runs-error-line '((t :inherit error))
  "Face of an error line of a log."
  :group 'magit-gha-badge)

(defface magit-gha-badge-runs-warning-line '((t :inherit warning))
  "Face of a warning line of a log."
  :group 'magit-gha-badge)

(defface magit-gha-badge-runs-notice-line '((t :inherit font-lock-doc-face))
  "Face of a notice line of a log."
  :group 'magit-gha-badge)

(defface magit-gha-badge-runs-command '((t :inherit font-lock-function-name-face))
  "Face of a command line of a log."
  :group 'magit-gha-badge)

(defface magit-gha-badge-runs-line-number '((t :inherit line-number))
  "Face of the line numbers of a log."
  :group 'magit-gha-badge)

;;;; What a buffer holds

(defvar-local magit-gha-badge-runs--owner nil
  "The owner of the repository whose runs this buffer shows.")

(defvar-local magit-gha-badge-runs--repo nil
  "The name of the repository whose runs this buffer shows.")

(defvar-local magit-gha-badge-runs--target nil
  "What this buffer shows: (run . ID), (sha . SHA), (ref . NAME) or nil.
Nil is the newest commit of the repository that has runs.")

(defvar-local magit-gha-badge-runs--focus nil
  "The id of the job a link named, which opens by itself, or nil.")

(defvar-local magit-gha-badge-runs--runs nil
  "The runs this buffer shows, each as the API answers it, newest first.")

(defvar-local magit-gha-badge-runs--jobs nil
  "Hash table: run id -> its jobs as the API answers them, or (error . TEXT).")

(defvar-local magit-gha-badge-runs--logs nil
  "Hash table: (JOB-ID . STEP-NUMBER) -> the parsed lines of its log.
Or `loading' while it is asked for, `waiting' before that, or
\(error . TEXT).")
;; A buffer opened again starts its mode anew and keeps the logs it has.
(put 'magit-gha-badge-runs--logs 'permanent-local t)

(defvar-local magit-gha-badge-runs--waiting nil
  "The keys of the logs that wait to be asked for, the next one first.")

(defvar-local magit-gha-badge-runs--unfold-all nil
  "Non-nil once `magit-gha-badge-runs-unfold-all' ran in this buffer.
No section is folded at first then.")

(defvar-local magit-gha-badge-runs--followed nil
  "The (JOB-ID . STEP-NUMBER) of each step that ended while its job was watched.
Its log shows by itself, so a job in progress reads as it goes.")

(defvar-local magit-gha-badge-runs--status nil
  "Nil once the runs are in, `loading' before that, or the text of a failure.")

(defvar-local magit-gha-badge-runs--generation 0
  "Counts the times this buffer asked for its runs anew.
An answer to an earlier question is dropped.")
;; It counts on when the mode starts anew: an earlier answer may still come.
(put 'magit-gha-badge-runs--generation 'permanent-local t)

(defvar-local magit-gha-badge-runs--timestamps nil
  "Non-nil when each log line shows its time before it.")

(defvar-local magit-gha-badge-runs--auto-point t
  "Non-nil until a command runs in this buffer.
While it holds, point goes to the first error once its log is in.")

;;;; Asking GitHub

(defconst magit-gha-badge-runs--deadline 30
  "Seconds an answer may take; past them its question has failed.
url.el can drop a callback, and a question without an end would stop the
following of its run.")

(defun magit-gha-badge-runs--read-text (_status)
  "Return the body of the response at point as text, as a log comes.
A reader for `ghub-request', in place of the one that parses JSON."
  (decode-coding-string (buffer-substring-no-properties (point) (point-max))
                        'utf-8))

(defun magit-gha-badge-runs--failure (err)
  "Return (STATUS TEXT) of ERR, an error ghub reported or signaled.
STATUS is the HTTP status, nil when no answer came.  The error document
of a log comes as text, so only a parsed one gives GitHub's own words."
  (pcase err
    (`(error http ,code . ,rest)
     (let ((words (and (consp (car rest)) (alist-get 'message (car rest)))))
       (list code (format "GitHub answered %s%s" code
                          (if words (concat ": " words) "")))))
    (_ (list nil (error-message-string err)))))

(defun magit-gha-badge-runs--late (errback)
  "Tell ERRBACK, if any, that GitHub did not answer by the deadline."
  (when errback
    (funcall errback nil (format "GitHub did not answer in %d seconds"
                                 magit-gha-badge-runs--deadline))))

(defun magit-gha-badge-runs--ask (endpoint callback &optional errback text)
  "Ask GitHub's API for ENDPOINT by callback.
CALLBACK takes the answer: parsed JSON, or with TEXT the body as a string.
ERRBACK takes the HTTP status, nil when no answer came, and a message.
An answer past `magit-gha-badge-runs--deadline' is dropped."
  (let* ((timer (run-at-time magit-gha-badge-runs--deadline nil
                             #'magit-gha-badge-runs--late errback))
         ;; A timer that fired or was cancelled is off `timer-list'.
         (in-time (lambda (function &rest args)
                    (when (memq timer timer-list)
                      (cancel-timer timer)
                      (when function
                        (apply function args))))))
    (magit-gha-badge--get
     (concat "/" endpoint) nil
     (lambda (payload) (funcall in-time callback payload))
     (lambda (err)
       (apply in-time errback (magit-gha-badge-runs--failure err)))
     (and text #'magit-gha-badge-runs--read-text))))

(defun magit-gha-badge-runs--endpoint (&rest parts)
  "Return the endpoint of this buffer's repository that PARTS name."
  (format "repos/%s/%s/%s"
          magit-gha-badge-runs--owner magit-gha-badge-runs--repo
          (mapconcat (lambda (part) (format "%s" part)) parts "")))

(defun magit-gha-badge-runs--guarded (function)
  "Return FUNCTION as a callback of this buffer's present question.
It runs in this buffer, and not at all once the buffer is gone or has
asked for its runs anew."
  (let ((buffer (current-buffer))
        (generation magit-gha-badge-runs--generation))
    (lambda (&rest args)
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (when (= generation magit-gha-badge-runs--generation)
            (apply function args)))))))

;;;; Times, durations and states

(defun magit-gha-badge-runs--time (string)
  "Return the time that STRING, a timestamp of the API, names, or nil."
  (and (stringp string) (not (string-empty-p string))
       (condition-case nil
           (encode-time (iso8601-parse string))
         (wrong-type-argument nil))))

(defun magit-gha-badge-runs--duration (seconds)
  "Return SECONDS as GitHub writes a duration: 35s, 1m 53s, 1h 2m 3s."
  (let* ((seconds (max 0 (round seconds)))
         (hours (/ seconds 3600))
         (minutes (/ (% seconds 3600) 60))
         (rest (% seconds 60)))
    (cond ((< 0 hours) (format "%dh %dm %ds" hours minutes rest))
          ((< 0 minutes) (format "%dm %ds" minutes rest))
          (t (format "%ds" rest)))))

(defun magit-gha-badge-runs--count (count unit)
  "Return COUNT of UNIT ago, as in 1 minute ago and 8 days ago."
  (format "%d %s%s ago" count unit (if (= count 1) "" "s")))

(defun magit-gha-badge-runs--now ()
  "Return the present time, by which ages and running durations count."
  (current-time))

(defun magit-gha-badge-runs--age (time &optional now)
  "Return how long before NOW TIME was, as GitHub writes it.
That is now, 5 minutes ago, 3 hours ago, 8 days ago, and past 30 days
the date: on Nov 15, 2026."
  (let ((seconds (float-time
                  (time-subtract (or now (magit-gha-badge-runs--now)) time))))
    (cond ((< seconds 60) "now")
          ((< seconds 3600)
           (magit-gha-badge-runs--count (floor seconds 60) "minute"))
          ((< seconds 86400)
           (magit-gha-badge-runs--count (floor seconds 3600) "hour"))
          ((< seconds (* 30 86400))
           (magit-gha-badge-runs--count (floor seconds 86400) "day"))
          (t (let ((system-time-locale "C"))
               (format-time-string "on %b %-d, %Y" time))))))

(defun magit-gha-badge-runs--state (item)
  "Return the state of ITEM, a run, job or step.
Its conclusion once it is over, such as success or failure, else its
status: queued, in_progress, waiting, pending or requested."
  (let ((conclusion (alist-get 'conclusion item)))
    (if (and (stringp conclusion) (not (string-empty-p conclusion)))
        conclusion
      (or (alist-get 'status item) "queued"))))

(defun magit-gha-badge-runs--over-p (item)
  "Return non-nil when ITEM, a run, job or step, is over."
  (equal "completed" (alist-get 'status item)))

(defun magit-gha-badge-runs--running-p (item)
  "Return non-nil when ITEM, a run, job or step, is in progress."
  (equal "in_progress" (alist-get 'status item)))

(defun magit-gha-badge-runs--failed-p (item)
  "Return non-nil when ITEM, a run, job or step, failed."
  (magit-gha-badge--failed-p (alist-get 'conclusion item)))

(defun magit-gha-badge-runs--skipped-p (item)
  "Return non-nil when ITEM, a job or step, was skipped."
  (equal "skipped" (alist-get 'conclusion item)))

(defconst magit-gha-badge-runs--marks
  '(("success" "✓" magit-gha-badge-runs-success)
    ("failure" "✗" magit-gha-badge-runs-failure)
    ("timed_out" "✗" magit-gha-badge-runs-failure)
    ("startup_failure" "✗" magit-gha-badge-runs-failure)
    ("action_required" "!" magit-gha-badge-runs-pending)
    ("cancelled" "⊘" magit-gha-badge-runs-muted)
    ("skipped" "⊘" magit-gha-badge-runs-muted)
    ("neutral" "○" magit-gha-badge-runs-muted)
    ("stale" "○" magit-gha-badge-runs-muted)
    ("in_progress" "●" magit-gha-badge-runs-pending))
  "The mark and its face for each state of a run, job or step.
A state it lacks waits to start: queued, waiting, pending, requested.")

(defun magit-gha-badge-runs--mark (item)
  "Return the mark of the state of ITEM, a run, job or step, in its face."
  (pcase-let ((`(,mark ,face)
               (or (cdr (assoc (magit-gha-badge-runs--state item)
                               magit-gha-badge-runs--marks))
                   '("○" magit-gha-badge-runs-pending))))
    (propertize mark 'font-lock-face face)))

(defun magit-gha-badge-runs--started (item)
  "Return the time ITEM, a run, job or step, started, or nil."
  (magit-gha-badge-runs--time (or (alist-get 'run_started_at item)
                                  (alist-get 'started_at item))))

(defun magit-gha-badge-runs--elapsed (item &optional now)
  "Return the seconds ITEM, a run, job or step, took, or has run by NOW.
Nil for one that has not started."
  (when-let* (((or (magit-gha-badge-runs--over-p item)
                   (magit-gha-badge-runs--running-p item)))
              (start (magit-gha-badge-runs--started item))
              (end (if (magit-gha-badge-runs--over-p item)
                       (magit-gha-badge-runs--time (or (alist-get 'completed_at item)
                                                       (alist-get 'updated_at item)))
                     (or now (magit-gha-badge-runs--now)))))
    (max 0 (float-time (time-subtract end start)))))

;;;; Logs

(defconst magit-gha-badge-runs--line-regexp
  (rx bos (group (= 4 digit) "-" (= 2 digit) "-" (= 2 digit) "T"
                 (= 2 digit) ":" (= 2 digit) ":" (= 2 digit)
                 (? "." (+ digit)) "Z")
      " " (group (* anything)) eos)
  "Regexp of a log line as GitHub serves it: its time, a space, its text.")

(defconst magit-gha-badge-runs--markers
  '(("##[group]" group) ("##[endgroup]" endgroup)
    ("##[error]" error "Error: " magit-gha-badge-runs-error-line)
    ("##[warning]" warning "Warning: " magit-gha-badge-runs-warning-line)
    ("##[notice]" notice "Notice: " magit-gha-badge-runs-notice-line)
    ("##[debug]" debug "##[debug]" magit-gha-badge-runs-muted)
    ("##[command]" command "" magit-gha-badge-runs-command)
    ("[command]" command "" magit-gha-badge-runs-command)
    ("##[start-action " hidden) ("##[end-action " hidden))
  "What the marker at the head of a log line makes of the line.
Each is (MARKER KIND LABEL FACE): the line shows LABEL and the text after
MARKER in FACE, as GitHub's page words it.  A `hidden' line never shows.")

(defun magit-gha-badge-runs--parse-line (line)
  "Return (KIND TIME TEXT) of LINE, a line of a log as GitHub serves it.
KIND is nil for plain text, else that of its marker in
`magit-gha-badge-runs--markers'.  TEXT has its escape sequences as faces."
  (let* ((line (string-remove-suffix "\r" line))
         (stamped (string-match magit-gha-badge-runs--line-regexp line))
         (time (and stamped (match-string 1 line)))
         (text (if stamped (match-string 2 line) line))
         (marker (seq-find (lambda (marker) (string-prefix-p (car marker) text))
                           magit-gha-badge-runs--markers)))
    (pcase marker
      ('nil (list nil time (ansi-color-apply text)))
      (`(,head ,kind ,label ,face)
       (list kind time
             (propertize (concat label (ansi-color-filter-apply
                                        (substring text (length head))))
                         'font-lock-face face)))
      (`(,head ,kind)
       (list kind time (ansi-color-filter-apply (substring text (length head))))))))

(defun magit-gha-badge-runs--parse-log (text)
  "Return the lines of TEXT, the log of a step, each as (KIND TIME TEXT).
See `magit-gha-badge-runs--parse-line'.  The byte order mark at its head
goes, and so do the lines that only mark where an action starts and ends."
  (let* ((ansi-color-context nil)
         (split (split-string (string-remove-prefix "\uFEFF" text) "\n"))
         (lines (if (equal "" (car (last split))) (butlast split) split)))
    (seq-remove (lambda (line) (eq (car line) 'hidden))
                (mapcar #'magit-gha-badge-runs--parse-line lines))))

(defun magit-gha-badge-runs--blocks (lines)
  "Return LINES, the parsed lines of a log, numbered and with groups gathered.
A block is (NUMBER KIND TIME TEXT), or (group HEAD . MEMBERS) of such
lines: a ##[group] line and the lines up to its ##[endgroup], which has
no number."
  (pcase-let
      ((`(,blocks ,group ,_)
        (seq-reduce
         (lambda (state line)
           (pcase-let* ((`(,blocks ,group ,count) state)
                        (number (1+ count))
                        (closed (if group (cons (reverse group) blocks) blocks)))
             (pcase (car line)
               ('endgroup (list closed nil count))
               ('group (list closed (list (cons number line) 'group) number))
               (_ (if group
                      (list blocks (cons (cons number line) group) number)
                    (list (cons (cons number line) blocks) nil number))))))
         lines (list nil nil 0))))
    (reverse (if group (cons (reverse group) blocks) blocks))))

(defun magit-gha-badge-runs--stamp (time)
  "Return TIME, the timestamp of a log line, in the form of GitHub's page.
As in Sun, 15 Nov 2026 22:23:22 GMT."
  (if-let* ((parsed (magit-gha-badge-runs--time
                     (concat (substring time 0 (min 19 (length time))) "Z"))))
      (let ((system-time-locale "C"))
        (format-time-string "%a, %d %b %Y %H:%M:%S GMT" parsed t))
    time))

;;;; Sections

(defun magit-gha-badge-runs-toggle ()
  "Fold or unfold the section whose heading point is on.
On any other line do nothing: a key that folds a log from its middle
loses the reader's place."
  (interactive nil magit-gha-badge-runs-mode)
  (when-let* ((section (magit-section-at))
              ((magit-section-position-in-heading-p section)))
    (magit-section-toggle section)))

(defvar-keymap magit-gha-badge-runs-section-map
  :doc "Keymap of the sections of `magit-gha-badge-runs-mode'.
It is a property of their text, so it holds where a mode map does not:
in Evil's normal state, whose own RET moves down a line."
  "RET" #'magit-gha-badge-runs-toggle)

(defclass magit-gha-badge-runs-root-section (magit-section) ()
  "The section that holds every run of a buffer.")

(defclass magit-gha-badge-runs-section (magit-section)
  ((keymap :initform 'magit-gha-badge-runs-section-map))
  "A section of a buffer of runs that folds on RET at its heading.")

(defclass magit-gha-badge-runs-run-section (magit-gha-badge-runs-section) ()
  "The section of a workflow run; its value is the id of the run.")

(defclass magit-gha-badge-runs-job-section (magit-gha-badge-runs-section) ()
  "The section of a job of a run; its value is the id of the job.")

(defclass magit-gha-badge-runs-step-section (magit-gha-badge-runs-section) ()
  "The section of a step of a job; its value is the number of the step.")

(defclass magit-gha-badge-runs-group-section (magit-gha-badge-runs-section) ()
  "The section of a group of log lines; its value is the number of its line.")

(defface magit-gha-badge-runs-group '((t nil))
  "Face of the line that names a group of log lines."
  :group 'magit-gha-badge)

(defun magit-gha-badge-runs--clock-text (duration start)
  "Return DURATION as the text of a heading, near the right edge of the window.
Not at the edge: a terminal keeps its last column for the mark of a line
cut short.  START, the time an item in progress started, stays on the
text as a property."
  (propertize
   (concat (propertize " " 'display
                       `(space :align-to (- right ,(+ 3 (string-width duration)))))
           (propertize duration 'font-lock-face 'magit-gha-badge-runs-muted))
   'magit-gha-badge-runs-clock start))

(defun magit-gha-badge-runs--clock (item)
  "Return the duration of ITEM, a run, job or step, as text, or nil.
That of an item in progress carries its start, by which
`magit-gha-badge-runs--tick-clocks' renews it."
  (when-let* ((elapsed (magit-gha-badge-runs--elapsed item)))
    (magit-gha-badge-runs--clock-text (magit-gha-badge-runs--duration elapsed)
                                      (and (magit-gha-badge-runs--running-p item)
                                           (magit-gha-badge-runs--started item)))))

(defun magit-gha-badge-runs--tick-clocks (&optional now)
  "Renew each duration of this buffer that belongs to an item in progress.
NOW is the time they run to, by default the present.  Only the duration
changes: the sections around it keep their markers."
  (let ((inhibit-read-only t)
        (now (or now (magit-gha-badge-runs--now))))
    (with-silent-modifications
      (save-excursion
        (goto-char (point-min))
        ;; No sequence to map: the durations are found in the buffer's text.
        (while-let ((match (text-property-search-forward
                            'magit-gha-badge-runs-clock nil (lambda (_ value) value))))
          (let* ((start (prop-match-beginning match))
                 (since (prop-match-value match))
                 (kept (seq-mapcat (lambda (property)
                                     (list property (get-text-property start property)))
                                   '(magit-section keymap)))
                 (text (magit-gha-badge-runs--clock-text
                        (magit-gha-badge-runs--duration
                         (float-time (time-subtract now since)))
                        since)))
            (delete-region start (prop-match-end match))
            (insert (apply #'propertize text kept))))))))

(defun magit-gha-badge-runs--heading (indent item name &optional face)
  "Return the heading of ITEM, a job or step: its mark, NAME and its duration.
INDENT is the column of the mark and FACE the face of NAME.  The space
at its end is never renewed, so what Magit hangs on the line end stays."
  (concat (make-string indent ?\s)
          (magit-gha-badge-runs--mark item) " "
          (propertize name
                      'font-lock-face face
                      'magit-gha-badge-runs-failed
                      (and (magit-gha-badge-runs--failed-p item) t))
          (magit-gha-badge-runs--clock item)
          " "))

(defun magit-gha-badge-runs--run-heading (run)
  "Return the heading of RUN: its mark, title, workflow, number and duration."
  (concat (magit-gha-badge-runs--mark run) " "
          (propertize (string-replace "\n" " " (or (alist-get 'display_title run) ""))
                      'font-lock-face 'magit-gha-badge-runs-title)
          "  "
          (propertize (format "%s #%s"
                              (alist-get 'name run) (alist-get 'run_number run))
                      'font-lock-face 'magit-gha-badge-runs-muted)
          (magit-gha-badge-runs--clock run)
          " "))

(defun magit-gha-badge-runs--sentence (text)
  "Return TEXT, a state of the API such as in_progress, as words: In progress."
  (let ((words (string-replace "_" " " text)))
    (if (string-empty-p words)
        words
      (concat (upcase (substring words 0 1)) (substring words 1)))))

(defun magit-gha-badge-runs--run-meta (run)
  "Return the line under the heading of RUN, in the words of GitHub's run page.
What triggered it and when, who pushed which commit to which branch, and
its status."
  (let* ((event (or (alist-get 'event run) ""))
         (actor (alist-get 'login (or (alist-get 'triggering_actor run)
                                      (alist-get 'actor run))))
         (created (magit-gha-badge-runs--time (alist-get 'created_at run)))
         (sha (alist-get 'head_sha run))
         (facts (list (concat "Triggered via " (string-replace "_" " " event)
                              (and created
                                   (concat " " (magit-gha-badge-runs--age created))))
                      (string-join
                       (remq nil (list actor
                                       (and (equal event "push") "pushed")
                                       (and sha (substring sha 0 (min 7 (length sha))))
                                       (alist-get 'head_branch run)))
                       " ")
                      (concat "Status "
                              (magit-gha-badge-runs--sentence
                               (magit-gha-badge-runs--state run))))))
    (propertize (concat "  " (string-join facts " · "))
                'font-lock-face 'magit-gha-badge-runs-muted)))

;;;; Drawing

(defun magit-gha-badge-runs--insert-note (text &optional indent)
  "Insert TEXT as a quiet line at column INDENT, by default that of log text."
  (insert (propertize (concat (make-string (or indent 9) ?\s) text "\n")
                      'font-lock-face 'magit-gha-badge-runs-muted)))

(defvar magit-gha-badge-runs--dates (make-hash-table :test 'equal)
  "Cache: the date of a log timestamp -> that date as GitHub's page shows it.")

(defun magit-gha-badge-runs--line-time (time)
  "Return TIME, the timestamp of a log line, as `magit-gha-badge-runs--stamp' does.
The date of a line is that of most lines of its log, so it is kept."
  (if (< (length time) 19)
      time
    (let ((date (with-memoization (gethash (substring time 0 10)
                                           magit-gha-badge-runs--dates)
                  (substring (magit-gha-badge-runs--stamp
                              (concat (substring time 0 10) "T00:00:00Z"))
                             0 16))))
      (concat date " " (substring time 11 19) " GMT"))))

(defun magit-gha-badge-runs--line-prefix (number time)
  "Return the prefix of line NUMBER of a log: the number, and TIME if asked."
  (concat (propertize (format "%7d  " number) 'face 'magit-gha-badge-runs-line-number)
          (and magit-gha-badge-runs--timestamps time
               (propertize (concat (magit-gha-badge-runs--line-time time) "  ")
                           'face 'magit-gha-badge-runs-muted))))

(defun magit-gha-badge-runs--line-text (line)
  "Return the text of LINE, (NUMBER KIND TIME TEXT) of a log, with its number.
The number shows before the line and is no text of the buffer; a line
that wraps goes on under its text."
  (pcase-let* ((`(,number ,kind ,time ,text) line)
               (prefix (magit-gha-badge-runs--line-prefix number time)))
    (propertize text
                'line-prefix prefix
                'wrap-prefix (make-string (string-width prefix) ?\s)
                'magit-gha-badge-runs-line number
                'magit-gha-badge-runs-error (eq kind 'error))))

(defun magit-gha-badge-runs--insert-line (line)
  "Insert LINE, (NUMBER KIND TIME TEXT) of a log."
  (insert (magit-gha-badge-runs--line-text (append (seq-take line 3)
                                                   (list (concat (nth 3 line) "\n"))))))

(defun magit-gha-badge-runs--insert-block (block)
  "Insert BLOCK of a log: a line, or a group of lines as a section that folds.
See `magit-gha-badge-runs--blocks'.  A group starts folded, as on GitHub's
page, unless `magit-gha-badge-runs-unfold-all' ran in the buffer."
  (pcase block
    (`(group ,head . ,members)
     (magit-insert-section (magit-gha-badge-runs-group-section
                            (car head) (not magit-gha-badge-runs--unfold-all))
       (magit-insert-heading
         (magit-gha-badge-runs--line-text
          (append (seq-take head 3)
                  (list (propertize (if (string-empty-p (nth 3 head)) " " (nth 3 head))
                                    'font-lock-face 'magit-gha-badge-runs-group)))))
       (when members
         (magit-insert-section-body
           (mapc #'magit-gha-badge-runs--insert-line members)))))
    (_ (magit-gha-badge-runs--insert-line block))))

(defun magit-gha-badge-runs--log-key (job step)
  "Return the key of the log of STEP of JOB: (JOB-ID . STEP-NUMBER)."
  (cons (alist-get 'id job) (alist-get 'number step)))

(defun magit-gha-badge-runs--log-failure (status message)
  "Return the line that stands for a log GitHub did not serve.
STATUS is the HTTP status of its answer and MESSAGE what came with it."
  (pcase status
    (403 (if (eq magit-gha-badge-auth 'none)
             "GitHub serves a log to a token only: see magit-gha-badge-auth"
           "GitHub denied this log to the token (403)"))
    (404 "GitHub has no log for this step")
    (410 "The log has expired")
    (_ message)))

(defconst magit-gha-badge-runs--log-limit 8
  "The most logs a buffer asks GitHub for at one time.
The others wait their turn: with every step unfolded, a run of many jobs
would open hundreds of connections at once.")

(defun magit-gha-badge-runs--fetch-waiting ()
  "Ask for the logs that wait their turn, as many as the limit has room for."
  (let* ((asked (seq-count (lambda (log) (eq log 'loading))
                           (hash-table-values magit-gha-badge-runs--logs)))
         (room (max 0 (- magit-gha-badge-runs--log-limit asked)))
         (next (seq-take magit-gha-badge-runs--waiting room)))
    (setq magit-gha-badge-runs--waiting
          (seq-drop magit-gha-badge-runs--waiting room))
    (mapc #'magit-gha-badge-runs--fetch-log next)))

(defun magit-gha-badge-runs--take-log (key log)
  "Keep LOG as what KEY names, draw it, and ask for the next log that waits.
LOG is (log . BLOCKS) or (error . TEXT)."
  (puthash key log magit-gha-badge-runs--logs)
  (magit-gha-badge-runs--fetch-waiting)
  (magit-gha-badge-runs--changed))

(defun magit-gha-badge-runs--fetch-log (key)
  "Ask for the log of the step that KEY names, and draw it once it is in.
KEY is (JOB-ID . STEP-NUMBER)."
  (puthash key 'loading magit-gha-badge-runs--logs)
  (magit-gha-badge-runs--ask
   (magit-gha-badge-runs--endpoint "actions/jobs/" (car key)
                                   "/steps/" (cdr key) "/logs")
   (magit-gha-badge-runs--guarded
    (lambda (text)
      (magit-gha-badge-runs--take-log
       key (cons 'log (magit-gha-badge-runs--blocks
                       (magit-gha-badge-runs--parse-log text))))))
   (magit-gha-badge-runs--guarded
    (lambda (status message)
      (magit-gha-badge-runs--take-log
       key (cons 'error (magit-gha-badge-runs--log-failure status message)))))
   t))

(defun magit-gha-badge-runs--log (job step)
  "Return the log of STEP of JOB from this buffer, asked for when it has none.
That is (log . BLOCKS), `loading', `waiting' for its turn, (error . TEXT),
or `running' for a step in progress, whose log GitHub serves once it ends."
  (let ((key (magit-gha-badge-runs--log-key job step)))
    (cond ((magit-gha-badge-runs--running-p step) 'running)
          ((gethash key magit-gha-badge-runs--logs))
          (t (puthash key 'waiting magit-gha-badge-runs--logs)
             (setq magit-gha-badge-runs--waiting
                   (append magit-gha-badge-runs--waiting (list key)))
             (magit-gha-badge-runs--fetch-waiting)
             (gethash key magit-gha-badge-runs--logs)))))

(defun magit-gha-badge-runs--insert-step-body (job step)
  "Insert the body of STEP of JOB: its log, or a line on why there is none."
  (pcase (magit-gha-badge-runs--log job step)
    ('running (magit-gha-badge-runs--insert-note
               "This step is running. GitHub serves its log once it ends."))
    ((or 'loading 'waiting) (magit-gha-badge-runs--insert-note "Loading the log…"))
    (`(error . ,text) (magit-gha-badge-runs--insert-note text))
    (`(log . ,blocks)
     (if blocks
         (mapc #'magit-gha-badge-runs--insert-block blocks)
       (magit-gha-badge-runs--insert-note "The log is empty.")))))

(defun magit-gha-badge-runs--step-hidden-p (job step)
  "Return non-nil when the section of STEP of JOB is folded at first.
A step that failed starts open, and so does one that ended while its job
was followed.  `magit-gha-badge-runs-unfold-all' has every step start open."
  (not (or magit-gha-badge-runs--unfold-all
           (magit-gha-badge-runs--failed-p step)
           (member (magit-gha-badge-runs--log-key job step)
                   magit-gha-badge-runs--followed))))

(defun magit-gha-badge-runs--insert-step (job step)
  "Insert the section of STEP of JOB.
A step that was skipped or has not started has no body to unfold."
  (magit-insert-section (magit-gha-badge-runs-step-section
                         (alist-get 'number step)
                         (magit-gha-badge-runs--step-hidden-p job step))
    (magit-insert-heading
      (magit-gha-badge-runs--heading
       4 step (or (alist-get 'name step) "")
       (and (magit-gha-badge-runs--skipped-p step)
            'magit-gha-badge-runs-muted)))
    (when (or (magit-gha-badge-runs--running-p step)
              (and (magit-gha-badge-runs--over-p step)
                   (not (magit-gha-badge-runs--skipped-p step))))
      (magit-insert-section-body
        (magit-gha-badge-runs--insert-step-body job step)))))

(defun magit-gha-badge-runs--job-hidden-p (job)
  "Return non-nil when the section of JOB is folded at first.
A job that failed or runs starts open, and so does the job a link named.
`magit-gha-badge-runs-unfold-all' has every job start open."
  (not (or magit-gha-badge-runs--unfold-all
           (magit-gha-badge-runs--failed-p job)
           (magit-gha-badge-runs--running-p job)
           (equal (alist-get 'id job) magit-gha-badge-runs--focus))))

(defun magit-gha-badge-runs--insert-job (job)
  "Insert the section of JOB, with a section for each step under it."
  (magit-insert-section (magit-gha-badge-runs-job-section
                         (alist-get 'id job)
                         (magit-gha-badge-runs--job-hidden-p job))
    (magit-insert-heading
      (magit-gha-badge-runs--heading
       2 job (or (alist-get 'name job) "") 'magit-gha-badge-runs-title))
    (when-let* ((steps (alist-get 'steps job)))
      (magit-insert-section-body
        (dolist (step steps)
          (magit-gha-badge-runs--insert-step job step))))))

(defun magit-gha-badge-runs--insert-run (run)
  "Insert the section of RUN: its heading, its line of facts and its jobs."
  (let ((id (alist-get 'id run)))
    (magit-insert-section (magit-gha-badge-runs-run-section id)
      (magit-insert-heading (magit-gha-badge-runs--run-heading run))
      (insert (magit-gha-badge-runs--run-meta run) "\n")
      (pcase (gethash id magit-gha-badge-runs--jobs)
        ('nil (magit-gha-badge-runs--insert-note "Loading the jobs…" 2))
        (`(error . ,text) (magit-gha-badge-runs--insert-note text 2))
        (`(jobs . ,jobs) (mapc #'magit-gha-badge-runs--insert-job jobs)))
      (insert "\n"))))

(defun magit-gha-badge-runs--insert-header ()
  "Insert the header: the repository, the ref and commit, and any news."
  (let ((sha (alist-get 'head_sha (car magit-gha-badge-runs--runs))))
    (insert (propertize (format "%s/%s"
                                magit-gha-badge-runs--owner magit-gha-badge-runs--repo)
                        'font-lock-face 'magit-gha-badge-runs-title)
            (propertize (concat (pcase magit-gha-badge-runs--target
                                  (`(ref . ,ref) (concat "  " ref)))
                                (and sha
                                     (concat "  " (substring sha 0 (min 7 (length sha))))))
                        'font-lock-face 'magit-gha-badge-runs-muted)
            "\n\n"))
  (cond ((stringp magit-gha-badge-runs--status)
         (insert magit-gha-badge-runs--status "\n"))
        (magit-gha-badge-runs--runs nil)
        ((eq magit-gha-badge-runs--status 'loading)
         (magit-gha-badge-runs--insert-note "Loading the runs…" 0))
        (t (magit-gha-badge-runs--insert-note "No workflow runs." 0))))

;;;; Keeping one's place

(defun magit-gha-badge-runs--where (position)
  "Return where POSITION is, for `magit-gha-badge-runs--there' to find it again.
\(END IDENT LINE COLUMN POSITION): END is non-nil at the end of the buffer,
IDENT names the section, LINE counts from the start of that section."
  (let ((section (magit-section-at position)))
    (save-excursion
      (goto-char position)
      (list (and (= position (point-max)) (< (point-min) (point-max)))
            (and section (magit-section-ident section))
            (if section (count-lines (oref section start) (pos-bol)) 0)
            (- position (pos-bol))
            position))))

(defun magit-gha-badge-runs--nearest (ident)
  "Return (SECTION . EXACT) for IDENT, as `magit-section-ident' gives it.
SECTION is the one IDENT names, or the nearest one above it that the
buffer still has; EXACT is non-nil for the first."
  (seq-some (lambda (tail)
              (and-let* ((section (magit-get-section tail)))
                (cons section (eq tail ident))))
            (cl-maplist #'identity ident)))

(defun magit-gha-badge-runs--there (where)
  "Return the position that WHERE names in the buffer as it is drawn now.
See `magit-gha-badge-runs--where'.  A section that is gone or folded away gives
the start of the nearest section above it that shows."
  (pcase-let ((`(,end ,ident ,line ,column ,position) where))
    (pcase (and (not end) ident (magit-gha-badge-runs--nearest ident))
      ('nil (if end (point-max) (min position (point-max))))
      (`(,section . ,exact)
       (let ((folded (seq-find (lambda (each) (oref each hidden))
                               (reverse (magit-section-lineage section t)))))
         (cond
          ((and folded (not (eq folded section))) (marker-position (oref folded start)))
          ((or (not exact) (and folded (< 0 line)))
           (marker-position (oref section start)))
          (t (save-excursion
               (goto-char (oref section start))
               (forward-line line)
               (if (< (point) (oref section end))
                   (min (+ (point) column) (pos-eol))
                 (marker-position (oref section start)))))))))))

(defun magit-gha-badge-runs--places ()
  "Return where point and each window of this buffer are.
`magit-gha-badge-runs--restore' puts them back once the buffer is drawn again."
  (cons (magit-gha-badge-runs--where (point))
        (mapcar (lambda (window)
                  (list window
                        (magit-gha-badge-runs--where (window-point window))
                        (magit-gha-badge-runs--where (window-start window))))
                (get-buffer-window-list nil nil t))))

(defun magit-gha-badge-runs--restore (places)
  "Put point and each window of this buffer back where PLACES has them.
A window at the end of the buffer stays there, so a log reads as it grows."
  (goto-char (magit-gha-badge-runs--there (car places)))
  (pcase-dolist (`(,window ,point ,start) (cdr places))
    (when (window-live-p window)
      (set-window-point window (magit-gha-badge-runs--there point))
      (unless (car point)
        (set-window-start window (magit-gha-badge-runs--there start) t)))))

(defun magit-gha-badge-runs--goto-failure ()
  "Move point, here and in each window of this buffer, to the first failure.
That is the first error line of a log, else the heading of the first
step or job that failed."
  (when-let* ((position (or (text-property-any (point-min) (point-max)
                                               'magit-gha-badge-runs-error t)
                            (text-property-any (point-min) (point-max)
                                               'magit-gha-badge-runs-failed t))))
    (goto-char position)
    (dolist (window (get-buffer-window-list nil nil t))
      (set-window-point window position))))

(defvar-local magit-gha-badge-runs--draw-timer nil
  "The timer that draws this buffer once what it holds has changed.")

(defconst magit-gha-badge-runs--bulk-delay 0.5
  "Seconds from a change to its drawing while logs wait their turn.
A log that comes draws the whole buffer again, and with every section
unfolded they come by the hundred.")

(defvar magit-gha-badge-runs--drawn nil
  "The root section of the drawing that the one in progress takes the place of.")

(defun magit-gha-badge-runs--keep-open (section)
  "Return `show' when SECTION, of a job, was unfolded in the drawing before.
So nothing folds by itself: a job that was open while it ran stays open
once it ends well, though such a job is folded at first."
  (and-let* ((magit-gha-badge-runs--drawn)
             ((magit-gha-badge-runs-job-section-p section))
             (old (magit-get-section (magit-section-ident section)
                                     magit-gha-badge-runs--drawn))
             ((not (oref old hidden))))
    'show))

(defun magit-gha-badge-runs--render ()
  "Draw this buffer from its data, with point, windows and folding as before."
  (let ((places (magit-gha-badge-runs--places))
        (inhibit-read-only t)
        (magit-gha-badge-runs--drawn magit-root-section))
    (setq magit-section-pre-command-section nil
          magit-section-highlighted-sections nil
          magit-section-focused-sections nil)
    (erase-buffer)
    (save-excursion
      (magit-insert-section (magit-gha-badge-runs-root-section)
        (magit-gha-badge-runs--insert-header)
        (mapc #'magit-gha-badge-runs--insert-run magit-gha-badge-runs--runs)))
    (let ((magit-section-cache-visibility nil))
      (magit-section-show magit-root-section))
    (magit-gha-badge-runs--restore places)
    (when magit-gha-badge-runs--auto-point
      (magit-gha-badge-runs--goto-failure))
    (magit-section-update-highlight t)
    (set-buffer-modified-p nil)))

(defun magit-gha-badge-runs--changed ()
  "Draw this buffer again soon, once for every change of one moment.
Only a buffer of runs is drawn: drawing erases the buffer, and a command
of this mode can be called in a buffer of any other."
  (when (and (derived-mode-p 'magit-gha-badge-runs-mode)
             (not (timerp magit-gha-badge-runs--draw-timer)))
    (setq magit-gha-badge-runs--draw-timer
          (run-with-timer (if magit-gha-badge-runs--waiting
                              magit-gha-badge-runs--bulk-delay
                            0)
                          nil #'magit-gha-badge-runs--flush (current-buffer)))))

(defun magit-gha-badge-runs--flush (&optional buffer)
  "Draw BUFFER, by default the current one, if it waits to be drawn.
While its region is active it waits on: drawing would lose the region."
  (let ((buffer (or buffer (current-buffer))))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (when (timerp magit-gha-badge-runs--draw-timer)
          (cancel-timer magit-gha-badge-runs--draw-timer)
          (setq magit-gha-badge-runs--draw-timer
                (and (region-active-p)
                     (run-with-timer 1 nil #'magit-gha-badge-runs--flush buffer)))
          (unless magit-gha-badge-runs--draw-timer
            (magit-gha-badge-runs--render)))))))

;;;; Runs and jobs

(defun magit-gha-badge-runs--run-jobs (run-id)
  "Return the jobs of the run RUN-ID as this buffer has them, or nil."
  (pcase (gethash run-id magit-gha-badge-runs--jobs)
    (`(jobs . ,jobs) jobs)))

(defun magit-gha-badge-runs--job-section (id)
  "Return the section of the job ID in this buffer as it is drawn, or nil."
  (seq-some (lambda (run)
              (seq-find (lambda (job) (equal id (oref job value)))
                        (oref run children)))
            (and magit-root-section (oref magit-root-section children))))

(defun magit-gha-badge-runs--watched-job ()
  "Return the id of the job to follow step by step, or nil.
That is the job in progress that point is in, else the only job in
progress of this buffer, and neither while its section is folded: as on
GitHub's page, one watches one job at a time."
  (let* ((window (get-buffer-window (current-buffer) t))
         (at (nth 1 (magit-gha-badge-runs--items-at
                     (if window (window-point window) (point)))))
         (running (seq-filter #'magit-gha-badge-runs--running-p
                              (seq-mapcat (lambda (run)
                                            (magit-gha-badge-runs--run-jobs
                                             (alist-get 'id run)))
                                          magit-gha-badge-runs--runs)))
         (job (cond ((and at (magit-gha-badge-runs--running-p at)) at)
                    ((and running (null (cdr running))) (car running)))))
    (and-let* ((id (alist-get 'id job))
               (section (magit-gha-badge-runs--job-section id))
               ((not (oref section hidden))))
      id)))

(defun magit-gha-badge-runs--ended-steps (old new)
  "Return the key of each step of the watched job that ended from OLD to NEW.
Those are two answers of the jobs of one run.  Only a step with a log
counts; see `magit-gha-badge-runs--watched-job'."
  (and-let* ((watched (magit-gha-badge-runs--watched-job))
             (job (seq-find (lambda (each) (equal watched (alist-get 'id each))) new))
             (before (seq-find (lambda (each) (equal watched (alist-get 'id each)))
                               old)))
    (seq-keep
     (lambda (step)
       (and (magit-gha-badge-runs--over-p step)
            (not (magit-gha-badge-runs--skipped-p step))
            (not (magit-gha-badge-runs--over-p
                  (seq-find (lambda (each)
                              (equal (alist-get 'number each) (alist-get 'number step)))
                            (alist-get 'steps before))))
            (magit-gha-badge-runs--log-key job step)))
     (alist-get 'steps job))))

(defun magit-gha-badge-runs--take-jobs (run-id jobs)
  "Keep JOBS as the jobs of the run RUN-ID, and draw them when they changed.
A step of the watched job that ended since the answer before is followed:
its log shows by itself, see `magit-gha-badge-runs--ended-steps'."
  (unless (equal (gethash run-id magit-gha-badge-runs--jobs) (cons 'jobs jobs))
    (setq magit-gha-badge-runs--followed
          (append (magit-gha-badge-runs--ended-steps
                   (magit-gha-badge-runs--run-jobs run-id) jobs)
                  magit-gha-badge-runs--followed))
    (puthash run-id (cons 'jobs jobs) magit-gha-badge-runs--jobs)
    (magit-gha-badge-runs--changed)))

(defun magit-gha-badge-runs--fetch-jobs (run)
  "Ask for the jobs of RUN and draw them once they are in."
  (let ((id (alist-get 'id run)))
    (magit-gha-badge-runs--ask
     (magit-gha-badge-runs--endpoint "actions/runs/" id "/jobs?per_page=100")
     (magit-gha-badge-runs--guarded
      (lambda (answer)
        (magit-gha-badge-runs--take-jobs id (alist-get 'jobs answer))))
     (magit-gha-badge-runs--guarded
      (lambda (_status message)
        (unless (magit-gha-badge-runs--run-jobs id)
          (puthash id (cons 'error message) magit-gha-badge-runs--jobs)
          (magit-gha-badge-runs--changed)))))))

(defun magit-gha-badge-runs--newest-commit (listed)
  "Return those of LISTED, newest first, that belong to the commit of the first."
  (seq-filter (lambda (run)
                (equal (alist-get 'head_sha run) (alist-get 'head_sha (car listed))))
              listed))

(defun magit-gha-badge-runs--runs-endpoint (&optional sha)
  "Return the endpoint of the workflow run or run list of this buffer.
With SHA, every run of that commit, whatever the buffer shows."
  (pcase (if sha (cons 'sha sha) magit-gha-badge-runs--target)
    (`(run . ,id) (magit-gha-badge-runs--endpoint "actions/runs/" id))
    (`(sha . ,commit)
     (magit-gha-badge-runs--endpoint "actions/runs?head_sha=" commit "&per_page=100"))
    (`(ref . ,ref)
     (magit-gha-badge-runs--endpoint "actions/runs?branch="
                                     (url-hexify-string ref) "&per_page=10"))
    (_ (magit-gha-badge-runs--endpoint "actions/runs?per_page=10"))))

(defun magit-gha-badge-runs--fail (status message)
  "Show, in place of a run list, why none came.
STATUS is the HTTP status of the answer and MESSAGE what came with it."
  (setq magit-gha-badge-runs--status
        (if (eql status 404)
            "GitHub has no such repository or run, or none this token may read."
          message))
  (magit-gha-badge-runs--changed))

(defun magit-gha-badge-runs--take-runs (answer &optional whole)
  "Keep each run of ANSWER, ask for its jobs and follow it while in progress.
ANSWER is one run or a list of runs, of which the newest commit's show.
A page full of them asks for every run of that commit, unless WHOLE."
  (let* ((listed (alist-get 'workflow_runs answer))
         (runs (if (alist-get 'id answer)
                   (list answer)
                 (magit-gha-badge-runs--newest-commit listed)))
         (sha (alist-get 'head_sha (car runs))))
    (if (and (not whole) sha (< 9 (length runs)) (= (length runs) (length listed)))
        (magit-gha-badge-runs--ask
         (magit-gha-badge-runs--runs-endpoint sha)
         (magit-gha-badge-runs--guarded
          (lambda (every) (magit-gha-badge-runs--take-runs every t)))
         (magit-gha-badge-runs--guarded #'magit-gha-badge-runs--fail))
      (setq magit-gha-badge-runs--runs runs
            magit-gha-badge-runs--status nil)
      (mapc #'magit-gha-badge-runs--fetch-jobs runs)
      (magit-gha-badge-runs--changed)
      (magit-gha-badge-runs--follow))))

(defvar-local magit-gha-badge-runs--polling nil
  "The ids of the runs in progress whose jobs are being asked for.")

(defun magit-gha-badge-runs--load ()
  "Ask GitHub for the run list of this buffer, then for the jobs, and draw.
A log that was still asked for, or that failed, is asked for anew."
  (setq magit-gha-badge-runs--generation (1+ magit-gha-badge-runs--generation)
        magit-gha-badge-runs--status 'loading
        magit-gha-badge-runs--polling nil
        magit-gha-badge-runs--waiting nil)
  (maphash (lambda (key log)
             (unless (eq (car-safe log) 'log)
               (remhash key magit-gha-badge-runs--logs)))
           magit-gha-badge-runs--logs)
  (magit-gha-badge-runs--changed)
  (magit-gha-badge-runs--ask
   (magit-gha-badge-runs--runs-endpoint)
   (magit-gha-badge-runs--guarded #'magit-gha-badge-runs--take-runs)
   (magit-gha-badge-runs--guarded #'magit-gha-badge-runs--fail)))

;;;; Following a run in progress

(defvar-local magit-gha-badge-runs--poll-timer nil
  "The timer that asks next about the runs in progress of this buffer.")

(defvar-local magit-gha-badge-runs--clock-timer nil
  "The timer that renews each second the durations of what is in progress.")

(defvar-local magit-gha-badge-runs--waited 0
  "How many times in a row this buffer, in no window, let its turn to ask pass.")

(defun magit-gha-badge-runs--stop-following ()
  "Cancel the timers that follow what is in progress in this buffer."
  (dolist (timer (list magit-gha-badge-runs--poll-timer
                       magit-gha-badge-runs--clock-timer))
    (when (timerp timer)
      (cancel-timer timer)))
  (setq magit-gha-badge-runs--poll-timer nil
        magit-gha-badge-runs--clock-timer nil))

(defun magit-gha-badge-runs--stop ()
  "Cancel every timer of this buffer, as its end asks."
  (magit-gha-badge-runs--stop-following)
  (when (timerp magit-gha-badge-runs--draw-timer)
    (cancel-timer magit-gha-badge-runs--draw-timer))
  (setq magit-gha-badge-runs--draw-timer nil))

(defun magit-gha-badge-runs--tick (buffer)
  "Renew the durations of what is in progress in BUFFER while a window has it."
  (when (and (buffer-live-p buffer) (get-buffer-window buffer t))
    (with-current-buffer buffer
      (magit-gha-badge-runs--tick-clocks))))

(defun magit-gha-badge-runs--follow ()
  "Follow what is in progress in this buffer; stop once nothing is."
  (if (seq-every-p #'magit-gha-badge-runs--over-p magit-gha-badge-runs--runs)
      (magit-gha-badge-runs--stop-following)
    (unless (timerp magit-gha-badge-runs--poll-timer)
      (setq magit-gha-badge-runs--poll-timer
            (run-with-timer magit-gha-badge-runs-poll-interval nil
                            #'magit-gha-badge-runs--poll-due (current-buffer))))
    (unless (timerp magit-gha-badge-runs--clock-timer)
      (setq magit-gha-badge-runs--clock-timer
            (run-with-timer 1 1 #'magit-gha-badge-runs--tick (current-buffer))))))

(defun magit-gha-badge-runs--polled (run-id)
  "Note that the question about the run RUN-ID has its answer."
  (setq magit-gha-badge-runs--polling (remove run-id magit-gha-badge-runs--polling)))

(defun magit-gha-badge-runs--ended-words (run)
  "Return how RUN ended, as words for the echo area."
  (pcase (alist-get 'conclusion run)
    ("success" "succeeded")
    ("failure" "failed")
    ("cancelled" "was cancelled")
    ("skipped" "was skipped")
    ("timed_out" "timed out")
    (conclusion (format "ended: %s" conclusion))))

(defun magit-gha-badge-runs--take-run (run)
  "Put RUN, as GitHub answers it now, in the place of what this buffer held.
A run that ended since says so in the echo area and is followed no more."
  (let* ((id (alist-get 'id run))
         (old (seq-find (lambda (each) (equal id (alist-get 'id each)))
                        magit-gha-badge-runs--runs)))
    (when (and old (not (equal old run)))
      (setq magit-gha-badge-runs--runs
            (mapcar (lambda (each) (if (eq each old) run each))
                    magit-gha-badge-runs--runs))
      (magit-gha-badge-runs--changed)
      (when (and (not (magit-gha-badge-runs--over-p old))
                 (magit-gha-badge-runs--over-p run))
        (message "magit-gha-badge: %s #%s %s"
                 (alist-get 'name run) (alist-get 'run_number run)
                 (magit-gha-badge-runs--ended-words run))
        (magit-gha-badge-runs--follow)))))

(defun magit-gha-badge-runs--poll-run (run)
  "Ask for the jobs of RUN, and for RUN itself once each of its jobs is over.
A run whose question has no answer yet is not asked again."
  (let ((id (alist-get 'id run)))
    (unless (member id magit-gha-badge-runs--polling)
      (setq magit-gha-badge-runs--polling (cons id magit-gha-badge-runs--polling))
      (magit-gha-badge-runs--ask
       (magit-gha-badge-runs--endpoint "actions/runs/" id "/jobs?per_page=100")
       (magit-gha-badge-runs--guarded
        (lambda (answer)
          (let ((jobs (alist-get 'jobs answer)))
            (magit-gha-badge-runs--take-jobs id jobs)
            (if (seq-every-p #'magit-gha-badge-runs--over-p jobs)
                (magit-gha-badge-runs--ask
                 (magit-gha-badge-runs--endpoint "actions/runs/" id)
                 (magit-gha-badge-runs--guarded (lambda (now)
                                                  (magit-gha-badge-runs--polled id)
                                                  (magit-gha-badge-runs--take-run now)))
                 (magit-gha-badge-runs--guarded
                  (lambda (&rest _) (magit-gha-badge-runs--polled id))))
              (magit-gha-badge-runs--polled id)))))
       (magit-gha-badge-runs--guarded
        (lambda (&rest _) (magit-gha-badge-runs--polled id)))))))

(defun magit-gha-badge-runs--poll (buffer)
  "Ask about each run in progress of BUFFER, then wait to ask again."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (setq magit-gha-badge-runs--poll-timer nil
            magit-gha-badge-runs--waited 0)
      (mapc #'magit-gha-badge-runs--poll-run
            (seq-remove #'magit-gha-badge-runs--over-p
                        magit-gha-badge-runs--runs))
      (magit-gha-badge-runs--follow))))

(defun magit-gha-badge-runs--poll-due (buffer)
  "Poll BUFFER if a window has it, else each sixth time only.
The wait between two times stays short, so a buffer that comes back into
a window is asked about within one of them."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (if (or (get-buffer-window buffer t) (< 4 magit-gha-badge-runs--waited))
          (magit-gha-badge-runs--poll buffer)
        (setq magit-gha-badge-runs--poll-timer nil
              magit-gha-badge-runs--waited (1+ magit-gha-badge-runs--waited))
        (magit-gha-badge-runs--follow)))))

;;;; Mode

(defun magit-gha-badge-runs--items-at (&optional position)
  "Return (RUN JOB STEP) of the section at POSITION, by default point.
Each is as the API answers it; those below the section are nil."
  (let* ((lineage (and-let* ((section (magit-section-at (or position (point)))))
                    (magit-section-lineage section t)))
         (value (lambda (type)
                  (and-let* ((section (seq-find
                                       (lambda (each) (eq type (oref each type)))
                                       lineage)))
                    (oref section value))))
         (run-id (funcall value 'magit-gha-badge-runs-run-section))
         (job-id (funcall value 'magit-gha-badge-runs-job-section))
         (number (funcall value 'magit-gha-badge-runs-step-section))
         (job (seq-find (lambda (each) (equal job-id (alist-get 'id each)))
                        (magit-gha-badge-runs--run-jobs run-id))))
    (list (seq-find (lambda (each) (equal run-id (alist-get 'id each)))
                    magit-gha-badge-runs--runs)
          job
          (seq-find (lambda (each) (equal number (alist-get 'number each)))
                    (alist-get 'steps job)))))

(defun magit-gha-badge-runs--url ()
  "Return the address of GitHub's page for what point is on.
A run or a job gives its own page, a step or a log line the page of its
job at that step and line, anything else the Actions page."
  (pcase-let ((`(,run ,job ,step) (magit-gha-badge-runs--items-at)))
    (cond ((and job step)
           (format "%s#step:%s:%s" (alist-get 'html_url job) (alist-get 'number step)
                   (or (get-text-property (point) 'magit-gha-badge-runs-line) 1)))
          (job (alist-get 'html_url job))
          (run (alist-get 'html_url run))
          (t (format "https://github.com/%s/%s/actions"
                     magit-gha-badge-runs--owner magit-gha-badge-runs--repo)))))

(defun magit-gha-badge-runs-browse ()
  "Open GitHub's page for the run, job, step or log line at point."
  (interactive nil magit-gha-badge-runs-mode)
  (browse-url (magit-gha-badge-runs--url)))

(defun magit-gha-badge-runs-copy-url ()
  "Copy the address of GitHub's page for the run, job, step or log line at point."
  (interactive nil magit-gha-badge-runs-mode)
  (let ((url (magit-gha-badge-runs--url)))
    (kill-new url)
    (message "Copied %s" url)))

(defun magit-gha-badge-runs-toggle-timestamps ()
  "Show or hide the time of each log line, before the line."
  (interactive nil magit-gha-badge-runs-mode)
  (setq magit-gha-badge-runs--timestamps (not magit-gha-badge-runs--timestamps))
  (magit-gha-badge-runs--changed))

(defun magit-gha-badge-runs-unfold-all ()
  "Unfold every job, step, log and group of this buffer, and each one to come.
A step that unfolds asks GitHub for its log, so this asks for every log.
In `magit-gha-badge-runs-mode-hook' it has each buffer of runs open so."
  (interactive nil magit-gha-badge-runs-mode)
  (when (derived-mode-p 'magit-gha-badge-runs-mode)
    (setq magit-gha-badge-runs--unfold-all t
          magit-section-visibility-cache nil)
    (magit-gha-badge-runs--changed)))

(defun magit-gha-badge-runs--commanded ()
  "Note a command in this buffer: point is the user's from now on."
  (setq magit-gha-badge-runs--auto-point nil))

(defun magit-gha-badge-runs--revert (&rest _)
  "Ask GitHub anew for the content of this buffer, as `revert-buffer' has it."
  (magit-gha-badge-runs--load))

(defvar-keymap magit-gha-badge-runs-mode-map
  :doc "Keymap of `magit-gha-badge-runs-mode'."
  "RET" #'magit-gha-badge-runs-toggle
  "b" #'magit-gha-badge-runs-browse
  "w" #'magit-gha-badge-runs-copy-url
  "t" #'magit-gha-badge-runs-toggle-timestamps
  "C-c C-o" #'magit-gha-badge-runs-browse
  "C-c C-w" #'magit-gha-badge-runs-copy-url
  "C-c C-t" #'magit-gha-badge-runs-toggle-timestamps)

(define-derived-mode magit-gha-badge-runs-mode magit-section-mode "GHA-Runs"
  "Major mode of a buffer of GitHub Actions runs, their jobs, steps and logs.
\\<magit-gha-badge-runs-mode-map>\\[magit-section-toggle] folds the section \
at point, \\[revert-buffer] asks GitHub anew.

\\{magit-gha-badge-runs-mode-map}"
  :group 'magit-gha-badge
  :interactive nil
  (setq magit-gha-badge-runs--jobs (make-hash-table :test 'equal))
  (unless magit-gha-badge-runs--logs
    (setq magit-gha-badge-runs--logs (make-hash-table :test 'equal)))
  (setq-local revert-buffer-function #'magit-gha-badge-runs--revert)
  ;; A redraw has what the user folded and unfolded from this cache alone.
  (setq-local magit-section-cache-visibility t)
  ;; After the visibility that Magit keeps of the sections the user folded.
  (add-hook 'magit-section-set-visibility-hook #'magit-gha-badge-runs--keep-open t t)
  (add-hook 'pre-command-hook #'magit-gha-badge-runs--commanded nil t)
  ;; `magit-gha-badge-runs' starts the mode anew in a buffer that exists.
  (add-hook 'change-major-mode-hook #'magit-gha-badge-runs--stop nil t)
  (add-hook 'kill-buffer-hook #'magit-gha-badge-runs--stop nil t))

;;;; Command

(defun magit-gha-badge-runs--ref-target (ref)
  "Return the target that REF names, nil for no REF.
A full hash gives (sha . REF), any other name (ref . REF)."
  (cond ((null ref) nil)
        ((string-match-p (rx bos (= 40 hex) eos) ref) (cons 'sha ref))
        (t (cons 'ref ref))))

(defconst magit-gha-badge-runs--link-regexp
  (rx bos (? (or "https://" "http://")) (? "www.") "github.com/"
      (group (+ (not (in "/?#")))) "/" (group (+ (not (in "/?#"))))
      (? "/" (group (* nonl))) eos)
  "Regexp of a link into a repository of GitHub: owner, name, the rest.")

(defconst magit-gha-badge-runs--name-regexp
  (rx bos (group (+ (not (in "/@ ")))) "/" (group (+ (not (in "/@ "))))
      (? "@" (group (+ nonl))) eos)
  "Regexp of OWNER/REPO with or without @REF: owner, name, ref.")

(defun magit-gha-badge-runs--parse-link (owner repo rest)
  "Return (OWNER REPO TARGET FOCUS) for REST, the end of a link to OWNER/REPO.
A run, a job of a run, a job by its check, a commit, or the Actions page
with or without a branch; any other link names the repository."
  (pcase rest
    ((rx bos "actions/runs/" (let run (+ digit))
         (? "/attempts/" (+ digit))
         (? "/job/" (let job (+ digit))))
     (list owner repo (cons 'run (string-to-number run))
           (and job (string-to-number job))))
    ((rx bos "runs/" (let job (+ digit)))
     (list owner repo (cons 'job (string-to-number job))))
    ((rx bos "commit/" (let sha (= 40 hex)))
     (list owner repo (cons 'sha sha)))
    ((rx bos "actions" (* nonl) "query=" (* nonl) "branch%3A"
         (let ref (+ (not (in "+&#")))))
     (list owner repo (cons 'ref (url-unhex-string ref))))
    (_ (list owner repo nil))))

(defun magit-gha-badge-runs--parse-input (input)
  "Return (OWNER REPO TARGET FOCUS) that INPUT names on GitHub, or nil.
INPUT is a link, or OWNER/REPO with or without @REF.  TARGET and FOCUS
are as `magit-gha-badge-runs' takes them, or (job . ID) for a job."
  (let ((input (string-trim input)))
    (cond
     ((string-match magit-gha-badge-runs--link-regexp input)
      (magit-gha-badge-runs--parse-link
       (match-string 1 input)
       (string-remove-suffix ".git" (match-string 2 input))
       (or (match-string 3 input) "")))
     ((string-match magit-gha-badge-runs--name-regexp input)
      (list (match-string 1 input) (match-string 2 input)
            (magit-gha-badge-runs--ref-target (match-string 3 input)))))))

(defun magit-gha-badge-runs--commit-at-point ()
  "Return the full hash of the commit at point in a Magit buffer, or nil."
  (and (derived-mode-p 'magit-mode)
       (and-let* ((commit (magit-commit-at-point)))
         (magit-rev-verify (concat commit "^{commit}")))))

(defun magit-gha-badge-runs--upstream-run (repo branch)
  "Return (SLUG . SHA) of the run that the badge of REPO has from an upstream.
That is where a pull request from a fork has its checks.  Nil unless the
run is of BRANCH as pushed now: the badge keeps one run per repository,
of the branch it last asked about."
  (let ((state (magit-gha-badge--state repo)))
    (and-let* ((branch)
               (slug (magit-gha-badge--foreign-repo state))
               (sha (plist-get state :sha))
               (pushed (plist-get state :pushed))
               ((equal pushed (magit-gha-badge--pushed-sha branch))))
      (cons slug sha))))

(defun magit-gha-badge-runs--repo-context ()
  "Return (OWNER REPO TARGET FOCUS) of the repository this buffer is in, or nil.
A job line of the badge names its run and job, a commit at point its
hash, and else the branch its newest runs, as the badge has them.  A
detached HEAD names its commit; a remote off GitHub is refused."
  (when-let* ((repo (magit-toplevel)))
    (if-let* ((link (get-text-property (pos-bol) 'magit-gha-badge-url)))
        (magit-gha-badge-runs--parse-input link)
      (let* ((branch (magit-get-current-branch))
             (slug (magit-gha-badge--slug (magit-gha-badge--remote-url branch)))
             (upstream (magit-gha-badge-runs--upstream-run repo branch))
             (commit (magit-gha-badge-runs--commit-at-point)))
        (unless slug
          (user-error "No github remote to look up runs on"))
        (pcase-let ((`(,owner ,name)
                     (split-string (or (car upstream) slug) "/")))
          (list owner name
                (cond (commit (cons 'sha commit))
                      (upstream (cons 'sha (cdr upstream)))
                      (branch (cons 'ref branch))
                      (t (cons 'sha (magit-rev-verify "HEAD"))))))))))

(defun magit-gha-badge-runs--context ()
  "Return (OWNER REPO TARGET FOCUS) for the buffer the command was called in.
A buffer of runs names its own, any other buffer the repository it is in.
Nil outside a repository."
  (if (derived-mode-p 'magit-gha-badge-runs-mode)
      (list magit-gha-badge-runs--owner magit-gha-badge-runs--repo
            magit-gha-badge-runs--target magit-gha-badge-runs--focus)
    (magit-gha-badge-runs--repo-context)))

(defvar magit-gha-badge-runs--history nil
  "Minibuffer history of what `magit-gha-badge-runs' read.")

(defun magit-gha-badge-runs--read (ask)
  "Return the arguments of `magit-gha-badge-runs', from this buffer or read.
ASK, or a buffer that names no repository, reads a repository or a link."
  (or (and (not ask) (magit-gha-badge-runs--context))
      (let ((input (read-string
                    "Repository (OWNER/REPO, with @REF or not) or link of a run: "
                    nil 'magit-gha-badge-runs--history)))
        (or (magit-gha-badge-runs--parse-input input)
            (user-error "%s names no repository or run of GitHub" input)))))

(defun magit-gha-badge-runs--resolve (owner repo target focus)
  "Return (TARGET FOCUS) with a TARGET that names a job turned into its run.
A link to a check names only the job of OWNER/REPO: GitHub is asked for
the run, and the job is what FOCUS then names."
  (pcase target
    (`(job . ,id)
     (let* ((resource (format "/repos/%s/%s/actions/jobs/%s" owner repo id))
            (auth magit-gha-badge-auth)
            (job (ghub-request "GET" resource nil :auth auth :silent t)))
       (list (cons 'run (alist-get 'run_id job)) id)))
    (_ (list target focus))))

(defun magit-gha-badge-runs--buffer-name (owner repo target)
  "Return the name of the buffer for TARGET of OWNER/REPO."
  (format "*magit-gha: %s/%s%s*" owner repo
          (pcase target
            (`(run . ,id) (format "#%s" id))
            (`(,_ . ,name) (concat "@" name))
            (_ ""))))

;;;###autoload
(defun magit-gha-badge-runs (owner repo &optional target focus)
  "Show GitHub Actions for OWNER/REPO: each workflow run, its jobs and logs.
TARGET is (run . ID), (sha . HASH), (ref . NAME) or nil, the newest commit
with runs; FOCUS is a job to open.  Interactively the buffer at hand names
them, a repository by its branch; a prefix argument reads them."
  (interactive (magit-gha-badge-runs--read current-prefix-arg))
  (pcase-let* ((`(,target ,focus)
                (magit-gha-badge-runs--resolve owner repo target focus))
               (buffer (get-buffer-create
                        (magit-gha-badge-runs--buffer-name owner repo target))))
    (with-current-buffer buffer
      ;; Anew in a buffer that exists too: the hook of the mode is where a
      ;; configuration says how a buffer of runs opens.
      (magit-gha-badge-runs-mode)
      (setq magit-gha-badge-runs--owner owner
            magit-gha-badge-runs--repo repo
            magit-gha-badge-runs--target target
            magit-gha-badge-runs--focus focus
            magit-gha-badge-runs--auto-point t)
      (magit-gha-badge-runs--load)
      (magit-gha-badge-runs--flush))
    (pop-to-buffer buffer)
    buffer))

(defun magit-gha-badge-runs-unload-function ()
  "Cancel the timers of every `magit-gha-badge-runs-mode' buffer.
None may outlive the code it calls."
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when (derived-mode-p 'magit-gha-badge-runs-mode)
        (magit-gha-badge-runs--stop))))
  nil)

(provide 'magit-gha-badge-runs)

;; Local Variables:
;; package-lint-main-file: "magit-gha-badge.el"
;; End:
;;; magit-gha-badge-runs.el ends here
