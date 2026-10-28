;;; switchboard-test.el --- Tests for Switchboard's hook handling -*- lexical-binding: t; -*-

;; Run with ./test.sh, which puts Doom's packages on the load path.
;; These feed `switchboard-hook' the events Claude Code and Codex send and
;; check the state a session ends up in, without a terminal or a real agent.

;;; Code:

(require 'ert)
(require 'switchboard)

(defvar switchboard-test--notified nil
  "(TITLE . STATUS) for each notification raised, newest first.")

(defmacro switchboard-test--with-session (var &rest body)
  "Run BODY with VAR bound to a fresh session that is the only one listed.
Rendering, saving and notifying are stubbed out; notifications are
recorded in `switchboard-test--notified'."
  (declare (indent 1))
  `(let* ((dir (file-name-as-directory (make-temp-file "switchboard-test" t)))
          (,var (switchboard--session-create :id "heron-0001" :name "heron"
                                             :harness 'claude :root dir :dir dir))
          (switchboard--sessions (list ,var))
          (switchboard-test--notified nil)
          (switchboard-remind-after nil))
     (cl-letf (((symbol-function 'switchboard--schedule-render) #'ignore)
               ((symbol-function 'switchboard--save-soon) #'ignore)
               ((symbol-function 'switchboard--notify)
                (lambda (s status &optional again)
                  (push (cons (switchboard--title s) (if again (list status 'again) status))
                        switchboard-test--notified)))
               ((symbol-function 'message) #'ignore))
       (unwind-protect (progn ,@body)
         (delete-directory dir t)))))

(defun switchboard-test--hook (event &optional payload)
  "Send EVENT for the test session, with PAYLOAD (an alist) as its JSON."
  (let ((file (and payload (make-temp-file "switchboard-payload" nil ".json"))))
    (when file
      (with-temp-file file (insert (json-serialize payload))))
    (switchboard-hook "heron-0001" event file)
    (when file
      (should-not (file-exists-p file)))))   ; the hook deletes it

(ert-deftest switchboard-test-prompt-starts-work ()
  (switchboard-test--with-session s
    (switchboard-test--hook "UserPromptSubmit" '((prompt . "Fix the login bug")))
    (should (eq (switchboard-session-status s) 'working))
    (should (equal (switchboard-session-task s) "Fix the login bug"))))

(ert-deftest switchboard-test-first-prompt-is-the-task ()
  (switchboard-test--with-session s
    (switchboard-test--hook "UserPromptSubmit" '((prompt . "Fix the login bug")))
    (switchboard-test--hook "Stop")
    (switchboard-test--hook "UserPromptSubmit" '((prompt . "Now add a test")))
    (should (equal (switchboard-session-task s) "Fix the login bug"))))

(ert-deftest switchboard-test-permission-request-waits-with-detail ()
  (switchboard-test--with-session s
    (switchboard-test--hook "UserPromptSubmit" '((prompt . "go")))
    (switchboard-test--hook "PermissionRequest"
                            '((tool_name . "Bash") (tool_input . ((command . "npm test")))))
    (should (eq (switchboard-session-status s) 'waiting))
    (should (equal (switchboard-session-detail s) "Bash: npm test"))
    (should (equal (car switchboard-test--notified) '("heron" . waiting)))))

(ert-deftest switchboard-test-tool-use-after-waiting-resumes-work ()
  (switchboard-test--with-session s
    (switchboard-test--hook "PermissionRequest"
                            '((tool_name . "Edit") (tool_input . ((file_path . "/a.el")))))
    (switchboard-test--hook "PostToolUse")
    (should (eq (switchboard-session-status s) 'working))
    (should-not (switchboard-session-detail s))))

(ert-deftest switchboard-test-stop-finishes-unread ()
  (switchboard-test--with-session s
    (switchboard-test--hook "UserPromptSubmit" '((prompt . "go")))
    (switchboard-test--hook "Stop")
    (should (eq (switchboard-session-status s) 'ready))
    ;; Nobody had it on screen, so it's new to you.
    (should (switchboard-session-unread s))
    (should (equal (car switchboard-test--notified) '("heron" . ready)))))

(ert-deftest switchboard-test-new-prompt-clears-unread ()
  (switchboard-test--with-session s
    (switchboard-test--hook "Stop")
    (switchboard-test--hook "UserPromptSubmit" '((prompt . "more")))
    (should-not (switchboard-session-unread s))))

(ert-deftest switchboard-test-stop-failure-is-an-error ()
  (switchboard-test--with-session s
    (switchboard-test--hook "UserPromptSubmit" '((prompt . "go")))
    (switchboard-test--hook "StopFailure" '((error . "rate_limit")))
    (should (eq (switchboard-session-status s) 'error))
    (should (equal (switchboard-session-detail s) "rate_limit"))))

(ert-deftest switchboard-test-stop-failure-without-text ()
  (switchboard-test--with-session s
    (switchboard-test--hook "StopFailure" '((error . ((code . 500)))))
    (should (equal (switchboard-session-detail s) "API error"))))

(ert-deftest switchboard-test-session-start-keeps-resume-id ()
  (switchboard-test--with-session s
    (switchboard-test--hook "SessionStart" '((session_id . "abc-123")))
    (should (equal (switchboard-session-resume-id s) "abc-123"))))

(ert-deftest switchboard-test-codex-notify-finishes ()
  (switchboard-test--with-session s
    (setf (switchboard-session-harness s) 'codex)
    (switchboard--set s 'working)
    (switchboard-test--hook "notify")
    (should (eq (switchboard-session-status s) 'ready))))

(ert-deftest switchboard-test-unknown-session-is-ignored ()
  (switchboard-test--with-session s
    (switchboard-hook "nobody-0000" "Stop" nil)
    (should (eq (switchboard-session-status s) 'ready))
    (should-not switchboard-test--notified)))

(ert-deftest switchboard-test-crash-notifies ()
  (switchboard-test--with-session s
    (let ((buf (generate-new-buffer " *switchboard-test*"))
          (switchboard-notify '(exited)))
      (with-current-buffer buf (setq switchboard--session s))
      (cl-letf (((symbol-function 'switchboard--notify)
                 (lambda (s status &rest _)
                   (when (memq status switchboard-notify)
                     (push (cons (switchboard--title s) status) switchboard-test--notified)))))
        (switchboard--on-exit buf "exited abnormally with code 1\n"))
      (kill-buffer buf)
      (should (eq (switchboard-session-status s) 'exited))
      (should (equal (switchboard-session-detail s) "exited abnormally with code 1"))
      (should (equal switchboard-test--notified '(("heron" . exited)))))))

(ert-deftest switchboard-test-clean-exit-is-quiet ()
  (switchboard-test--with-session s
    (let ((buf (generate-new-buffer " *switchboard-test*"))
          (switchboard-notify '(exited)))
      (with-current-buffer buf (setq switchboard--session s))
      (cl-letf (((symbol-function 'switchboard--notify)
                 (lambda (s status &rest _)
                   (when (memq status switchboard-notify)
                     (push (cons (switchboard--title s) status) switchboard-test--notified)))))
        (switchboard--on-exit buf "finished\n"))
      (kill-buffer buf)
      (should (eq (switchboard-session-status s) 'exited))
      (should-not switchboard-test--notified))))

(ert-deftest switchboard-test-waiting-reminds-until-answered ()
  (switchboard-test--with-session s
    (let ((switchboard-remind-after 600))
      (switchboard-test--hook "PermissionRequest" '((tool_name . "Bash")))
      (let ((timer (gethash "heron-0001" switchboard--reminders)))
        (should timer)
        (funcall (timer--function timer) s))          ; ten minutes pass
      (should (equal (car switchboard-test--notified) '("heron" waiting again)))
      (should (gethash "heron-0001" switchboard--reminders))   ; and again later
      (switchboard-test--hook "PostToolUse")         ; you answered
      (should-not (gethash "heron-0001" switchboard--reminders)))))

(ert-deftest switchboard-test-same-status-notifies-once ()
  (switchboard-test--with-session s
    (switchboard-test--hook "PermissionRequest" '((tool_name . "Bash")))
    (switchboard-test--hook "PermissionRequest" '((tool_name . "Bash")))
    (should (= (length switchboard-test--notified) 1))))

(defun switchboard-test--refresh (s)
  "Run `switchboard--update-git' for S and wait for it to finish."
  (switchboard--update-git s)
  (while (gethash (switchboard-session-id s) switchboard--git-procs)
    (accept-process-output nil 0.05)))

(ert-deftest switchboard-test-git-reads-branch-and-changes ()
  (switchboard-test--with-session s
    (let ((default-directory (switchboard-session-dir s)))
      (switchboard-test--refresh s)               ; not a repository
      (should-not (switchboard-session-branch s))
      (should-not (switchboard-session-changes s))
      (call-process "git" nil nil nil "init" "-q" "-b" "main")
      (with-temp-file "f" (insert "a\nb\nc\n"))
      (call-process "git" nil nil nil "add" "f")
      (call-process "git" nil nil nil "-c" "user.name=t" "-c" "user.email=t@t"
                    "commit" "-qm" "init")
      (switchboard-test--refresh s)
      (should (equal (switchboard-session-branch s) "main"))
      (should-not (switchboard-session-changes s))
      (with-temp-file "f" (insert "a\nX\nY\nZ\n"))
      (switchboard-test--refresh s)
      (should (equal (switchboard-session-changes s) '(3 2 0)))
      (with-temp-file "new" (insert "x"))
      (switchboard-test--refresh s)
      (should (equal (switchboard-session-changes s) '(3 2 1))))))

(ert-deftest switchboard-test-tool-calls-refresh-git-once ()
  (switchboard-test--with-session s
    (let ((runs 0))
      (cl-letf (((symbol-function 'switchboard--update-git) (lambda (_) (cl-incf runs))))
        (dotimes (_ 5) (switchboard-test--hook "PostToolUse"))
        (should (= runs 0))                      ; not while edits keep coming
        (funcall (timer--function (gethash "heron-0001" switchboard--git-timers)))
        (should (= runs 1))
        (should-not (gethash "heron-0001" switchboard--git-timers))))))

(ert-deftest switchboard-test-migrate-rebuilds-old-records ()
  (switchboard-test--with-session s
    ;; Pretend the old layout lacked `detail': a record one slot shorter.
    (let* ((slots (switchboard--slots))
           (old-slots (remq 'detail slots))
           (old (apply #'record 'switchboard-session
                       (mapcar (lambda (slot)
                                 (pcase slot ('name "wren") ('status 'waiting) (_ nil)))
                               (cdr old-slots))))
           (buf (generate-new-buffer " *switchboard-test*")))
      (setq switchboard--sessions (list old))
      (with-current-buffer buf (setq switchboard--session old))
      (unwind-protect
          (progn
            (switchboard--migrate-sessions old-slots)
            (let ((new (car switchboard--sessions)))
              (should (= (length new) (length slots)))
              (should (equal (switchboard-session-name new) "wren"))
              (should (eq (switchboard-session-status new) 'waiting))
              (should-not (switchboard-session-detail new))
              (should (eq (buffer-local-value 'switchboard--session buf) new))))
        (kill-buffer buf)))))

(ert-deftest switchboard-test-parse-git-handles-binary-files ()
  (should (equal (switchboard--parse-git "sb/heron\n@@\n-\t-\timg.png\n4\t1\ta.el\n@@\n")
                 '("sb/heron" 4 1 0))))

(provide 'switchboard-test)
;;; switchboard-test.el ends here
