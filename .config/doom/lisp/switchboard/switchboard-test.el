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
          (switchboard-test--notified nil))
     (cl-letf (((symbol-function 'switchboard--schedule-render) #'ignore)
               ((symbol-function 'switchboard--save-soon) #'ignore)
               ((symbol-function 'switchboard--notify)
                (lambda (s status)
                  (push (cons (switchboard--title s) status) switchboard-test--notified)))
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

(ert-deftest switchboard-test-same-status-notifies-once ()
  (switchboard-test--with-session s
    (switchboard-test--hook "PermissionRequest" '((tool_name . "Bash")))
    (switchboard-test--hook "PermissionRequest" '((tool_name . "Bash")))
    (should (= (length switchboard-test--notified) 1))))

(ert-deftest switchboard-test-changes-counts-lines-and-new-files ()
  (let ((dir (file-name-as-directory (make-temp-file "switchboard-git" t))))
    (unwind-protect
        (let ((default-directory dir))
          (should-not (switchboard--changes dir))   ; not a repository
          (call-process "git" nil nil nil "init" "-q")
          (with-temp-file "f" (insert "a\nb\nc\n"))
          (call-process "git" nil nil nil "add" "f")
          (call-process "git" nil nil nil "-c" "user.name=t" "-c" "user.email=t@t"
                        "commit" "-qm" "init")
          (should-not (switchboard--changes dir))
          (with-temp-file "f" (insert "a\nX\nY\nZ\n"))
          (should (equal (switchboard--changes dir) '(3 2 0)))
          (with-temp-file "new" (insert "x"))
          (should (equal (switchboard--changes dir) '(3 2 1))))
      (delete-directory dir t))))

(provide 'switchboard-test)
;;; switchboard-test.el ends here
