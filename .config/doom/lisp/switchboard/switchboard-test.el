;;; switchboard-test.el --- Tests for Switchboard's hook handling -*- lexical-binding: t; -*-

;; Run with ./test.sh, which puts Doom's packages on the load path.
;; These feed `switchboard-hook' the events Claude Code, Codex and Pi send and
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
          (switchboard-remind-after nil)
          ;; A git or gh run left by an earlier test, under the same id,
          ;; would swallow this test's.
          (switchboard--git-procs (make-hash-table :test 'equal))
          (switchboard--git-timers (make-hash-table :test 'equal))
          (switchboard--pr-procs (make-hash-table :test 'equal))
          (switchboard--pr-checked (make-hash-table :test 'equal))
          (switchboard--pending-agents (make-hash-table :test 'equal)))
     (cl-letf (((symbol-function 'switchboard--schedule-render) #'ignore)
               ((symbol-function 'switchboard--save-soon) #'ignore)
               ((symbol-function 'switchboard--notify)
                (lambda (s status &optional again)
                  (push (cons (switchboard--title s) (if again (list status 'again) status))
                        switchboard-test--notified)))
               ((symbol-function 'message) #'ignore))
       (unwind-protect (progn ,@body)
         ;; Stop git and gh runs this test started.  Left going, they end
         ;; in a later test, whose session has the same id, and their
         ;; sentinels clear its run from the table it waits on.
         (dolist (table (list switchboard--git-procs switchboard--pr-procs))
           (maphash (lambda (_ proc)
                      (when (processp proc)
                        (set-process-sentinel proc #'ignore)
                        (delete-process proc)
                        (when (buffer-live-p (process-buffer proc))
                          (kill-buffer (process-buffer proc)))))
                    table))
         (maphash (lambda (_ timer) (cancel-timer timer)) switchboard--git-timers)
         (delete-directory dir t)))))

(defun switchboard-test--hook (event &optional payload)
  "Send EVENT for the test session, with PAYLOAD (an alist) as its JSON."
  (let ((file (and payload (make-temp-file "switchboard-payload" nil ".json"))))
    (when file
      (with-temp-file file (insert (json-serialize payload))))
    (switchboard-hook "heron-0001" event file)
    (when file
      (should-not (file-exists-p file)))))   ; the hook deletes it

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

;; Codex: its hooks, once trusted, and notify until then

(defmacro switchboard-test--with-codex (var &rest body)
  "Like `switchboard-test--with-session', for a Codex session."
  (declare (indent 1))
  `(switchboard-test--with-session ,var
     (let* ((switchboard--codex-hooked (make-hash-table :test 'equal))
            (switchboard--codex-turns (make-hash-table :test 'equal))
            (switchboard--codex-sizes (make-hash-table :test 'equal))
            (home (make-temp-file "switchboard-codex" t))
            ;; As if already started, so none starts; tests check by hand.
            (switchboard--codex-watch-timer t)
            (process-environment (cons (concat "CODEX_HOME=" home) process-environment)))
       (setf (switchboard-session-harness ,var) 'codex)
       (unwind-protect (progn ,@body)
         (delete-directory home t)))))

(defun switchboard-test--rollout (home thread &optional meta &rest lines)
  "Write THREAD's rollout under HOME with META first and LINES after; return it."
  (let ((file (expand-file-name (format "sessions/2026/11/01/rollout-x-%s.jsonl" thread) home)))
    (make-directory (file-name-directory file) t)
    (with-temp-file file
      (insert (or meta "{\"type\":\"session_meta\",\"payload\":{\"id\":\"x\"}}") "\n")
      (dolist (line lines) (insert line "\n")))
    file))

(ert-deftest switchboard-test-codex-without-hooks-notify-ends-the-turn ()
  (switchboard-test--with-codex s
    (switchboard-test--rollout home "t1")
    (switchboard--set s 'working)
    (switchboard-test--hook "notify" '((thread-id . "t1")))
    (should (eq (switchboard-session-status s) 'ready))
    (should (equal (switchboard-session-resume-id s) "t1"))))

(ert-deftest switchboard-test-codex-without-hooks-notify-skips-other-threads ()
  ;; Both fire before the turn ends: the thread writing the title, which
  ;; has no rollout, and a subagent's, whose rollout names its parent.
  (switchboard-test--with-codex s
    (switchboard-test--rollout
     home "child"
     "{\"type\":\"session_meta\",\"payload\":{\"id\":\"child\",\"parent_thread_id\":\"t1\"}}")
    (switchboard--set s 'working)
    (switchboard-test--hook "notify" '((thread-id . "title")))
    (switchboard-test--hook "notify" '((thread-id . "child")))
    (should (eq (switchboard-session-status s) 'working))
    (should-not (switchboard-session-resume-id s))))

(ert-deftest switchboard-test-codex-notify-does-not-end-a-hooked-turn ()
  ;; Stop found an agent still running; notify, right behind it, can't tell.
  (switchboard-test--with-codex s
    (switchboard-test--hook "SessionStart" '((session_id . "t1") (source . "startup")))
    (switchboard-test--hook "SubagentStart" '((agent_id . "a1") (agent_type . "default")))
    (switchboard-test--hook "Stop" '((session_id . "t1")))
    (should (eq (switchboard-session-status s) 'agents))
    (switchboard-test--hook "notify" '((thread-id . "t1")))
    (should (eq (switchboard-session-status s) 'agents))))

(ert-deftest switchboard-test-codex-return-means-working-only-without-hooks ()
  (switchboard-test--with-codex s
    (let ((switchboard--session s))
      (switchboard--note-input 'return)
      (should (eq (switchboard-session-status s) 'working))
      (switchboard-test--hook "Stop" '((session_id . "t1")))
      (should (eq (switchboard-session-status s) 'ready))
      ;; Now UserPromptSubmit says so; RET may only have answered a menu.
      (switchboard--note-input 'return)
      (should (eq (switchboard-session-status s) 'ready)))))

(ert-deftest switchboard-test-codex-interrupt-stops ()
  (switchboard-test--with-codex s
    (switchboard-test--hook "UserPromptSubmit" '((prompt . "go")))
    (switchboard-test--hook "Interrupt" '((session_id . "t1")))
    (should (eq (switchboard-session-status s) 'stopped))))

;; The sequences below are as Codex 0.166 sent them, cut down.

(ert-deftest switchboard-test-codex-background-agents-end-ready ()
  (switchboard-test--with-codex s
    (let ((spawn (lambda (task)
                   (dolist (event '("PreToolUse" "PostToolUse"))
                     (switchboard-test--hook
                      event `((session_id . "t1") (tool_name . "collaborationspawn_agent")
                              (tool_input . ((agent_type . :null) (task_name . ,task))))))))
          (started (lambda (id)
                     (switchboard-test--hook
                      "SubagentStart" `((session_id . "t1") (agent_id . ,id) (agent_type . "default")))))
          (stopped (lambda (id)
                     (switchboard-test--hook
                      "SubagentStop" `((session_id . "t1") (agent_id . ,id) (agent_type . "default"))))))
      (switchboard-test--hook "SessionStart" '((session_id . "t1") (source . "startup")))
      (switchboard-test--hook "UserPromptSubmit" '((session_id . "t1") (prompt . "go")))
      (funcall spawn "sleep_a")
      (funcall started "a1")
      (funcall spawn "echo_b")
      (switchboard-test--hook "PreToolUse" '((session_id . "t1") (agent_id . "a1")
                                             (agent_type . "default") (tool_name . "Bash")))
      (funcall started "a2")
      (should (equal (mapcar #'switchboard-agent-description (switchboard-session-agents s))
                     '("sleep a" "echo b")))
      (should (equal (switchboard-agent-tool (car (switchboard-session-agents s))) "Bash"))
      (should (equal (switchboard--agent-name (car (switchboard-session-agents s))) "general"))
      (switchboard-test--hook "Stop" '((session_id . "t1")))
      (should (eq (switchboard-session-status s) 'agents))
      (funcall stopped "a2")
      (should (eq (switchboard-session-status s) 'agents))
      (funcall stopped "a1")
      ;; No turn follows: Codex keeps their results for your next prompt.
      (should (eq (switchboard-session-status s) 'ready))
      (should (equal (car switchboard-test--notified) '("heron" . ready))))))

(ert-deftest switchboard-test-codex-interrupt-leaves-agents-running ()
  (switchboard-test--with-codex s
    (switchboard-test--hook "UserPromptSubmit" '((session_id . "t1") (prompt . "go")))
    (switchboard-test--hook "SubagentStart" '((session_id . "t1") (agent_id . "a1")
                                              (agent_type . "default")))
    (switchboard-test--hook "Interrupt" '((session_id . "t1")))
    (should (eq (switchboard-session-status s) 'stopped))
    (should (= (switchboard--live-agents s) 1))
    (switchboard-test--hook "SubagentStop" '((session_id . "t1") (agent_id . "a1")
                                             (agent_type . "default")))
    (should (= (switchboard--live-agents s) 0))
    (should (eq (switchboard-session-status s) 'stopped))))

(ert-deftest switchboard-test-codex-permission-needs-you ()
  (switchboard-test--with-codex s
    (switchboard-test--hook "UserPromptSubmit" '((session_id . "t1") (prompt . "go")))
    (switchboard-test--hook "PermissionRequest"
                            '((session_id . "t1") (tool_name . "Bash")
                              (tool_input . ((command . "curl example.com")))))
    (should (eq (switchboard-session-status s) 'waiting))
    (should (equal (switchboard-session-detail s) "Bash: curl example.com"))
    (should (equal (car switchboard-test--notified) '("heron" . waiting)))))

(ert-deftest switchboard-test-codex-hooks-trusted-mid-session-give-the-resume-id ()
  (switchboard-test--with-codex s
    (switchboard-test--hook "UserPromptSubmit" '((session_id . "t1") (prompt . "go")))
    (should (equal (switchboard-session-resume-id s) "t1"))))

(ert-deftest switchboard-test-codex-agent-prompt-survives-parent-polling ()
  ;; The parent's wait_agent call ended while its agent waited on you.
  (switchboard-test--with-codex s
    (let ((agent '((session_id . "t1") (agent_id . "a1") (agent_type . "default")))
          (poll '((session_id . "t1") (tool_name . "collaborationwait_agent"))))
      (switchboard-test--hook "UserPromptSubmit" '((session_id . "t1") (prompt . "go")))
      (switchboard-test--hook "SubagentStart" agent)
      (switchboard-test--hook "PermissionRequest"
                              (append agent '((tool_name . "Bash")
                                              (tool_input . ((command . "curl example.com"))))))
      (should (eq (switchboard-session-status s) 'waiting))
      (should (equal (switchboard-session-detail s) "general · Bash: curl example.com"))
      (switchboard-test--hook "PostToolUse" poll)
      (switchboard-test--hook "PreToolUse" poll)
      (should (eq (switchboard-session-status s) 'waiting))
      ;; You answered, and the agent's tool ran.
      (switchboard-test--hook "PostToolUse" (append agent '((tool_name . "Bash"))))
      (should (eq (switchboard-session-status s) 'working)))))

(ert-deftest switchboard-test-codex-stop-keeps-an-agents-prompt ()
  (switchboard-test--with-codex s
    (let ((agent '((session_id . "t1") (agent_id . "a1") (agent_type . "default"))))
      (switchboard-test--hook "UserPromptSubmit" '((session_id . "t1") (prompt . "go")))
      (switchboard-test--hook "SubagentStart" agent)
      (switchboard-test--hook "PermissionRequest" (append agent '((tool_name . "Bash"))))
      (switchboard-test--hook "Stop" '((session_id . "t1")))
      (should (eq (switchboard-session-status s) 'waiting))
      (should (switchboard-session-detail s))
      (switchboard-test--hook "PostToolUse" (append agent '((tool_name . "Bash"))))
      (should (eq (switchboard-session-status s) 'agents))
      (switchboard-test--hook "SubagentStop" agent)
      (should (eq (switchboard-session-status s) 'ready)))))

(ert-deftest switchboard-test-agent-ending-at-its-prompt-frees-the-session ()
  (switchboard-test--with-codex s
    (let ((agent '((session_id . "t1") (agent_id . "a1") (agent_type . "default"))))
      (switchboard-test--hook "UserPromptSubmit" '((session_id . "t1") (prompt . "go")))
      (switchboard-test--hook "SubagentStart" agent)
      (switchboard-test--hook "PermissionRequest" (append agent '((tool_name . "Bash"))))
      (switchboard-test--hook "SubagentStop" agent)
      (should (eq (switchboard-session-status s) 'working)))))

(ert-deftest switchboard-test-codex-esc-at-an-agents-prompt-ends-it ()
  ;; Codex aborts its turn and sends no SubagentStop.
  (switchboard-test--with-codex s
    (let ((agent '((session_id . "t1") (agent_id . "a1") (agent_type . "default"))))
      (switchboard-test--hook "UserPromptSubmit" '((session_id . "t1") (prompt . "go")))
      (switchboard-test--hook "SubagentStart" agent)
      (switchboard-test--hook "PermissionRequest" (append agent '((tool_name . "Bash"))))
      (switchboard-test--hook "Interrupt" '((session_id . "t1")))
      (should (eq (switchboard-session-status s) 'stopped))
      (should (= (switchboard--live-agents s) 0)))))

(ert-deftest switchboard-test-codex-failed-turn-is-an-error ()
  (switchboard-test--with-codex s
    (let* ((failed (lambda (turn)
                     (format "{\"type\":\"event_msg\",\"payload\":{\"type\":\"task_complete\",\"turn_id\":\"%s\",\"error\":{\"message\":\"bad model\",\"codex_error_info\":\"model_not_found\"}}}" turn)))
           (rollout (switchboard-test--rollout home "t1" nil (funcall failed "old"))))
      (switchboard-test--hook "UserPromptSubmit"
                              `((session_id . "t1") (turn_id . "new") (prompt . "go")
                                (transcript_path . ,rollout)))
      ;; The record it finds is the last prompt's: this turn still runs.
      (should (switchboard--codex-check-errors))
      (should (eq (switchboard-session-status s) 'working))
      (switchboard-test--rollout home "t1" nil (funcall failed "old") (funcall failed "new"))
      (should-not (switchboard--codex-check-errors))
      (should (eq (switchboard-session-status s) 'error))
      (should (equal (switchboard-session-detail s) "bad model"))
      (should (equal (car switchboard-test--notified) '("heron" . error))))))

(ert-deftest switchboard-test-codex-finished-turn-is-no-error ()
  (switchboard-test--with-codex s
    (let ((rollout (switchboard-test--rollout
                    home "t1" nil
                    "{\"type\":\"event_msg\",\"payload\":{\"type\":\"task_complete\",\"turn_id\":\"u\",\"error\":null}}")))
      (should-not (switchboard--codex-turn-error rollout "u")))))

(ert-deftest switchboard-test-codex-command ()
  (switchboard-test--with-codex s
    (let ((new (switchboard--command s "go" nil)))
      (should (equal (seq-take new 2) '("codex" "--no-daemon")))
      (should (equal (car (last new)) "go"))
      (should (cl-some (lambda (a) (string-match-p "\\`hooks\\.Stop=.*codex-hook Stop" a)) new))
      (should (cl-some (lambda (a) (string-prefix-p "notify=" a)) new)))
    (should (equal (seq-take (switchboard--command s nil t) 3) '("codex" "resume" "--last")))
    (setf (switchboard-session-resume-id s) "t1")
    (should (equal (seq-take (switchboard--command s nil t) 3) '("codex" "resume" "t1")))))

(ert-deftest switchboard-test-codex-tokens-and-title ()
  ;; Records as Codex 0.166 writes them, cut down.
  (switchboard-test--with-codex s
    (let ((rollout (expand-file-name "rollout.jsonl" home)))
      (with-temp-file rollout
        (insert "{\"type\":\"event_msg\",\"payload\":{\"type\":\"token_count\",\"info\":{\"last_token_usage\":{\"input_tokens\":14265}}}}\n"
                "{\"type\":\"event_msg\",\"payload\":{\"type\":\"token_count\",\"info\":null}}\n"))
      (with-temp-file (expand-file-name "session_index.jsonl" home)
        (insert "{\"id\":\"t1\",\"thread_name\":\"Old\",\"updated_at\":\"x\"}\n"
                "{\"id\":\"t2\",\"thread_name\":\"Other\",\"updated_at\":\"x\"}\n"
                "{\"id\":\"t1\",\"thread_name\":\"Fix the parser\",\"updated_at\":\"x\"}\n"))
      (switchboard-test--hook "SessionStart"
                              `((session_id . "t1") (source . "startup")
                                (transcript_path . ,rollout)))
      (should (equal (switchboard-session-tokens s) 14265))
      (should (equal (switchboard-session-title s) "Fix the parser")))))

(ert-deftest switchboard-test-menu-marks-missing-harnesses ()
  (let ((exec-path nil))
    (should (switchboard--harness-missing-p "claude"))
    (should (equal (switchboard--harness-label "Claude" "claude")
                   "Claude (needs installation)")))
  (cl-letf (((symbol-function 'executable-find) (lambda (&rest _) "/bin/claude")))
    (should-not (switchboard--harness-missing-p "claude"))
    (should (equal (switchboard--harness-label "Claude" "claude") "Claude"))))

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

(ert-deftest switchboard-test-pause-stops-the-process-and-stays-paused ()
  (switchboard-test--with-session s
    (let ((buf (generate-new-buffer " *switchboard-test*"))
          (switchboard-notify '(ready error exited)))
      (with-current-buffer buf (setq switchboard--session s))
      (setf (switchboard-session-buffer s) buf)
      ;; Like ghostel's native terminal: the buffer's process is a pipe,
      ;; with the agent's pid kept on it.
      (let ((agent (start-process "switchboard-test" nil "sleep" "60"))
            (pipe (make-pipe-process :name "switchboard-test-pipe" :buffer buf :noquery t)))
        (process-put pipe 'ghostel--native-pid (process-id agent))
        (switchboard-test--hook "Stop")
        (setq switchboard-test--notified nil)
        (switchboard-pause s)
        (should (eq (switchboard-session-status s) 'paused))
        (while (process-live-p agent) (accept-process-output agent 0.05))
        (delete-process pipe)
        ;; The cut-off turn's events and the exit leave it paused, quietly.
        (switchboard-test--hook "StopFailure" '((error . "interrupted")))
        (switchboard--on-exit buf "terminated\n")
        (should (eq (switchboard-session-status s) 'paused))
        (should-not (switchboard-session-detail s))
        (should-not (cl-find-if (lambda (n) (memq (cdr n) switchboard-notify))
                                switchboard-test--notified)))
      (kill-buffer buf))))

(ert-deftest switchboard-test-merged-pull-request-shows-merged-when-idle ()
  (switchboard-test--with-session s
    (setf (switchboard-session-pr s) '(:number 7 :state merged :draft nil))
    (should (eq (switchboard--shown-status s) 'merged))
    (should (equal (switchboard--label s) "MERGED"))
    ;; Exited or paused, it says so, and that it was merged.
    (dolist (status '(exited paused))
      (setf (switchboard-session-status s) status)
      (should (eq (switchboard--shown-status s) status))
      (should (equal (switchboard--label s) (concat (upcase (symbol-name status)) " | MERGED"))))
    ;; Work, a prompt or an unseen turn still show over it.
    (dolist (status '(working waiting error))
      (setf (switchboard-session-status s) status)
      (should (eq (switchboard--shown-status s) status)))
    (setf (switchboard-session-status s) 'ready
          (switchboard-session-unread s) t)
    (should (eq (switchboard--status-key s) 'unread))
    (setf (switchboard-session-unread s) nil
          (switchboard-session-pr s) '(:number 7 :state open :draft t))
    (should (eq (switchboard--pr-state s) 'draft))
    (should (eq (switchboard--shown-status s) 'ready))))

(ert-deftest switchboard-test-new-branch-drops-the-old-pull-request ()
  (switchboard-test--with-session s
    (setf (switchboard-session-branch s) "old"
          (switchboard-session-pr s) '(:number 7 :state merged :draft nil))
    (let ((proc (make-process :name "switchboard-test-git" :command '("true") :sentinel #'ignore
                              :buffer (generate-new-buffer " *switchboard-test*")))
          looked-up)
      (while (process-live-p proc) (accept-process-output proc 0.05))
      (with-current-buffer (process-buffer proc) (insert "new\n@@\n@@\n"))
      (cl-letf (((symbol-function 'switchboard--update-pr)
                 (lambda (s &rest _) (push (switchboard-session-branch s) looked-up))))
        (switchboard--git-done s proc))
      (should (equal (switchboard-session-branch s) "new"))
      (should-not (switchboard-session-pr s))
      (should (equal looked-up '("new"))))))

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

(ert-deftest switchboard-test-ctrl-c-or-escape-stops-a-turn ()
  (dolist (key '("\x03" "\e"))
    (switchboard-test--with-session s
      (with-temp-buffer
        (setq switchboard--session s)
        (switchboard-test--hook "UserPromptSubmit" '((prompt . "go")))
        (switchboard--note-string key)
        (should (eq (switchboard-session-status s) 'stopped))
        (switchboard--note-string key)          ; idle: nothing to stop
        (should (eq (switchboard-session-status s) 'stopped))))))

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

(ert-deftest switchboard-test-follows-the-agent-into-a-worktree ()
  (switchboard-test--with-session s
    (let* ((default-directory (switchboard-session-dir s))
           (tree (concat (directory-file-name default-directory) "-tree/")))
      (call-process "git" nil nil nil "init" "-q" "-b" "main")
      (call-process "git" nil nil nil "-c" "user.name=t" "-c" "user.email=t@t"
                    "commit" "-q" "--allow-empty" "-m" "init")
      (cl-letf (((symbol-function 'switchboard--update-pr) #'ignore))
        (switchboard-test--refresh s)
        (should (equal (switchboard-session-branch s) "main"))
        (should-not (switchboard-session-worktree s))
        (call-process "git" nil nil nil "worktree" "add" "-q" "-b" "fix" tree)
        (unwind-protect
            (progn
              ;; Its next hook event reports the worktree as its folder.
              (switchboard-hook "heron-0001" "Stop"
                                (let ((f (make-temp-file "switchboard-payload")))
                                  (with-temp-file f
                                    (insert (json-serialize `((cwd . ,tree)))))
                                  f))
              (while (gethash (switchboard-session-id s) switchboard--git-procs)
                (accept-process-output nil 0.05))
              (should (equal (switchboard--work-dir s) tree))
              (should (equal (switchboard-session-dir s) default-directory))
              (should (equal (switchboard-session-branch s) "fix"))
              (should (switchboard-session-worktree s)))
          (delete-directory tree t))))))

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

(defmacro switchboard-test--with-worktrees (names &rest body)
  "Run BODY with a scratch repo and a worktree per name in NAMES.
`switchboard-worktree-directory' points at the scratch area; in BODY,
`root' is the repo and `(wt NAME)' a worktree's directory."
  (declare (indent 1))
  `(let* ((top (file-name-as-directory (make-temp-file "switchboard-wt" t)))
          (root (file-name-as-directory (expand-file-name "repo" top)))
          (switchboard-worktree-directory (expand-file-name "wt/" top)))
     (cl-flet ((git (dir &rest args)
                 (let ((default-directory dir))
                   (apply #'call-process "git" nil nil nil
                          "-c" "user.name=t" "-c" "user.email=t@t" args)))
               (wt (name) (file-name-as-directory
                           (expand-file-name (concat "repo/" name)
                                             switchboard-worktree-directory))))
       (unwind-protect
           (progn
             (make-directory root)
             (git root "init" "-q" "-b" "main")
             (git root "commit" "-q" "--allow-empty" "-m" "init")
             (dolist (name ,names) (switchboard--add-worktree root name))
             ,@body)
         (delete-directory top t)))))

(ert-deftest switchboard-test-kill-removes-a-merged-clean-worktree ()
  (switchboard-test--with-worktrees '("wren")
    (let* ((s (switchboard--session-create :id "wren-0001" :name "wren" :root root
                                           :dir (wt "wren") :branch "sb/wren"))
           (switchboard--sessions nil))
      (cl-letf (((symbol-function 'y-or-n-p) (lambda (_) t))
                ((symbol-function 'message) #'ignore))
        (switchboard--offer-worktree-removal s))
      (should-not (file-directory-p (wt "wren")))
      (should-not (switchboard--git root "rev-parse" "--verify" "--quiet" "sb/wren")))))

(ert-deftest switchboard-test-kill-keeps-unmerged-work-without-asking ()
  (switchboard-test--with-worktrees '("wren")
    (git (wt "wren") "commit" "-q" "--allow-empty" "-m" "work")
    (let* ((s (switchboard--session-create :id "wren-0001" :name "wren" :root root
                                           :dir (wt "wren") :branch "sb/wren"))
           (switchboard--sessions nil)
           said)
      (cl-letf (((symbol-function 'y-or-n-p) (lambda (_) (error "Shouldn't ask")))
                ((symbol-function 'message) (lambda (&rest args) (setq said (apply #'format args)))))
        (switchboard--offer-worktree-removal s))
      (should (file-directory-p (wt "wren")))
      (should (string-match-p "unmerged work on sb/wren" said)))))

(ert-deftest switchboard-test-kill-leaves-a-shared-worktree ()
  (switchboard-test--with-worktrees '("wren")
    (let* ((s (switchboard--session-create :id "wren-0001" :name "wren" :root root
                                           :dir (wt "wren")))
           (other (switchboard--session-create :id "kite-0001" :name "kite" :root root
                                               :dir (wt "wren")))
           (switchboard--sessions (list other)))
      (cl-letf (((symbol-function 'y-or-n-p) (lambda (_) (error "Shouldn't ask")))
                ((symbol-function 'message) #'ignore))
        (switchboard--offer-worktree-removal s))
      (should (file-directory-p (wt "wren"))))))

(ert-deftest switchboard-test-prune-sorts-merged-unmerged-and-dirty ()
  (switchboard-test--with-worktrees '("a" "b" "c")
    (git (wt "b") "commit" "-q" "--allow-empty" "-m" "work")
    (with-temp-file (expand-file-name "new" (wt "c")) (insert "x"))
    (let ((switchboard--sessions nil) asked)
      (should (equal (mapcar #'cddr (switchboard--orphan-worktrees))
                     '(("sb/a" t nil) ("sb/b" nil nil) ("sb/c" t t))))
      (cl-letf (((symbol-function 'completing-read-multiple)
                 (lambda (_ choices &rest _) (mapcar #'car choices)))
                ((symbol-function 'yes-or-no-p) (lambda (p) (push p asked) t))
                ((symbol-function 'y-or-n-p) (lambda (p) (push p asked) nil))
                ((symbol-function 'message) #'ignore))
        (switchboard-prune-worktrees))
      (should-not (switchboard--orphan-worktrees))
      ;; Asked about c's changes and b's unmerged branch, which it kept.
      (should (= (length asked) 2))
      (should (switchboard--git root "rev-parse" "--verify" "--quiet" "sb/b"))
      (should-not (switchboard--git root "rev-parse" "--verify" "--quiet" "sb/a")))))

(ert-deftest switchboard-test-groups-keep-your-order ()
  (let* ((mk (lambda (name root)
               (switchboard--session-create :id name :name name :root root :dir root)))
         (switchboard--sessions (list (funcall mk "a" "/p/api/") (funcall mk "b" "/p/web/")
                                      (funcall mk "c" "/p/infra/") (funcall mk "d" "/p/api/")))
         (switchboard--group 'project)
         (switchboard--filter nil)
         (switchboard--group-order nil)
         (keys (lambda () (mapcar #'car (switchboard--groups)))))
    (should (equal (funcall keys) '("api" "web" "infra")))   ; as the sessions started
    (cl-letf (((symbol-function 'switchboard--render) #'ignore)
              ((symbol-function 'switchboard--at-point)
               (lambda () (car (last switchboard--sessions 2)))))   ; on infra's session
      (switchboard-move-group-up)
      (should (equal (funcall keys) '("api" "infra" "web")))
      (switchboard-move-group-up 5)                             ; stops at the top
      (should (equal (funcall keys) '("infra" "api" "web")))
      (switchboard-move-group-down)
      (should (equal (funcall keys) '("api" "infra" "web"))))
    ;; A new project goes after the ones you placed.
    (setq switchboard--sessions (append switchboard--sessions
                                        (list (funcall mk "e" "/p/docs/"))))
    (should (equal (funcall keys) '("api" "infra" "web" "docs")))))

(ert-deftest switchboard-test-recently-closed ()
  (switchboard-test--with-session s
    (let ((switchboard--recent nil)
          resumed)
      (cl-letf (((symbol-function 'switchboard--render) #'ignore)
                ((symbol-function 'switchboard-resume) (lambda (x) (setq resumed x))))
        ;; At most ten, newest first, each session once.
        (dotimes (i 12)
          (switchboard--remember-closed
           (switchboard--session-create :id (format "s-%d" i) :name "x"
                                        :root (switchboard-session-root s)
                                        :dir (switchboard-session-dir s))))
        (switchboard--remember-closed
         (switchboard--session-create :id "s-5" :name "again"
                                      :root (switchboard-session-root s)
                                      :dir (switchboard-session-dir s)))
        (should (= (length switchboard--recent) 10))
        (should (equal (plist-get (car switchboard--recent) :name) "again"))
        (should (= 1 (cl-count "s-5" switchboard--recent
                               :key (lambda (e) (plist-get e :id)) :test #'equal)))
        ;; Reopening makes the session again and takes it off the list.
        (setf (switchboard-session-resume-id s) "conv-1")
        (switchboard--remember-closed s)
        (setq switchboard--sessions nil)
        (switchboard-reopen (car switchboard--recent))
        (should (equal (switchboard-session-id resumed) "heron-0001"))
        (should (equal (switchboard-session-resume-id resumed) "conv-1"))
        (should (memq resumed switchboard--sessions))
        (should-not (cl-find "heron-0001" switchboard--recent
                             :key (lambda (e) (plist-get e :id)) :test #'equal))
        ;; Not when its folder has gone; x forgets it.
        (let ((entry (append (list :closed 0 :dir "/no/such/folder/") (cddr (car switchboard--recent)))))
          (push entry switchboard--recent)
          (should-error (switchboard-reopen entry) :type 'user-error)
          (switchboard-forget entry)
          (should-not (memq entry switchboard--recent)))))))

(ert-deftest switchboard-test-parse-git-handles-binary-files ()
  (should (equal (switchboard--parse-git "sb/heron\n@@\n-\t-\timg.png\n4\t1\ta.el\n@@\n")
                 '("sb/heron" 4 1 0))))

(ert-deftest switchboard-test-events-go-to-their-conversation ()
  ;; A background agent's worker can carry another session's SWITCHBOARD_ID.
  (switchboard-test--with-session s
    (let ((other (switchboard--session-create :id "wren-0002" :name "wren" :harness 'claude
                                              :root dir :dir dir :resume-id "conv-wren")))
      (push other switchboard--sessions)
      (switchboard-test--hook "SessionStart" '((session_id . "conv-heron")))
      (switchboard-test--hook "SubagentStart" '((session_id . "conv-wren") (agent_id . "a1")
                                                (agent_type . "Explore")))
      (should-not (switchboard-session-agents s))
      (should (= 1 (length (switchboard-session-agents other))))
      ;; A new conversation (/clear) still goes by SWITCHBOARD_ID.
      (switchboard-test--hook "SessionStart" '((session_id . "conv-heron-2")))
      (should (equal (switchboard-session-resume-id s) "conv-heron-2")))))

(ert-deftest switchboard-test-a-backgrounded-fork-goes-to-its-terminal ()
  ;; The daemon's fork of wren's conversation carries heron's SWITCHBOARD_ID.
  (switchboard-test--with-session s
    (let ((other (switchboard--session-create :id "wren-0002" :name "wren" :harness 'claude
                                              :root dir :dir dir :resume-id "conv-wren")))
      (push other switchboard--sessions)
      (cl-letf (((symbol-function 'switchboard--fork-parent)
                 (lambda (id) (and (equal id "conv-fork") "conv-wren"))))
        (switchboard-test--hook "SessionStart" '((session_id . "conv-heron")))
        (switchboard-test--hook "SessionStart" '((session_id . "conv-fork") (source . "resume")))
        (switchboard-test--hook "SubagentStart" '((session_id . "conv-fork") (agent_id . "a1")
                                                  (agent_type . "Explore")))
        (should (equal (switchboard-session-resume-id other) "conv-fork"))
        (should (equal (switchboard-session-resume-id s) "conv-heron"))
        (should-not (switchboard-session-agents s))
        (should (= 1 (length (switchboard-session-agents other))))))))

(ert-deftest switchboard-test-fork-parent-reads-the-forks-command-line ()
  (let ((switchboard--fork-parents (make-hash-table :test 'equal)))
    (cl-letf (((symbol-function 'call-process)
               (lambda (&rest _)
                 (insert "/bin/zsh\n"
                         "claude --session-id f-1 --fork-session --resume /p/proj/c-0.jsonl --settings x\n"))))
      (should (equal (switchboard--fork-parent "f-1") "c-0"))
      (should-not (switchboard--fork-parent "f-2")))))

;;; Subagents

(defun switchboard-test--spawn (id type description &optional background)
  "The main agent starts subagent ID: its Agent call, then SubagentStart."
  (switchboard-test--hook "PreToolUse"
                          `((tool_name . "Agent")
                            (tool_input . ((description . ,description) (subagent_type . ,type)
                                           (run_in_background . ,(if background t :false))))))
  (switchboard-test--hook "SubagentStart" `((agent_id . ,id) (agent_type . ,type))))

(defun switchboard-test--agent (s id)
  (cl-find id (switchboard-session-agents s) :key #'switchboard-agent-id :test #'equal))

(ert-deftest switchboard-test-subagent-is-listed-with-its-task-and-tool ()
  (switchboard-test--with-session s
    (switchboard-test--hook "UserPromptSubmit" '((prompt . "go")))
    (switchboard-test--spawn "a1" "Explore" "find the hook fields")
    (switchboard-test--hook "PreToolUse" '((agent_id . "a1") (agent_type . "Explore")
                                           (tool_name . "Grep")))
    (let ((a (switchboard-test--agent s "a1")))
      (should (equal (switchboard-agent-description a) "find the hook fields"))
      (should (equal (switchboard-agent-tool a) "Grep"))
      (should (eq (switchboard-agent-status a) 'running)))
    (should (equal (switchboard--label s) "WORKING · 1 AGENT"))
    (switchboard-test--hook "SubagentStop" '((agent_id . "a1") (agent_type . "Explore")))
    (should (eq (switchboard-agent-status (switchboard-test--agent s "a1")) 'done))
    (should (equal (switchboard--label s) "WORKING"))))

(ert-deftest switchboard-test-stop-with-background-agents-waits-on-them ()
  (switchboard-test--with-session s
    (switchboard-test--hook "UserPromptSubmit" '((prompt . "go")))
    (switchboard-test--spawn "a1" "general-purpose" "bisect the failure" t)
    (switchboard-test--spawn "a2" "Explore" "other uses of retry" t)
    (switchboard-test--hook "Stop" '((background_tasks
                                      . [((id . "t1") (type . "subagent") (status . "running")
                                          (description . "bisect the failure")
                                          (agent_type . "general-purpose"))
                                         ((id . "t2") (type . "subagent") (status . "running")
                                          (description . "other uses of retry")
                                          (agent_type . "Explore"))
                                         ((id . "b1") (type . "shell") (status . "running")
                                          (description . "npm run dev"))])))
    (should (eq (switchboard-session-status s) 'agents))
    (should (equal (switchboard--label s) "WAITING ON 2 AGENTS"))
    ;; Not finished yet (`switchboard-notify' leaves out `agents').
    (should-not (memq 'agents switchboard-notify))
    (should-not (member '("heron" . ready) switchboard-test--notified))
    (should-not (switchboard-session-unread s))
    ;; Their tool calls don't make the session itself WORKING.
    (switchboard-test--hook "PostToolUse" '((agent_id . "a1") (tool_name . "Bash")))
    (should (eq (switchboard-session-status s) 'agents))
    (switchboard-test--hook "SubagentStop" '((agent_id . "a1")))
    (should (eq (switchboard-session-status s) 'agents))
    ;; The last one done: the main agent takes their results.
    (switchboard-test--hook "SubagentStop" '((agent_id . "a2")))
    (should (eq (switchboard-session-status s) 'working))
    (switchboard-test--hook "Stop" '((background_tasks . [])))
    (should (eq (switchboard-session-status s) 'ready))
    (should (equal (car switchboard-test--notified) '("heron" . ready)))))

(ert-deftest switchboard-test-stop-adds-background-agents-it-missed ()
  (switchboard-test--with-session s
    (switchboard-test--hook "UserPromptSubmit" '((prompt . "go")))
    (switchboard-test--hook "Stop" '((background_tasks
                                      . [((id . "t9") (type . "subagent") (status . "running")
                                          (description . "audit deps") (agent_type . "Explore"))])))
    (should (eq (switchboard-session-status s) 'agents))
    (let ((a (car (switchboard-session-agents s))))
      (should (equal (switchboard-agent-description a) "audit deps"))
      (should (switchboard-agent-background a)))))

(ert-deftest switchboard-test-stop-ends-agents-that-never-reported-stopping ()
  (switchboard-test--with-session s
    (switchboard-test--hook "UserPromptSubmit" '((prompt . "go")))
    (switchboard-test--spawn "a1" "Explore" "look around")
    (switchboard-test--hook "Stop" '((background_tasks . [])))
    (should (eq (switchboard-agent-status (switchboard-test--agent s "a1")) 'done))
    (should (eq (switchboard-session-status s) 'ready))))

(ert-deftest switchboard-test-blocked-agent-needs-you-then-goes-back ()
  (switchboard-test--with-session s
    (switchboard-test--hook "UserPromptSubmit" '((prompt . "go")))
    (switchboard-test--spawn "a1" "general-purpose" "fetch docs" t)
    (switchboard-test--hook "Stop" '((background_tasks
                                      . [((id . "a1") (type . "subagent") (status . "running")
                                          (description . "fetch docs"))])))
    (switchboard-test--hook "PermissionRequest"
                            '((agent_id . "a1") (agent_type . "general-purpose")
                              (tool_name . "WebFetch") (tool_input . ((url . "https://x.dev")))))
    (should (eq (switchboard-session-status s) 'waiting))
    (should (equal (switchboard-session-detail s) "general · WebFetch: https://x.dev"))
    (should (switchboard--agents-expanded-p s))    ; listed even off screen
    (switchboard-test--hook "PostToolUse" '((agent_id . "a1") (tool_name . "WebFetch")))
    (should (eq (switchboard-session-status s) 'agents))
    (should-not (switchboard-session-detail s))))

(ert-deftest switchboard-test-escape-ends-only-foreground-agents ()
  (switchboard-test--with-session s
    (switchboard-test--hook "UserPromptSubmit" '((prompt . "go")))
    (switchboard-test--spawn "fg" "Explore" "inline")
    (switchboard-test--spawn "bg" "Explore" "detached" t)
    (with-temp-buffer
      (setq switchboard--session s)
      (switchboard--note-input 'escape))
    (should (eq (switchboard-session-status s) 'stopped))
    (should (eq (switchboard-agent-status (switchboard-test--agent s "fg")) 'done))
    (should (eq (switchboard-agent-status (switchboard-test--agent s "bg")) 'running))
    (should (equal (switchboard--label s) "STOPPED · 1 AGENT"))))

(ert-deftest switchboard-test-finished-agents-drop-after-a-while ()
  (switchboard-test--with-session s
    (switchboard-test--spawn "a1" "Explore" "quick look")
    (switchboard-test--hook "SubagentStop" '((agent_id . "a1")))
    (should-not (switchboard--prune-agents))
    (let ((a (switchboard-test--agent s "a1")))
      (switchboard--change-agent s a :end (- (float-time) switchboard--agent-linger 1)))
    (should (switchboard--prune-agents))
    (should-not (switchboard-session-agents s))))

(ert-deftest switchboard-test-agent-changes-leave-old-copies-alone ()
  ;; The image cache keys on copies of sessions, which share agents.
  (switchboard-test--with-session s
    (switchboard-test--spawn "a1" "Explore" "look")
    (let ((copy (copy-sequence s)))
      (switchboard-test--hook "PreToolUse" '((agent_id . "a1") (tool_name . "Read")))
      (should-not (switchboard-agent-tool (car (switchboard-session-agents copy))))
      (should-not (equal copy s)))))

(defmacro switchboard-test--with-terminal-keys (&rest body)
  "Run BODY with an empty terminal keymap, restoring the real one after."
  (declare (indent 0))
  `(let ((switchboard-terminal-mode-map (make-sparse-keymap))
         (switchboard--terminal-bound nil))
     ,@body))

(ert-deftest switchboard-test-terminal-keys-first-entry-wins ()
  (switchboard-test--with-terminal-keys
    (switchboard--bind-terminal-keys
     (append '(("C-g" . send) ("s-v" . nil) ("M-<left>" . ignore))
             switchboard-terminal-keys))
    (should (eq (keymap-lookup switchboard-terminal-mode-map "C-g") 'ghostel--send-event))
    (should-not (keymap-lookup switchboard-terminal-mode-map "s-v"))
    (should (eq (keymap-lookup switchboard-terminal-mode-map "M-<left>") 'ignore))))

(ert-deftest switchboard-test-terminal-keys-rebind-drops-old-keys ()
  (switchboard-test--with-terminal-keys
    (switchboard--bind-terminal-keys '(("C-g" . send) ("s-v" . paste)))
    (switchboard--bind-terminal-keys '(("s-v" . paste)))
    (should-not (keymap-lookup switchboard-terminal-mode-map "C-g"))
    (should (keymap-lookup switchboard-terminal-mode-map "s-v"))))

;;; Pi

;; These replay what switchboard-pi.ts sent from a real Pi run (against a
;; scripted model), cut down to the fields that matter.

(defmacro switchboard-test--with-pi-session (var &rest body)
  "Like `switchboard-test--with-session', for a Pi session that has started."
  (declare (indent 1))
  `(switchboard-test--with-session ,var
     (setf (switchboard-session-harness ,var) 'pi)
     (cl-letf (((symbol-function 'switchboard--update-git) #'ignore)
               ((symbol-function 'switchboard--update-git-soon) #'ignore))
       (switchboard-test--hook "SessionStart" '((session_id . "01a197b5") (source . "startup")
                                                (tokens . 0)))
       ,@body)))

(ert-deftest switchboard-test-pi-command ()
  (switchboard-test--with-session s
    (setf (switchboard-session-harness s) 'pi)
    (let ((base (list "pi" "-e" switchboard--pi-extension
                      "--append-system-prompt" (switchboard--sb-instructions s))))
      (should (equal (switchboard--command s "-x first") (append base '("--" "-x first"))))
      (should (equal (switchboard--command s nil t) (append base '("--continue"))))
      (setf (switchboard-session-resume-id s) "01a197b5")
      (should (equal (switchboard--command s nil t)
                     (append base '("--session-id" "01a197b5")))))))

(ert-deftest switchboard-test-pi-turn ()
  (switchboard-test--with-pi-session s
    (should (equal (switchboard-session-resume-id s) "01a197b5"))
    (switchboard-test--hook "UserPromptSubmit" '((session_id . "01a197b5") (prompt . "fix it")
                                                 (tokens . 0)))
    (should (eq (switchboard-session-status s) 'working))
    (should (equal (switchboard-session-task s) "fix it"))
    (switchboard-test--hook "PreToolUse" '((session_id . "01a197b5") (tool_name . "bash")))
    (switchboard-test--hook "PostToolUse" '((session_id . "01a197b5") (tool_name . "bash")))
    (switchboard-test--hook "Stop" '((session_id . "01a197b5") (tokens . 1239)))
    (should (eq (switchboard-session-status s) 'ready))
    (should (= (switchboard-session-tokens s) 1239))))

(ert-deftest switchboard-test-pi-error-and-abort ()
  (switchboard-test--with-pi-session s
    (switchboard-test--hook "UserPromptSubmit" '((session_id . "01a197b5") (prompt . "go")))
    (switchboard-test--hook "StopFailure" '((session_id . "01a197b5") (error . "400: scripted")))
    (should (eq (switchboard-session-status s) 'error))
    (should (equal (switchboard-session-detail s) "400: scripted"))
    (switchboard-test--hook "UserPromptSubmit" '((session_id . "01a197b5") (prompt . "again")))
    (switchboard-test--hook "Aborted" '((session_id . "01a197b5")))
    (should (eq (switchboard-session-status s) 'stopped))
    ;; ESC already stopped it: the Aborted that follows changes nothing.
    (switchboard-test--hook "Aborted" '((session_id . "01a197b5")))
    (should (eq (switchboard-session-status s) 'stopped))))

(ert-deftest switchboard-test-pi-prompt-needs-you ()
  (switchboard-test--with-pi-session s
    (switchboard-test--hook "UserPromptSubmit" '((session_id . "01a197b5") (prompt . "go")))
    (switchboard-test--hook "PreToolUse" '((session_id . "01a197b5") (tool_name . "bash")))
    (switchboard-test--hook "PermissionRequest"
                            '((session_id . "01a197b5") (tool_name . "bash")
                              (tool_input . ((command . "rm -rf junk")))))
    (should (eq (switchboard-session-status s) 'waiting))
    (should (equal (switchboard-session-detail s) "bash: rm -rf junk"))
    (switchboard-test--hook "PreToolUse" '((session_id . "01a197b5") (tool_name . "bash")))
    (should (eq (switchboard-session-status s) 'working))
    (should-not (switchboard-session-detail s))))

(ert-deftest switchboard-test-pi-title-from-its-name ()
  (switchboard-test--with-pi-session s
    (switchboard-test--hook "SessionInfo" '((session_id . "01a197b5") (title . "Fix the hook")))
    (should (equal (switchboard-session-title s) "Fix the hook"))))

(ert-deftest switchboard-test-pi-foreground-subagent ()
  (switchboard-test--with-pi-session s
    (switchboard-test--hook "UserPromptSubmit" '((session_id . "01a197b5") (prompt . "go")))
    (switchboard-test--hook "PreToolUse" '((session_id . "01a197b5") (tool_name . "subagent")))
    (switchboard-test--hook "SubagentStart" '((session_id . "01a197b5") (agent_id . "r1:0")
                                              (agent_type . "scout")
                                              (description . "CHILD look around")))
    (switchboard-test--hook "PreToolUse" '((session_id . "01a197b5") (agent_id . "r1:0")
                                           (agent_type . "scout") (tool_name . "bash")))
    (let ((a (switchboard-test--agent s "r1:0")))
      (should (equal (switchboard-agent-description a) "CHILD look around"))
      (should (equal (switchboard-agent-tool a) "bash"))
      (should-not (switchboard-agent-background a)))
    (switchboard-test--hook "SubagentStop" '((session_id . "01a197b5") (agent_id . "r1:0")))
    (switchboard-test--hook "PostToolUse" '((session_id . "01a197b5") (tool_name . "subagent")))
    (switchboard-test--hook "Stop" '((session_id . "01a197b5") (tokens . 1239)))
    (should (eq (switchboard-agent-status (switchboard-test--agent s "r1:0")) 'done))
    (should (eq (switchboard-session-status s) 'ready))))

(ert-deftest switchboard-test-pi-background-subagent ()
  (switchboard-test--with-pi-session s
    (switchboard-test--hook "UserPromptSubmit" '((session_id . "01a197b5") (prompt . "go")))
    (switchboard-test--hook "SubagentStart" '((session_id . "01a197b5") (agent_id . "bg1")
                                              (agent_type . "scout")
                                              (description . "CHILD look around")
                                              (background . t)))
    (switchboard-test--hook "Stop" '((session_id . "01a197b5")))
    (should (eq (switchboard-session-status s) 'agents))
    (should (equal (switchboard--label s) "WAITING ON 1 AGENT"))
    ;; Reloading Pi's extensions keeps it; its completion wakes a turn,
    ;; which pi-subagents starts before it reports the child done.
    (switchboard-test--hook "SessionStart" '((session_id . "01a197b5") (source . "reload")))
    (should (switchboard-test--agent s "bg1"))
    (switchboard-test--hook "UserPromptSubmit" '((session_id . "01a197b5")))
    (should (eq (switchboard-session-status s) 'working))
    (should (equal (switchboard-session-task s) "go"))
    (switchboard-test--hook "SubagentStop" '((session_id . "01a197b5") (agent_id . "bg1")))
    (switchboard-test--hook "Stop" '((session_id . "01a197b5")))
    (should (eq (switchboard-agent-status (switchboard-test--agent s "bg1")) 'done))
    (should (eq (switchboard-session-status s) 'ready))))

(ert-deftest switchboard-test-pi-background-subagent-tool-after-the-turn ()
  (switchboard-test--with-pi-session s
    (switchboard-test--hook "UserPromptSubmit" '((session_id . "01a197b5") (prompt . "go")))
    (switchboard-test--hook "SubagentStart" '((session_id . "01a197b5") (agent_id . "bg1")
                                              (agent_type . "scout") (background . t)))
    (switchboard-test--hook "Stop" '((session_id . "01a197b5")))
    ;; Its tool shows on the agent; the session's turn stays over.
    (switchboard-test--hook "PreToolUse" '((session_id . "01a197b5") (agent_id . "bg1")
                                           (agent_type . "scout") (tool_name . "bash")))
    (should (equal (switchboard-agent-tool (switchboard-test--agent s "bg1")) "bash"))
    (should (eq (switchboard-session-status s) 'agents))))

(ert-deftest switchboard-test-pi-session-does-not-look-for-forks ()
  (switchboard-test--with-session s
    (setf (switchboard-session-harness s) 'pi)
    (cl-letf (((symbol-function 'switchboard--update-git) #'ignore)
              ((symbol-function 'switchboard--fork-parent)
               (lambda (_) (error "Looked for a fork"))))
      (switchboard-test--hook "SessionStart" '((session_id . "01a197b5") (source . "startup")))
      (should (equal (switchboard-session-resume-id s) "01a197b5")))))

;;; Helpers

(defvar switchboard-test--sent nil
  "(SESSION-TITLE . TEXT) typed into terminals, newest first.")

(defalias 'switchboard-test--real-notify (symbol-function 'switchboard--notify))

(defmacro switchboard-test--with-helpers (var &rest body)
  "Like `switchboard-test--with-session', with spawning and typing stubbed out."
  (declare (indent 1))
  `(switchboard-test--with-session ,var
     (let ((switchboard-test--sent nil)
           (switchboard--helper-results (make-hash-table :test 'equal)))
       (setf (switchboard-session-buffer ,var) (generate-new-buffer " *sb-test*"))
       (unwind-protect
           (cl-letf (((symbol-function 'switchboard--spawn)
                      (lambda (s &optional prompt _resume)
                        (setf (switchboard-session-buffer s) (generate-new-buffer " *sb-test*")
                              (switchboard-session-task s) prompt)
                        (switchboard--set s 'working)))
                     ((symbol-function 'switchboard--send)
                      (lambda (s text)
                        (push (cons (switchboard--title s) text) switchboard-test--sent)))
                     ((symbol-function 'switchboard--update-git) #'ignore)
                     ((symbol-function 'switchboard--update-git-soon) #'ignore)
                     ((symbol-function 'switchboard--update-tokens) #'ignore)
                     ((symbol-function 'executable-find) (lambda (&rest _) "/bin/true"))
                     ((symbol-function 'run-at-time) #'ignore)
                     ((symbol-function 'switchboard--clear-notification) #'ignore)
                     ((symbol-function 'switchboard--on-screen-p)
                      (lambda (s) (eq s (bound-and-true-p switchboard-test--on-screen)))))
             ,@body)
         (dolist (s switchboard--sessions)
           (when (buffer-live-p (switchboard-session-buffer s))
             (kill-buffer (switchboard-session-buffer s))))))))

(defvar switchboard-test--on-screen nil)

(defun switchboard-test--sb (caller &rest args)
  "Run `sb ARGS' as CALLER through the real entry point: (CODE . OUTPUT)."
  (let ((file (make-temp-file "sb-test-args"))
        (reply (make-temp-file "sb-test-reply")))
    (unwind-protect
        (progn
          ;; What bin/sb writes.
          (with-temp-file file
            (dolist (a args)
              (insert "\""
                      (replace-regexp-in-string "\"" "\\\\\"" (string-replace "\\" "\\\\" a) t)
                      "\"\n")))
          (switchboard-sb (switchboard-session-id caller) file reply)
          (with-temp-buffer
            (insert-file-contents reply)
            (goto-char (point-min))
            (let ((code (read (current-buffer))))
              (forward-line 1)
              (cons code (string-trim (buffer-substring (point) (point-max)))))))
      (delete-file file)
      (delete-file reply))))

(defun switchboard-test--helper (name)
  (cl-find name switchboard--sessions :key #'switchboard--title :test #'equal))

(defun switchboard-test--event (s event &optional payload)
  "Send EVENT as session S, whichever it is."
  (let ((file (and payload (make-temp-file "switchboard-payload" nil ".json"))))
    (when file (with-temp-file file (insert (json-serialize payload))))
    (switchboard-hook (switchboard-session-id s) event file)))

(defun switchboard-test--quiet (s)
  "Nobody has typed in S for a while."
  (setf (switchboard-session-last-input s) nil))

(ert-deftest switchboard-test-sb-run-starts-a-helper-under-its-parent ()
  (switchboard-test--with-helpers s
    (let ((out (switchboard-test--sb s "run" "--agent" "codex" "--name" "reviewer" "check it")))
      (should (= (car out) 0))
      (should (string-match-p "Started reviewer (codex)" (cdr out))))
    (let ((h (switchboard-test--helper "reviewer")))
      (should (eq (switchboard-session-harness h) 'codex))
      (should (equal (switchboard-session-task h) "check it"))
      (should (eq (switchboard--parent h) s))
      (should (eq (switchboard-session-status h) 'working))
      ;; Under its parent, not in a group of its own.
      (should-not (memq h (switchboard--visible))))))

(ert-deftest switchboard-test-sb-defaults-to-the-callers-agent ()
  (switchboard-test--with-helpers s
    (setf (switchboard-session-harness s) 'pi)
    (switchboard-test--sb s "run" "--name" "a" "go")
    (should (eq (switchboard-session-harness (switchboard-test--helper "a")) 'pi))))

(ert-deftest switchboard-test-sb-text-is-never-code ()
  (switchboard-test--with-helpers s
    (let ((task "say \"hi\" \\ (kill-emacs) $(touch /tmp/x) `id`\nsecond line"))
      (should (= (car (switchboard-test--sb s "run" "--name" "a" task)) 0))
      (should (equal (switchboard-session-task (switchboard-test--helper "a")) task)))))

(ert-deftest switchboard-test-sb-a-task-starting-with-a-dash-stays-a-task ()
  (switchboard-test--with-helpers s
    ;; The agent's command line would read it as an option otherwise.
    (should (= (car (switchboard-test--sb s "run" "--name" "a" "-x do it")) 0))
    (should (equal (switchboard-session-task (switchboard-test--helper "a")) "Task: -x do it"))))

(ert-deftest switchboard-test-sb-refuses-bad-runs ()
  (switchboard-test--with-helpers s
    (should (= (car (switchboard-test--sb s "run")) 1))
    (should (= (car (switchboard-test--sb s "run" "two" "tasks")) 1))
    (should (= (car (switchboard-test--sb s "run" "--name" "Bad Name" "go")) 1))
    (should (= (car (switchboard-test--sb s "run" "--agent" "cursor" "go")) 1))
    (should (= (car (switchboard-test--sb s "frobnicate")) 1))
    (should (= (car (switchboard-test--sb s "run" "--name" "a" "go")) 0))
    (should (= (car (switchboard-test--sb s "run" "--name" "a" "again")) 1))))

(ert-deftest switchboard-test-sb-goes-two-levels-deep ()
  (switchboard-test--with-helpers s
    (should (= (car (switchboard-test--sb s "run" "--name" "one" "go")) 0))
    (let ((one (switchboard-test--helper "one")))
      (should (= (car (switchboard-test--sb one "run" "--name" "two" "go")) 0))
      (let* ((two (switchboard-test--helper "two"))
             (out (switchboard-test--sb two "run" "--name" "three" "go")))
        (should (eq (switchboard--parent two) one))
        (should (= (car out) 1))
        (should (string-match-p "levels deep" (cdr out)))))))

(ert-deftest switchboard-test-sb-limits-a-whole-tree-to-ten ()
  (switchboard-test--with-helpers s
    (dotimes (i 9)
      (should (= (car (switchboard-test--sb s "run" "--name" (format "h%d" i) "go")) 0)))
    ;; The tenth comes from a helper: the limit is the tree's, not each session's.
    (should (= (car (switchboard-test--sb (switchboard-test--helper "h0")
                                          "run" "--name" "nested" "go"))
               0))
    (let ((out (switchboard-test--sb s "run" "--name" "eleven" "go")))
      (should (= (car out) 1))
      (should (string-match-p "10 helpers running" (cdr out))))
    ;; A finished one frees a place.
    (switchboard-test--event (switchboard-test--helper "h1") "Stop")
    (should (= (car (switchboard-test--sb s "run" "--name" "eleven" "go")) 0))))

(ert-deftest switchboard-test-sb-only-reaches-your-own-helpers ()
  (switchboard-test--with-helpers s
    (switchboard-test--sb s "run" "--name" "mine" "go")
    (let ((mine (switchboard-test--helper "mine")))
      (switchboard-test--sb mine "run" "--name" "deeper" "go")
      (should (= (car (switchboard-test--sb s "send" "deeper" "hi")) 1))
      (should (= (car (switchboard-test--sb s "stop" "deeper")) 1))
      (should (switchboard-test--helper "deeper"))
      (should (= (car (switchboard-test--sb mine "report" "x")) 0))
      (should (= (car (switchboard-test--sb s "report" "x")) 1)))))

(ert-deftest switchboard-test-sb-send-wont-answer-permission-prompts ()
  (switchboard-test--with-helpers s
    (switchboard-test--sb s "run" "--name" "a" "go")
    (let ((a (switchboard-test--helper "a")))
      (should (= (car (switchboard-test--sb s "send" "a" "keep going")) 0))
      (should (equal (car switchboard-test--sent) '("a" . "keep going")))
      (switchboard-test--event a "PermissionRequest"
                               '((tool_name . "Bash") (tool_input . ((command . "rm -rf /")))))
      (setq switchboard-test--sent nil)
      (let ((out (switchboard-test--sb s "send" "a" "yes")))
        (should (= (car out) 1))
        (should (string-match-p "only the user" (cdr out))))
      (should-not switchboard-test--sent))))

(ert-deftest switchboard-test-helper-result-waits-until-the-parent-is-free ()
  (switchboard-test--with-helpers s
    (switchboard-test--event s "UserPromptSubmit" '((prompt . "go")))
    (switchboard-test--sb s "run" "--name" "a" "look")
    (let ((a (switchboard-test--helper "a")))
      (switchboard-test--sb a "report" "found \"it\"")
      (switchboard-test--quiet s)
      (switchboard-test--event a "Stop")
      ;; The parent is mid-turn: it can't take it yet.
      (should-not switchboard-test--sent)
      (should (switchboard-session-pending a))
      (switchboard-test--event s "Stop")
      (should (equal (car switchboard-test--sent)
                     '("heron" . "<sb from=\"a\" status=\"finished\">found \"it\"</sb>")))
      (should-not (switchboard-session-pending a))
      (should (switchboard-session-told a)))))

(ert-deftest switchboard-test-helper-result-names-its-branch ()
  (switchboard-test--with-helpers s
    (switchboard-test--sb s "run" "--name" "a" "work")
    (let ((a (switchboard-test--helper "a")))
      (setf (switchboard-session-worktree a) t (switchboard-session-branch a) "sb/a-1")
      (should (string-match-p "branch=\"sb/a-1\"" (switchboard--helper-report-text a)))
      (should (string-match-p "No report; see: sb read a" (switchboard--helper-report-text a))))))

(ert-deftest switchboard-test-long-helper-report-is-cut ()
  (switchboard-test--with-helpers s
    (switchboard-test--sb s "run" "--name" "a" "work")
    (let ((a (switchboard-test--helper "a")))
      (setf (switchboard-session-report a) (make-string 5000 ?x))
      (let ((text (switchboard--helper-report-text a)))
        (should (< (length text) 2200))
        (should (string-match-p "sb read a" text))))))

(ert-deftest switchboard-test-helper-result-holds-off-while-you-type ()
  (switchboard-test--with-helpers s
    (switchboard-test--sb s "run" "--name" "a" "look")
    (setf (switchboard-session-status s) 'ready
          (switchboard-session-last-input s) (float-time))
    (switchboard-test--event (switchboard-test--helper "a") "Stop")
    (should-not switchboard-test--sent)
    (switchboard-test--quiet s)
    (switchboard--deliver-reports s)
    (should switchboard-test--sent)))

(ert-deftest switchboard-test-parent-waits-on-running-helpers ()
  (switchboard-test--with-helpers s
    (switchboard-test--event s "UserPromptSubmit" '((prompt . "go")))
    (switchboard-test--sb s "run" "--name" "a" "look")
    (switchboard-test--event s "Stop")
    (should (eq (switchboard-session-status s) 'agents))
    (should (equal (switchboard--label s) "WAITING ON 1 AGENT"))
    (let ((a (switchboard-test--helper "a")))
      (switchboard-test--quiet s)
      (switchboard-test--event a "Stop")
      (should switchboard-test--sent))))

(ert-deftest switchboard-test-sb-wait-takes-the-result-itself ()
  (switchboard-test--with-helpers s
    (switchboard-test--event s "UserPromptSubmit" '((prompt . "go")))
    (switchboard-test--sb s "run" "--name" "a" "look")
    (should (= (car (switchboard-test--sb s "wait" "a")) 3))
    (let ((a (switchboard-test--helper "a")))
      (switchboard-test--sb a "report" "done")
      (switchboard-test--event a "Stop")
      (let ((out (switchboard-test--sb s "wait" "a")))
        (should (= (car out) 0))
        (should (string-match-p "done" (cdr out))))
      ;; It already has it: nothing gets typed in as well.
      (switchboard-test--event s "Stop")
      (should-not switchboard-test--sent))))

(ert-deftest switchboard-test-sb-wait-result-ends-a-parents-wait ()
  (switchboard-test--with-helpers s
    (switchboard-test--sb s "run" "--name" "a" "look")
    (setf (switchboard-session-status s) 'agents)
    (let ((a (switchboard-test--helper "a")))
      (switchboard-test--quiet s)
      (setf (switchboard-session-last-input s) (float-time))
      (switchboard-test--event a "Stop")
      (switchboard-test--sb s "wait" "a")
      (switchboard--deliver-reports s)
      (should (eq (switchboard-session-status s) 'ready)))))

(ert-deftest switchboard-test-helper-needing-you-shows-on-its-parent ()
  (switchboard-test--with-helpers s
    (switchboard-test--event s "UserPromptSubmit" '((prompt . "go")))
    (switchboard-test--sb s "run" "--name" "a" "look")
    (let ((a (switchboard-test--helper "a")))
      (should-not (switchboard--agents-expanded-p
                   (switchboard--session-create :id "other" :name "o" :dir "/" :root "/")))
      (should-not (switchboard--agents-expanded-p s))
      (switchboard-test--event a "PermissionRequest"
                               '((tool_name . "Bash") (tool_input . ((command . "ls")))))
      (should (eq (switchboard--shown-status s) 'waiting))
      (should (equal (switchboard--label s) "NEEDS YOU · 1 AGENT"))
      (should (switchboard--agents-expanded-p s))
      ;; Its own status stays, and the queue goes to the one that asked.
      (should-not (eq (switchboard-session-status s) 'waiting))
      (cl-letf (((symbol-function 'switchboard-open)
                 (lambda (x) (should (eq x a))))
                ((symbol-function 'switchboard--main-window) #'selected-window))
        (switchboard-next-attention)))))

(ert-deftest switchboard-test-helper-finishing-doesnt-notify-you ()
  (switchboard-test--with-helpers s
    (switchboard-test--sb s "run" "--name" "a" "look")
    (let ((a (switchboard-test--helper "a"))
          (posted nil))
      ;; The real `switchboard--notify', stopped only where it would post.
      (cl-letf (((symbol-function 'switchboard--notify)
                 (symbol-function 'switchboard-test--real-notify))
                ((symbol-function 'frame-focus-state) #'ignore)
                ((symbol-function 'start-process)
                 (lambda (&rest args) (push args posted) nil)))
        (let ((switchboard-notify '(waiting ready error exited)))
          (switchboard-test--event a "Stop")
          (should-not posted)
          (should-not (switchboard-session-unread a))
          ;; A prompt or an error is still yours.
          (switchboard-test--event a "StopFailure" '((error . "boom")))
          (should posted))))))

(ert-deftest switchboard-test-finished-helper-is-cleaned-up-after-a-while ()
  (switchboard-test--with-helpers s
    (switchboard-test--sb s "run" "--name" "a" "look")
    (let ((a (switchboard-test--helper "a")))
      (switchboard-test--sb a "report" "the answer")
      (switchboard-test--quiet s)
      (switchboard-test--event a "Stop")
      (switchboard-test--event s "Stop")
      (switchboard--prune-helpers)
      (should (memq a switchboard--sessions))
      (setf (switchboard-session-told a) (- (float-time) switchboard--agent-linger 1))
      (switchboard--prune-helpers)
      (should-not (memq a switchboard--sessions))
      (should-not (buffer-live-p (switchboard-session-buffer a)))
      ;; Its parent can still read what it said.
      (let ((out (switchboard-test--sb s "read" "a")))
        (should (= (car out) 0))
        (should (string-match-p "the answer" (cdr out))))
      (should (= (car (switchboard-test--sb s "wait" "a")) 0)))))

(ert-deftest switchboard-test-kept-helper-isnt-cleaned-up ()
  (switchboard-test--with-helpers s
    (switchboard-test--sb s "run" "--keep" "--name" "a" "look")
    (let ((a (switchboard-test--helper "a")))
      (switchboard-test--event a "Stop")
      (setf (switchboard-session-told a) (- (float-time) switchboard--agent-linger 1))
      (switchboard--prune-helpers)
      (should (memq a switchboard--sessions))
      (switchboard-test--sb s "release" "a")
      (switchboard--prune-helpers)
      (should-not (memq a switchboard--sessions)))))

(ert-deftest switchboard-test-helper-on-screen-isnt-cleaned-up ()
  (switchboard-test--with-helpers s
    (switchboard-test--sb s "run" "--name" "a" "look")
    (let* ((a (switchboard-test--helper "a"))
           (switchboard-test--on-screen a))
      (switchboard-test--event a "Stop")
      (setf (switchboard-session-told a) (- (float-time) switchboard--agent-linger 1))
      (switchboard--prune-helpers)
      (should (memq a switchboard--sessions)))))

(ert-deftest switchboard-test-followup-restarts-a-helpers-clock ()
  (switchboard-test--with-helpers s
    (switchboard-test--sb s "run" "--name" "a" "look")
    (let ((a (switchboard-test--helper "a")))
      (switchboard-test--event a "Stop")
      (setf (switchboard-session-told a) (- (float-time) 30))
      (switchboard-test--event a "UserPromptSubmit" '((prompt . "more")))
      (should-not (switchboard-session-told a))
      (switchboard-test--quiet s)
      (setf (switchboard-session-status s) 'ready)
      (switchboard-test--event a "Stop")
      ;; One report for the task, and another for the follow-up.
      (should (= (length switchboard-test--sent) 2)))))

(ert-deftest switchboard-test-stopped-helper-stays-yours ()
  (switchboard-test--with-helpers s
    (switchboard-test--sb s "run" "--name" "a" "look")
    (let ((a (switchboard-test--helper "a")))
      (switchboard--set a 'stopped)
      (should-not (switchboard--helper-finished-p a))
      (should-not (switchboard-session-pending a)))))

(ert-deftest switchboard-test-helpers-go-when-their-parent-does ()
  (switchboard-test--with-helpers s
    (switchboard-test--sb s "run" "--name" "a" "look")
    (switchboard-test--sb (switchboard-test--helper "a") "run" "--name" "b" "look")
    (switchboard--stop-helpers s)
    (should-not (switchboard-test--helper "a"))
    (should-not (switchboard-test--helper "b"))))

(ert-deftest switchboard-test-helpers-arent-saved ()
  (switchboard-test--with-helpers s
    (switchboard-test--sb s "run" "--name" "a" "look")
    (let ((switchboard-save-file (make-temp-file "sb-test-save")))
      (unwind-protect
          (progn
            (switchboard--save)
            (with-temp-buffer
              (insert-file-contents switchboard-save-file)
              (let ((saved (read (current-buffer))))
                (should (= (length saved) 1))
                (should (equal (plist-get (car saved) :id) "heron-0001")))))
        (delete-file switchboard-save-file)))))

(ert-deftest switchboard-test-bar-counts-helpers-and-subagents ()
  (switchboard-test--with-helpers s
    (switchboard-test--sb s "run" "--name" "a" "look")
    (switchboard-test--event s "SubagentStart" '((agent_id . "x") (agent_type . "Explore")))
    (should (equal (mapcar #'switchboard-agent-status (switchboard--bar-agents s))
                   '(running running)))
    (should (= (length (switchboard--descendants s)) 1))))

(ert-deftest switchboard-test-draws-helper-rows ()
  (switchboard-test--with-helpers s
    (switchboard-test--sb s "run" "--name" "reviewer" "check the retry logic")
    (let ((h (switchboard-test--helper "reviewer")))
      (should (switchboard--helper-row h 0 nil 400 nil (switchboard--palette)))
      (setf (switchboard-session-worktree h) t (switchboard-session-branch h) "sb/x"
            (switchboard-session-held h) t)
      (should (switchboard--helper-row h 1 t 400 t (switchboard--palette)))
      (should (switchboard--row s 1 400 nil (switchboard--palette))))))

(ert-deftest switchboard-test-claudes-own-agents-dont-wake-a-parent-waiting-on-a-helper ()
  ;; Seen for real: Claude Code ends an agent of its own just after a turn.
  (switchboard-test--with-helpers s
    (switchboard-test--event s "UserPromptSubmit" '((prompt . "go")))
    (switchboard-test--sb s "run" "--name" "a" "look")
    (switchboard-test--event s "Stop")
    (should (eq (switchboard-session-status s) 'agents))
    (switchboard-test--event s "SubagentStop" '((agent_id . "claudes-own")))
    (should (eq (switchboard-session-status s) 'agents))
    (switchboard-test--quiet s)
    (switchboard-test--event (switchboard-test--helper "a") "Stop")
    (should switchboard-test--sent)))

(ert-deftest switchboard-test-helper-stuck-at-a-prompt-needs-you ()
  (switchboard-test--with-helpers s
    (switchboard-test--sb s "run" "--name" "a" "look")
    (let ((a (switchboard-test--helper "a"))
          (screen "Do you trust this folder?"))
      (cl-letf (((symbol-function 'switchboard--helper-screen) (lambda (&rest _) screen)))
        ;; The first look has nothing to compare with.
        (switchboard--watch-startup a)
        (should (eq (switchboard-session-status a) 'working))
        (switchboard--watch-startup a screen 1)
        (should (eq (switchboard-session-status a) 'waiting))
        (should (eq (switchboard--shown-status s) 'waiting))
        (should (= (car (switchboard-test--sb s "send" "a" "yes")) 1))
        ;; You answer: its terminal moves on.
        (let ((before screen))
          (setq screen "Starting up")
          (switchboard--watch-startup a before 2))
        (should (eq (switchboard-session-status a) 'working))
        (should-not (switchboard-session-detail a))))))

(ert-deftest switchboard-test-helper-event-ends-a-startup-stall ()
  (switchboard-test--with-helpers s
    (switchboard-test--sb s "run" "--name" "a" "look")
    (let ((a (switchboard-test--helper "a")))
      (cl-letf (((symbol-function 'switchboard--helper-screen) (lambda (&rest _) "same")))
        (switchboard--watch-startup a "same" 1))
      (should (eq (switchboard-session-status a) 'waiting))
      (switchboard-test--event a "SessionStart" '((session_id . "x")))
      (should (eq (switchboard-session-status a) 'working))
      ;; Having reported, it's no longer watched.
      (cl-letf (((symbol-function 'switchboard--helper-screen) (lambda (&rest _) "same")))
        (switchboard--watch-startup a "same" 2))
      (should (eq (switchboard-session-status a) 'working)))))

(ert-deftest switchboard-test-sb-spool-answers-requests ()
  (switchboard-test--with-helpers s
    (let* ((user-emacs-directory (file-name-as-directory (make-temp-file "sb-test-home" t)))
           (spool (switchboard--sb-spool))
           (base (expand-file-name "heron-0001--abc123" spool)))
      (with-temp-file (concat base ".req") (insert "\"ls\"\n"))
      (switchboard--sb-poll)
      (should-not (file-exists-p (concat base ".req")))
      (should-not (file-exists-p (concat base ".working")))
      (with-temp-buffer
        (insert-file-contents (concat base ".reply"))
        (should (string-match-p "\\`0\nYou have no helpers" (buffer-string))))
      (delete-directory user-emacs-directory t))))

(ert-deftest switchboard-test-the-real-sb-sends-one-request ()
  ;; Seen for real: a run that started its helper, then failed saying it
  ;; already existed, because the script had asked twice.
  (switchboard-test--with-helpers s
    (let* ((user-emacs-directory (file-name-as-directory (make-temp-file "sb-test-home" t)))
           (output (generate-new-buffer " *sb-test-output*"))
           (process-environment
            (append (list (concat "SWITCHBOARD_ID=" (switchboard-session-id s))
                          (concat "SWITCHBOARD_SB_DIR=" (switchboard--sb-spool)))
                    process-environment))
           (proc (make-process :name "sb-test" :buffer output :connection-type 'pipe
                               :command (list "/bin/sh"
                                              (expand-file-name "sb" switchboard--sb-directory)
                                              "run" "--name" "once" "say \"hi\""))))
      (unwind-protect
          (progn
            (while (process-live-p proc)
              (switchboard--sb-poll)
              (accept-process-output proc 0.05))
            (should (= (process-exit-status proc) 0))
            (should (string-match-p "Started once"
                                    (with-current-buffer output (buffer-string))))
            (should (equal (switchboard-session-task (switchboard-test--helper "once"))
                           "say \"hi\"")))
        (kill-buffer output)
        (delete-directory user-emacs-directory t)))))

;; These use real git: deleting someone's work by mistake is the worst this
;; can do.

(defmacro switchboard-test--with-worktree-helper (var parent &rest body)
  "Run BODY with PARENT a session in a repo, and VAR its helper on a worktree."
  (declare (indent 2))
  `(switchboard-test--with-helpers ,parent
     (let* ((default-directory (switchboard-session-dir ,parent))
            (switchboard-worktree-directory (make-temp-file "sb-test-worktrees" t)))
       (call-process "git" nil nil nil "init" "-q" "-b" "main")
       (call-process "git" nil nil nil "-c" "user.name=t" "-c" "user.email=t@t"
                     "commit" "-q" "--allow-empty" "-m" "init")
       (should (= (car (switchboard-test--sb ,parent "run" "--worktree" "--name" "w" "edit")) 0))
       (let ((,var (switchboard-test--helper "w")))
         (unwind-protect (progn ,@body)
           (delete-directory switchboard-worktree-directory t))))))

(defun switchboard-test--commit-in (dir)
  (let ((default-directory dir))
    (with-temp-file "new.txt" (insert "work"))
    (call-process "git" nil nil nil "add" ".")
    (call-process "git" nil nil nil "-c" "user.name=t" "-c" "user.email=t@t"
                  "commit" "-q" "-m" "work")))

(defun switchboard-test--expire (h)
  (setf (switchboard-session-status h) 'ready
        (switchboard-session-told h) (- (float-time) switchboard--agent-linger 1))
  (switchboard--prune-helpers))

(ert-deftest switchboard-test-helper-worktree-with-nothing-to-lose-is-removed ()
  (switchboard-test--with-worktree-helper h s
    (let ((dir (switchboard-session-dir h)))
      (should (file-directory-p dir))
      (should (string-prefix-p "sb/w-" (switchboard-session-branch h)))
      (switchboard-test--expire h)
      (should-not (memq h switchboard--sessions))
      (should-not (file-directory-p dir)))))

(ert-deftest switchboard-test-helper-unmerged-commits-are-kept ()
  (switchboard-test--with-worktree-helper h s
    (let ((dir (switchboard-session-dir h))
          (branch (switchboard-session-branch h)))
      (switchboard-test--commit-in dir)
      (switchboard-test--expire h)
      ;; Still listed, and its work is still there.
      (should (memq h switchboard--sessions))
      (should (switchboard-session-held h))
      (should (file-exists-p (expand-file-name "new.txt" dir)))
      ;; Merged into its parent: now it can go.
      (let ((default-directory (switchboard-session-dir s)))
        (call-process "git" nil nil nil "-c" "user.name=t" "-c" "user.email=t@t"
                      "merge" "-q" branch))
      (switchboard-test--expire h)
      (should-not (memq h switchboard--sessions))
      (should-not (file-directory-p dir)))))

(ert-deftest switchboard-test-helper-merged-into-a-worktree-parent-can-go ()
  (switchboard-test--with-worktree-helper h s
    (let* ((dir (switchboard-session-dir h))
           (branch (switchboard-session-branch h))
           (parent-dir (make-temp-file "sb-test-parent-tree" t)))
      (delete-directory parent-dir t)
      (let ((default-directory (switchboard-session-dir s)))
        (call-process "git" nil nil nil "worktree" "add" "-q" "-b" "parent-work" parent-dir))
      (setf (switchboard-session-dir s) (file-name-as-directory parent-dir))
      (switchboard-test--commit-in dir)
      (let ((default-directory parent-dir))
        (call-process "git" nil nil nil "-c" "user.name=t" "-c" "user.email=t@t"
                      "merge" "-q" branch))
      ;; Not in main, but in the parent's branch, where it was meant to go.
      (switchboard-test--expire h)
      (should-not (memq h switchboard--sessions))
      (delete-directory parent-dir t))))

(ert-deftest switchboard-test-helper-uncommitted-changes-are-kept ()
  (switchboard-test--with-worktree-helper h s
    (let ((dir (switchboard-session-dir h)))
      (with-temp-file (expand-file-name "draft.txt" dir) (insert "draft"))
      (switchboard-test--expire h)
      (should (memq h switchboard--sessions))
      (should (file-exists-p (expand-file-name "draft.txt" dir))))))

(ert-deftest switchboard-test-sb-stop-keeps-unmerged-work ()
  (switchboard-test--with-worktree-helper h s
    (let ((dir (switchboard-session-dir h)))
      (switchboard-test--commit-in dir)
      (let ((out (switchboard-test--sb s "stop" "w")))
        (should (= (car out) 0))
        (should (string-match-p "Kept its worktree" (cdr out))))
      (should-not (memq h switchboard--sessions))
      (should (file-directory-p dir)))))

(ert-deftest switchboard-test-sb-worktree-warns-about-uncommitted-work ()
  (switchboard-test--with-helpers s
    (let* ((default-directory (switchboard-session-dir s))
           (switchboard-worktree-directory (make-temp-file "sb-test-worktrees" t)))
      (call-process "git" nil nil nil "init" "-q" "-b" "main")
      (call-process "git" nil nil nil "-c" "user.name=t" "-c" "user.email=t@t"
                    "commit" "-q" "--allow-empty" "-m" "init")
      (with-temp-file "uncommitted.txt" (insert "x"))
      (let ((out (switchboard-test--sb s "run" "--worktree" "--name" "w" "edit")))
        (should (string-match-p "uncommitted changes" (cdr out))))
      (should-not (file-exists-p
                   (expand-file-name "uncommitted.txt"
                                     (switchboard-session-dir (switchboard-test--helper "w")))))
      (delete-directory switchboard-worktree-directory t))))

(provide 'switchboard-test)
;;; switchboard-test.el ends here
