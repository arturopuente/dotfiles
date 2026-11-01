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

(ert-deftest switchboard-test-codex-notify-finishes ()
  (switchboard-test--with-session s
    (setf (switchboard-session-harness s) 'codex)
    (switchboard--set s 'working)
    (switchboard-test--hook "notify")
    (should (eq (switchboard-session-status s) 'ready))))

;; Codex: its hooks, once trusted, and notify until then

(defmacro switchboard-test--with-codex (var &rest body)
  "Like `switchboard-test--with-session', for a Codex session."
  (declare (indent 1))
  `(switchboard-test--with-session ,var
     (let ((switchboard--codex-hooked (make-hash-table :test 'equal)))
       (setf (switchboard-session-harness ,var) 'codex)
       ,@body)))

(ert-deftest switchboard-test-codex-without-hooks-notify-gives-the-resume-id ()
  (switchboard-test--with-codex s
    (switchboard--set s 'working)
    (switchboard-test--hook "notify" '((thread-id . "t1")))
    (should (eq (switchboard-session-status s) 'ready))
    (should (equal (switchboard-session-resume-id s) "t1"))))

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
    (let* ((home (make-temp-file "switchboard-codex" t))
           (rollout (expand-file-name "rollout.jsonl" home))
           (process-environment (cons (concat "CODEX_HOME=" home) process-environment)))
      (with-temp-file rollout
        (insert "{\"type\":\"event_msg\",\"payload\":{\"type\":\"token_count\",\"info\":{\"last_token_usage\":{\"input_tokens\":14265}}}}\n"
                "{\"type\":\"event_msg\",\"payload\":{\"type\":\"token_count\",\"info\":null}}\n"))
      (with-temp-file (expand-file-name "session_index.jsonl" home)
        (insert "{\"id\":\"t1\",\"thread_name\":\"Old\",\"updated_at\":\"x\"}\n"
                "{\"id\":\"t2\",\"thread_name\":\"Other\",\"updated_at\":\"x\"}\n"
                "{\"id\":\"t1\",\"thread_name\":\"Fix the parser\",\"updated_at\":\"x\"}\n"))
      (unwind-protect
          (progn
            (switchboard-test--hook "SessionStart"
                                    `((session_id . "t1") (source . "startup")
                                      (transcript_path . ,rollout)))
            (should (equal (switchboard-session-tokens s) 14265))
            (should (equal (switchboard-session-title s) "Fix the parser")))
        (delete-directory home t)))))

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
    (should (equal (switchboard--command s "-x first")
                   (list "pi" "-e" switchboard--pi-extension "--" "-x first")))
    (should (equal (switchboard--command s nil t)
                   (list "pi" "-e" switchboard--pi-extension "--continue")))
    (setf (switchboard-session-resume-id s) "01a197b5")
    (should (equal (switchboard--command s nil t)
                   (list "pi" "-e" switchboard--pi-extension "--session-id" "01a197b5")))))

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

(provide 'switchboard-test)
;;; switchboard-test.el ends here
