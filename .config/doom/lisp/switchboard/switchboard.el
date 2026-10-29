;;; switchboard.el --- A vertical list of Claude Code and Codex sessions -*- lexical-binding: t; -*-

;;; Commentary:

;; Each agent runs in its own ghostel terminal, usually in its own git
;; worktree. A side window lists them with their state:
;;
;;   WORKING    the agent is mid-turn
;;   NEEDS YOU  it is waiting on a permission prompt
;;   ERROR      the turn died on an API error
;;   STOPPED    you interrupted it (ESC)
;;   READY      it finished its turn and waits for the next prompt
;;   EXITED     the process is gone; r resumes it
;;
;; Most of that comes from the harness itself. Claude Code sessions get
;; hooks through `--settings' (so only Switchboard sessions report), Codex
;; gets a `notify' program, and both call switchboard-hook.sh, which calls
;; `switchboard-hook' through emacsclient. Two states have no event and
;; are inferred from the keys you send: ESC means STOPPED, and RET in a
;; Codex session means WORKING (Codex only reports when a turn ends).
;;
;; Entry point: M-x switchboard.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'server)
(require 'text-property-search)
(require 'svg)
(require 'transient)

(declare-function ghostel-exec "ghostel")
(declare-function ghostel-paste-string "ghostel")
(declare-function ghostel-send-key "ghostel")
(declare-function ghostel--adjust-size "ghostel")
(declare-function ghostel--anchor-window "ghostel")
(declare-function magit-status-setup-buffer "magit-status")
(declare-function evil-set-initial-state "evil-core")
(defvar ghostel-kill-buffer-on-exit)
(defvar ghostel-query-before-killing)
(defvar ghostel-shell)

(defgroup switchboard nil
  "A vertical list of agent sessions in ghostel terminals."
  :group 'tools
  :prefix "switchboard-")

(defcustom switchboard-worktree-directory "~/.worktrees/"
  "Where new worktrees go, as <this>/<repo>/<session-name>."
  :type 'directory)

(defcustom switchboard-sidebar-width 40
  "Width of the session list window, in columns."
  :type 'natnum)

(defcustom switchboard-names
  '("heron" "otter" "lynx" "wren" "moth" "ibex" "newt" "kite" "vole" "puffin"
    "stoat" "finch" "marten" "plover" "tern" "gecko" "egret" "shrike")
  "Default session names, used in order while free."
  :type '(repeat string))

(defcustom switchboard-compact nil
  "Non-nil shows one line per session instead of two."
  :type 'boolean)

(defcustom switchboard-notify '(waiting ready error exited)
  "Statuses that raise a macOS notification, unless you're watching the session.
`exited' only counts a crash or an error exit, not quitting it yourself."
  :type '(set (const :tag "Needs you" waiting)
              (const :tag "Finished its turn" ready)
              (const :tag "Errored" error)
              (const :tag "Exited unexpectedly" exited)))

(defcustom switchboard-remind-after 600
  "Seconds a session can need you before it notifies again, and again.
nil notifies once."
  :type '(choice (const :tag "Only once" nil) integer))

(defconst switchboard--hook-script
  (expand-file-name "switchboard-hook.sh"
                    (file-name-directory (or load-file-name buffer-file-name)))
  "The script Claude Code hooks and Codex notify call.")

(defconst switchboard--buffer-name "*switchboard*")


;;; Faces

(defface switchboard-working
  '((((background light)) :foreground "#0031a9") (t :foreground "#2fafff"))
  "Sessions that are mid-turn.")
(defface switchboard-waiting
  '((((background light)) :foreground "#721045" :weight bold)
    (t :foreground "#feacd0" :weight bold))
  "Sessions waiting on a permission prompt.")
(defface switchboard-error
  '((((background light)) :foreground "#a60000") (t :foreground "#ff5f59"))
  "Sessions whose turn failed on an API error.")
(defface switchboard-stopped
  '((((background light)) :foreground "#6f5500") (t :foreground "#d0bc00"))
  "Sessions you interrupted.")
(defface switchboard-ready
  '((((background light)) :foreground "#006800") (t :foreground "#44bc44"))
  "Sessions waiting for their next prompt.")
(defface switchboard-exited '((t :inherit shadow))
  "Sessions whose process has exited.")
(defface switchboard-row-waiting
  '((((background light)) :background "#f7e0f3" :extend t) (t :background "#3a1d36" :extend t))
  "Row background for sessions that need you.")
(defface switchboard-row-error
  '((((background light)) :background "#ffe0e0" :extend t) (t :background "#3d1010" :extend t))
  "Row background for errored sessions.")
(defface switchboard-row-working
  '((((background light)) :background "#e6ecfb" :extend t) (t :background "#15203a" :extend t))
  "Row background for sessions that are mid-turn.")
(defface switchboard-row-ready
  '((((background light)) :background "#e3f3e3" :extend t) (t :background "#13281a" :extend t))
  "Row background for sessions waiting for their next prompt.")
(defface switchboard-row-stopped
  '((((background light)) :background "#f5ecc8" :extend t) (t :background "#2e2a10" :extend t))
  "Row background for stopped sessions.")
(defface switchboard-header-focused
  '((((background light)) :background "#ffe58a") (t :background "#4d3f00"))
  "Background of the focused window's header, and of the selected row in the list.")
(defface switchboard-sidebar
  '((((background light)) :background "#f2f2f2") (t :background "#1e1e1e"))
  "Background of the session list.")



;;; Sessions

(cl-defstruct (switchboard-session (:constructor switchboard--session-create)
                                   (:copier nil))
  id name title named harness root dir branch buffer
  (status 'ready) unread marked task detail tokens resume-id transcript changes
  (changed (float-time)) (turn-start (float-time)))

(defvar switchboard--sessions nil
  "Live sessions, oldest first.")

(defvar-local switchboard--session nil
  "The session a terminal buffer belongs to.")
(put 'switchboard--session 'permanent-local t)

(defun switchboard--title (s)
  "What to call S: Claude's title for it, or your name, or its handle."
  (or (switchboard-session-title s) (switchboard-session-name s)))

(defun switchboard--find (id)
  (cl-find id switchboard--sessions :key #'switchboard-session-id :test #'equal))

(defun switchboard--project (s)
  (file-name-nondirectory (directory-file-name (switchboard-session-root s))))

(defun switchboard--set (s status)
  "Move S to STATUS, stamping when it changed."
  (unless (eq status (switchboard-session-status s))
    (when (eq status 'working)
      (setf (switchboard-session-turn-start s) (float-time)
            (switchboard-session-unread s) nil))
    (setf (switchboard-session-status s) status
          (switchboard-session-changed s) (float-time))
    (when (and (memq status '(waiting error)) (not (switchboard--watching-p s)))
      (message "Switchboard: %s %s" (switchboard--title s)
               (if (eq status 'waiting) "needs you" "hit an error")))
    (switchboard--notify s status)
    (switchboard--schedule-reminder s))
  (switchboard--schedule-render))

(defvar switchboard--reminders (make-hash-table :test 'equal)
  "Session id to the timer that will remind you it still needs you.")

(defun switchboard--schedule-reminder (s)
  "While S needs you, notify again every `switchboard-remind-after' seconds."
  (let ((id (switchboard-session-id s)))
    (when-let* ((timer (gethash id switchboard--reminders)))
      (cancel-timer timer)
      (remhash id switchboard--reminders))
    (when (and switchboard-remind-after (eq (switchboard-session-status s) 'waiting))
      (puthash id (run-at-time switchboard-remind-after nil #'switchboard--remind s)
               switchboard--reminders))))

(defun switchboard--remind (s)
  (remhash (switchboard-session-id s) switchboard--reminders)
  (when (and (memq s switchboard--sessions) (eq (switchboard-session-status s) 'waiting))
    (switchboard--notify s 'waiting t)
    (switchboard--schedule-reminder s)))

(defun switchboard--watching-p (s)
  "Non-nil if S is in the selected window."
  (eq (window-buffer (selected-window)) (switchboard-session-buffer s)))

(defvar switchboard--notified nil
  "Ids of sessions with a terminal-notifier notification that may still be up.")

(defun switchboard--notify (s status &optional again)
  "Raise a macOS notification that S moved to STATUS, if you aren't watching it.
AGAIN says it has been in STATUS a while: a reminder.
With terminal-notifier, clicking it brings Emacs back on S; osascript
can only post it."
  (when (memq status switchboard-notify)
    (unless (and (switchboard--watching-p s) (eq (frame-focus-state) t))
      (let ((title (switchboard--title s))
            (subtitle (pcase status
                        ('waiting (if again
                                      (format "Still needs you, for %s" (switchboard--age s))
                                    "Needs you")) ('ready "Finished") ('error "Errored")
                        ('exited "Exited")))
            (body (or (switchboard-session-detail s) (switchboard--project s))))
        (cond
         ((executable-find "terminal-notifier")
          (push (switchboard-session-id s) switchboard--notified)
          ;; A leading [ or - would be read as an option, so escape it.
          (start-process "switchboard-notify" nil "terminal-notifier"
                         "-title" title "-subtitle" subtitle
                         "-message" (replace-regexp-in-string "\\`[[-]" "\\\\\\&" body)
                         ;; One per session: a newer one replaces the last.
                         "-group" (switchboard--notification-group s)
                         "-activate" "org.gnu.Emacs"
                         "-execute" (mapconcat #'shell-quote-argument
                                               (list (or (executable-find "emacsclient") "emacsclient")
                                                     "-s" (expand-file-name server-name server-socket-dir)
                                                     "-n" "--eval"
                                                     (format "(switchboard--notification-clicked %S)"
                                                             (switchboard-session-id s)))
                                               " ")))
         ((executable-find "osascript")
          ;; Text goes in as arguments, so quotes in it need no escaping.
          (start-process "switchboard-notify" nil "osascript"
                         "-e" "on run argv"
                         "-e" "display notification (item 3 of argv) with title (item 1 of argv) subtitle (item 2 of argv)"
                         "-e" "end run"
                         title subtitle body)))))))

(defun switchboard--notification-group (s)
  (concat "switchboard-" (switchboard-session-id s)))

(defun switchboard--clear-notification (s)
  "Take S's notification out of Notification Center: you've seen it now."
  (when (member (switchboard-session-id s) switchboard--notified)
    (setq switchboard--notified (delete (switchboard-session-id s) switchboard--notified))
    (when (executable-find "terminal-notifier")
      (start-process "switchboard-notify" nil "terminal-notifier"
                     "-remove" (switchboard--notification-group s)))))

(defun switchboard--notification-clicked (id)
  "Open session ID, with the sidebar on its row: a notification was clicked.
If it has exited since, offer to resume it."
  (when-let* ((s (switchboard--find id)))
    (select-frame-set-input-focus (window-frame (switchboard--main-window)))
    (switchboard--display-sidebar)
    (if (buffer-live-p (switchboard-session-buffer s))
        (switchboard--show s t)
      (switchboard--select-row s))
    (when (eq (switchboard-session-status s) 'exited)
      ;; Ask once emacsclient's request is done, not from inside it.
      (run-at-time 0 nil (lambda ()
                           (when (y-or-n-p (format "%s has exited.  Resume it? "
                                                   (switchboard--title s)))
                             (switchboard-resume s))))))
  nil)


;;; Starting sessions

(defun switchboard--free-name ()
  (or (cl-find-if-not (lambda (n) (cl-find n switchboard--sessions
                                           :key #'switchboard-session-name :test #'equal))
                      switchboard-names)
      (format "agent-%d" (1+ (length switchboard--sessions)))))

(defun switchboard--read-project ()
  (require 'project)
  (let* ((here (and (fboundp 'doom-project-root) (doom-project-root)))
         (roots (delete-dups
                 (mapcar (lambda (d) (abbreviate-file-name (file-name-as-directory d)))
                         (append (and here (list here))
                                 (and (fboundp 'projectile-relevant-known-projects)
                                      (projectile-relevant-known-projects))
                                 (project-known-project-roots))))))
    ;; A directory prompt, so typing completes paths.  It starts in the
    ;; current project's parent, where its siblings are; RET as is takes the
    ;; current project, and M-n steps through the other known ones.
    (file-name-as-directory
     (expand-file-name
      (read-directory-name (format-prompt "Project" (car roots))
                           (and here (file-name-directory (directory-file-name here)))
                           roots t)))))

(defun switchboard--git (dir &rest args)
  "Run git with ARGS in DIR; return trimmed stdout, or nil on failure."
  (let ((default-directory dir))
    (with-temp-buffer
      (when (zerop (apply #'call-process "git" nil t nil args))
        (string-trim (buffer-string))))))

(defun switchboard--add-worktree (root name &optional start)
  "Create (or reuse) a worktree for NAME off ROOT.  Return (DIR . BRANCH).
A new branch starts from START, or ROOT's checked-out commit."
  (let* ((repo (file-name-nondirectory (directory-file-name root)))
         (dir (file-name-as-directory
               (expand-file-name (concat repo "/" name) switchboard-worktree-directory)))
         (branch (concat "sb/" name)))
    (unless (file-directory-p dir)
      (make-directory (file-name-directory (directory-file-name dir)) t)
      (let ((args (if (switchboard--git root "rev-parse" "--verify" "--quiet"
                                        (concat "refs/heads/" branch))
                      (list "worktree" "add" dir branch)
                    (append (list "worktree" "add" "-b" branch dir)
                            (and start (list start))))))
        (unless (apply #'switchboard--git root args)
          (user-error "git %s failed" (string-join args " ")))))
    (cons dir branch)))

(defun switchboard--claude-settings ()
  "Write the hooks file passed to Claude Code with --settings; return its path."
  (let* ((file (expand-file-name (locate-user-emacs-file "switchboard/claude-settings.json")))
         (hook (lambda (event &optional matcher)
                 `[(,@(and matcher `(:matcher ,matcher))
                    :hooks [(:type "command" :timeout 5
                             :command ,(format "/bin/sh %s claude %s"
                                               (shell-quote-argument switchboard--hook-script)
                                               event))])]))
         (hooks (list :SessionStart (funcall hook "SessionStart")
                      :UserPromptSubmit (funcall hook "UserPromptSubmit")
                      :PreToolUse (funcall hook "PreToolUse")
                      :PostToolUse (funcall hook "PostToolUse")
                      :PostToolUseFailure (funcall hook "PostToolUseFailure")
                      :PermissionRequest (funcall hook "PermissionRequest")
                      :Stop (funcall hook "Stop")
                      :StopFailure (funcall hook "StopFailure"))))
    (make-directory (file-name-directory file) t)
    (with-temp-file file
      (insert (json-serialize (list :hooks hooks))))
    file))

(defun switchboard--claude-background-id (s)
  "Return the `claude attach' id of S if it runs in the background.
A session sent to the background (Ctrl-B) outlives the process in its
terminal, and `claude --resume' refuses it while it runs.  Its session
id can differ from S's resume id, so it is also known by the
SWITCHBOARD_ID it took with it from S's terminal."
  (when-let* ((agents (ignore-errors
                        (with-temp-buffer
                          (call-process "claude" nil '(t nil) nil "agents" "--json")
                          (json-parse-string (buffer-string) :object-type 'alist
                                             :array-type 'list))))
              (mark (format "SWITCHBOARD_ID=%s\\_>" (regexp-quote (switchboard-session-id s))))
              (agent (cl-find-if
                      (lambda (a)
                        (and (equal (alist-get 'kind a) "background")
                             (or (equal (alist-get 'sessionId a)
                                        (switchboard-session-resume-id s))
                                 (when-let* ((pid (alist-get 'pid a)))
                                   (string-match-p
                                    mark (shell-command-to-string
                                          (format "ps eww -o command= -p %d" pid)))))))
                      agents)))
    (or (alist-get 'id agent) (alist-get 'sessionId agent))))

(defun switchboard--command (s &optional prompt resume)
  "Return the argv that starts S, or resumes it when RESUME is non-nil."
  (pcase (switchboard-session-harness s)
    ('claude
     (if-let* ((bg (and resume (switchboard--claude-background-id s))))
         (list "claude" "attach" bg)
       ;; No -n: Claude then titles the session itself (see `switchboard--ai-title').
       (append (list "claude" "--settings" (switchboard--claude-settings))
               (cond ((and resume (switchboard-session-resume-id s))
                      (list "--resume" (switchboard-session-resume-id s)))
                     (resume (list "--continue")))
               (and prompt (list prompt)))))
    ('codex
     (append (list "codex")
             (and resume (list "resume" "--last"))
             (list "-c" (format "notify=[\"/bin/sh\", %S, \"codex\"]"
                                switchboard--hook-script))
             (and prompt (list prompt))))))

(defconst switchboard--inherited-env
  (rx bos (or "CLAUDECODE" "CLAUDE_PID" "CLAUDE_EFFORT" "AI_AGENT"
              (seq "CLAUDE_CODE_" (or "ENTRYPOINT" "SESSION_ID" "CHILD_SESSION"
                                      "SESSION_ATTENDED" "MESSAGING_TOKEN" "EXECPATH")))
      "=")
  "Variables a Claude Code session sets for its children.
If Emacs was started from inside one, a new session must not inherit
them: claude would think it is nested, and turns off transcripts.")

(defun switchboard--spawn (s &optional prompt resume)
  "Run S's harness in its buffer, creating the buffer if needed."
  (require 'ghostel)
  (unless (server-running-p) (server-start))
  (let* ((buf (or (and (buffer-live-p (switchboard-session-buffer s))
                       (switchboard-session-buffer s))
                  (generate-new-buffer (format "*sb:%s*" (switchboard--title s)))))
         (argv (switchboard--command s prompt resume))
         (program (or (executable-find (car argv))
                      (user-error "%s is not on exec-path" (car argv))))
         (default-directory (switchboard-session-dir s))
         (process-environment
          (append (list (concat "SWITCHBOARD_ID=" (switchboard-session-id s))
                        (concat "SWITCHBOARD_SOCKET="
                                (expand-file-name server-name server-socket-dir))
                        (concat "SWITCHBOARD_EMACSCLIENT="
                                (or (executable-find "emacsclient") "emacsclient")))
                  (cl-remove-if (lambda (v) (string-match-p switchboard--inherited-env v))
                                process-environment))))
    (with-current-buffer buf
      (setq default-directory (switchboard-session-dir s)))
    (setf (switchboard-session-buffer s) buf)
    ;; Show it first so ghostel sizes the terminal to the real window.
    (switchboard--show s t)
    (ghostel-exec buf program (cdr argv))
    (with-current-buffer buf
      (setq-local switchboard--session s
                  ghostel-kill-buffer-on-exit nil
                  header-line-format '(:eval switchboard--terminal-header-image)
                  ;; ESC interrupts the agent.  evil-ghostel's `auto' only sends
                  ;; it to full-screen apps, and Claude Code isn't one, so ESC
                  ;; switched evil to normal state instead.  C-c C-r toggles back.
                  evil-ghostel--escape-mode 'terminal))
    (switchboard--schedule-render)
    (when prompt
      (setf (switchboard-session-task s) prompt)
      (switchboard--set s 'working))
    buf))

(defun switchboard--start (harness root prompt worktree &optional title dir)
  "Start a HARNESS session in project ROOT, sending PROMPT first.
With WORKTREE non-nil it gets its own worktree, on a branch named after
a free handle from `switchboard-names', starting from the branch
WORKTREE names if it's a string; otherwise it runs in DIR, or ROOT.
Claude titles the session once its first turn ends; TITLE pins a name
of your own instead."
  (setq title (and title (not (string-blank-p title)) title))
  (pcase-let* ((name (switchboard--free-name))
               (`(,dir . ,branch)
                (if worktree
                    (switchboard--add-worktree root name (and (stringp worktree) worktree))
                  (let ((dir (or dir root)))
                    (cons dir (switchboard--git dir "branch" "--show-current")))))
               (s (switchboard--session-create
                   :id (format "%s-%04x" name (random #xffff))
                   :name name :title title :named (and title t)
                   :harness harness :root root :dir dir :branch branch)))
    (setq switchboard--sessions (append switchboard--sessions (list s)))
    (switchboard--save-soon)
    (switchboard--display-sidebar)
    (switchboard--spawn s (and prompt (not (string-empty-p prompt)) prompt))
    s))

;; A menu like magit's, so a session starts with one key: pick the
;; options first if you want any, then press the harness.

(defun switchboard--new-scope ()
  "(ROOT DIR BRANCH) for a new session: the row at point's, or this project."
  (let ((here (and (eq (current-buffer) (get-buffer switchboard--buffer-name))
                   (get-text-property (point) 'switchboard-session)))
        (project (and (fboundp 'doom-project-root) (doom-project-root))))
    (cond (here (list (switchboard-session-root here) (switchboard-session-dir here)
                      (switchboard-session-branch here)))
          (project (list (file-name-as-directory (expand-file-name project)))))))

(defun switchboard--new-description ()
  (let ((scope (transient-scope)))
    (format "New session in %s"
            (propertize (if scope (abbreviate-file-name (or (nth 1 scope) (car scope)))
                          "a project you pick")
                        'face 'transient-value))))

(defun switchboard--read-project-option (&rest _)
  (abbreviate-file-name (switchboard--read-project)))

(defun switchboard--new-with (harness)
  (let* ((args (transient-args 'switchboard-new))
         (scope (transient-scope 'switchboard-new))
         (other (transient-arg-value "--project=" args))
         (root (if other
                   (file-name-as-directory (expand-file-name other))
                 (or (car scope) (switchboard--read-project))))
         (from (and (not other) (transient-arg-value "--from-branch" args) (nth 2 scope)))
         (worktree (or from (transient-arg-value "--worktree" args))))
    (when (and worktree (not (switchboard--git root "rev-parse" "--git-dir")))
      (user-error "%s is not a git repository" (abbreviate-file-name root)))
    (switchboard--start harness root (transient-arg-value "--prompt=" args) worktree
                        (transient-arg-value "--name=" args) (and (not other) (nth 1 scope)))))

(transient-define-suffix switchboard-new-claude ()
  "Start a Claude Code session."
  (interactive)
  (switchboard--new-with 'claude))

(transient-define-suffix switchboard-new-codex ()
  "Start a Codex session."
  (interactive)
  (switchboard--new-with 'codex))

;;;###autoload (autoload 'switchboard-new "switchboard" nil t)
(transient-define-prefix switchboard-new ()
  "Start a new session, in the project and folder of the row at point."
  [:description switchboard--new-description
   ("-w" "New worktree" "--worktree")
   ("-b" "New worktree from its branch" "--from-branch"
    :if (lambda () (nth 2 (transient-scope))))
   ("-n" "Name" "--name=")
   ("-p" "First prompt" "--prompt=")
   ("-P" "Other project" "--project=" :reader switchboard--read-project-option)]
  ["Start"
   ("c" "Claude" switchboard-new-claude)
   ("x" "Codex" switchboard-new-codex)]
  (interactive)
  (transient-setup 'switchboard-new nil nil :scope (switchboard--new-scope)))


;;; Events from the harness

(defun switchboard--read-payload (file)
  "Parse and delete the JSON payload FILE, if any."
  (when (and file (not (string-empty-p file)))
    (unwind-protect
        (ignore-errors
          (with-temp-buffer
            (insert-file-contents file)
            (json-parse-buffer :object-type 'alist :null-object nil :false-object nil)))
      (ignore-errors (delete-file file)))))

(defun switchboard--context-tokens (transcript)
  "Tokens in context as of the last assistant message in TRANSCRIPT."
  (when (and transcript (file-readable-p transcript))
    (with-temp-buffer
      (let ((size (file-attribute-size (file-attributes transcript))))
        (insert-file-contents transcript nil (max 0 (- size 262144)) size))
      (goto-char (point-max))
      (catch 'found
        (while (search-backward "\"usage\"" nil t)
          (let ((usage (ignore-errors
                         (alist-get 'usage
                                    (alist-get 'message
                                               (json-parse-string
                                                (buffer-substring (line-beginning-position)
                                                                  (line-end-position))
                                                :object-type 'alist :null-object nil))))))
            (when usage
              (throw 'found (+ (or (alist-get 'input_tokens usage) 0)
                               (or (alist-get 'cache_read_input_tokens usage) 0)
                               (or (alist-get 'cache_creation_input_tokens usage) 0)))))
          (beginning-of-line))
        nil))))

(defun switchboard--ai-title (transcript)
  "The latest title Claude Code gave the session in TRANSCRIPT, if any."
  (when (and transcript (file-readable-p transcript))
    (with-temp-buffer
      (let ((size (file-attribute-size (file-attributes transcript))))
        (insert-file-contents transcript nil (max 0 (- size 262144)) size))
      (goto-char (point-max))
      (when (search-backward "\"type\":\"ai-title\"" nil t)
        (ignore-errors
          (alist-get 'aiTitle (json-parse-string
                               (buffer-substring (line-beginning-position) (line-end-position))
                               :object-type 'alist :null-object nil)))))))

(defun switchboard--update-tokens (s payload)
  "Refresh S's token count and, unless you named it, its title."
  (when-let* ((path (alist-get 'transcript_path payload)))
    (setf (switchboard-session-transcript s) path))
  (when-let* ((tokens (switchboard--context-tokens (switchboard-session-transcript s))))
    (setf (switchboard-session-tokens s) tokens))
  (unless (switchboard-session-named s)
    (let ((title (switchboard--ai-title (switchboard-session-transcript s))))
      (when (and title (not (equal title (switchboard-session-title s))))
        (setf (switchboard-session-title s) title)
        (switchboard--rename-buffer s)
        (switchboard--save-soon)))))

(defun switchboard--rename-buffer (s)
  (when (buffer-live-p (switchboard-session-buffer s))
    (with-current-buffer (switchboard-session-buffer s)
      (rename-buffer (format "*sb:%s*" (switchboard--title s)) t))))

(defvar switchboard--git-procs (make-hash-table :test 'equal)
  "Session id to the git process reading its branch and changes.")

(defconst switchboard--git-script
  (concat "git branch --show-current 2>/dev/null; echo @@;"
          "git diff HEAD --numstat 2>/dev/null; echo @@;"
          "git ls-files --others --exclude-standard 2>/dev/null")
  "One shell run that prints the branch, the diff and new files, split by @@.")

(defun switchboard--update-git (s)
  "Re-read S's branch, in case the agent checked out another one, and
its uncommitted changes.  Git runs in the background, so a big repo
doesn't stall Emacs; asked again while it runs, it runs once more."
  (let* ((id (switchboard-session-id s))
         (running (gethash id switchboard--git-procs))
         (dir (switchboard-session-dir s)))
    (cond
     ((process-live-p running) (process-put running 'again t))
     ((file-directory-p dir)
      (let ((default-directory dir))
        (puthash id
                 (make-process
                  :name "switchboard-git" :noquery t :connection-type 'pipe
                  :buffer (generate-new-buffer " *switchboard-git*")
                  :command (list "sh" "-c" switchboard--git-script)
                  :sentinel (lambda (proc _event)
                              (unless (process-live-p proc)
                                (switchboard--git-done s proc))))
                 switchboard--git-procs))))))

(defvar switchboard--git-timers (make-hash-table :test 'equal)
  "Session id to the timer that will refresh its git state.")

(defun switchboard--update-git-soon (s)
  "Refresh S's git state once its tool calls pause for a couple of seconds.
A turn can make dozens of edits; this reads git once per burst."
  (let ((id (switchboard-session-id s)))
    (when-let* ((timer (gethash id switchboard--git-timers)))
      (cancel-timer timer))
    (puthash id (run-at-time 2 nil (lambda ()
                                     (remhash id switchboard--git-timers)
                                     (when (memq s switchboard--sessions)
                                       (switchboard--update-git s))))
             switchboard--git-timers)))

(defun switchboard--git-done (s proc)
  (let ((output (with-current-buffer (process-buffer proc) (buffer-string))))
    (kill-buffer (process-buffer proc))
    (remhash (switchboard-session-id s) switchboard--git-procs)
    (when (memq s switchboard--sessions)
      (pcase-let ((`(,branch . ,changes) (switchboard--parse-git output)))
        (when (and branch (not (equal branch (switchboard-session-branch s))))
          (setf (switchboard-session-branch s) branch)
          (switchboard--save-soon))
        (unless (equal changes (switchboard-session-changes s))
          (setf (switchboard-session-changes s) changes)
          (switchboard--schedule-render)))
      (when (process-get proc 'again)
        (switchboard--update-git s)))))

(defun switchboard--parse-git (output)
  "Parse `switchboard--git-script' OUTPUT into (BRANCH . CHANGES).
BRANCH is nil when detached or not a repository; CHANGES is
(ADDED DELETED UNTRACKED), or nil if there are none."
  (pcase-let* ((`(,branch ,diff ,others) (split-string output "^@@\n"))
               (branch (string-trim (or branch "")))
               (added 0) (deleted 0)
               (untracked (length (split-string (or others "") "\n" t))))
    ;; Binary files show as "-	-".
    (dolist (line (split-string (or diff "") "\n" t))
      (pcase (split-string line "\t")
        (`(,a ,d . ,_) (cl-incf added (string-to-number a)) (cl-incf deleted (string-to-number d)))))
    (cons (and (not (string-empty-p branch)) branch)
          (and (or (> added 0) (> deleted 0) (> untracked 0))
               (list added deleted untracked)))))

(defun switchboard--finish-turn (s status)
  (setf (switchboard-session-unread s) (not (switchboard--watching-p s)))
  (switchboard--set s status))

;;;###autoload
(defun switchboard-hook (id event &optional file)
  "Record EVENT for session ID; FILE holds its JSON payload.
Called by switchboard-hook.sh through emacsclient."
  (let ((payload (switchboard--read-payload file))
        (s (switchboard--find id)))
    (when s
      (pcase event
        ("SessionStart"
         (setf (switchboard-session-resume-id s) (alist-get 'session_id payload))
         (switchboard--update-git s)
         (switchboard--update-tokens s payload)
         (switchboard--save-soon))
        ("UserPromptSubmit"
         (unless (switchboard-session-task s)
           (setf (switchboard-session-task s) (alist-get 'prompt payload))
           (switchboard--save-soon))
         (setf (switchboard-session-detail s) nil)
         (switchboard--update-tokens s payload)
         (switchboard--set s 'working))
        ((or "PreToolUse" "PostToolUse" "PostToolUseFailure")
         (when (eq (switchboard-session-status s) 'waiting)
           (setf (switchboard-session-detail s) nil))
         (when (equal event "PostToolUse")
           (switchboard--update-git-soon s))
         (switchboard--set s 'working))
        ("PermissionRequest"
         (let ((input (alist-get 'tool_input payload)))
           (setf (switchboard-session-detail s)
                 (format "%s: %s" (alist-get 'tool_name payload)
                         (or (alist-get 'command input) (alist-get 'file_path input)
                             (alist-get 'url input) ""))))
         (switchboard--set s 'waiting))
        ("Stop"
         (setf (switchboard-session-detail s) nil)
         (switchboard--update-git s)
         (switchboard--update-tokens s payload)
         (switchboard--finish-turn s 'ready))
        ("StopFailure"
         (setf (switchboard-session-detail s)
               (let ((e (or (alist-get 'error payload) (alist-get 'error_type payload)
                            (alist-get 'message payload))))
                 (if (stringp e) e "API error")))
         (switchboard--finish-turn s 'error))
        ("notify"                       ; Codex: a turn finished
         (setf (switchboard-session-detail s) nil)
         (switchboard--finish-turn s 'ready)))))
  nil)

;; No event marks an interrupt, and Codex has none for a turn starting, so
;; watch what goes to the terminal. These are ghostel internals: every key
;; ends up in one of them, whichever keymap or evil state sent it.
(defun switchboard--note-key (key-name &rest _)
  (switchboard--note-input (pcase key-name ("escape" 'escape) ("return" 'return))))

(defun switchboard--note-string (string &rest _)
  (switchboard--note-input (pcase string ("\e" 'escape) ("\r" 'return))))

(defun switchboard--note-input (key)
  (when-let* ((s switchboard--session))
    (let ((status (switchboard-session-status s)))
      (cond ((and (eq key 'escape) (memq status '(working waiting)))
             (switchboard--set s 'stopped))
            ((and (eq key 'return) (eq (switchboard-session-harness s) 'codex)
                  (memq status '(ready stopped error)))
             (switchboard--set s 'working))))))

(defun switchboard--on-exit (buf event)
  ;; Killing the buffer (x) doesn't get here, ghostel only calls this for
  ;; a live one; a clean exit is /exit or Ctrl-D, so only a failure notifies.
  (when-let* ((s (buffer-local-value 'switchboard--session buf)))
    (let ((switchboard-notify (if (string-prefix-p "finished" event)
                                  (remq 'exited switchboard-notify)
                                switchboard-notify)))
      (unless (string-prefix-p "finished" event)
        (setf (switchboard-session-detail s) (string-trim event)))
      (switchboard--set s 'exited)))
  ;; A drawer whose shell exited closes its split.
  (when (buffer-local-value 'switchboard--drawer-of buf)
    (dolist (w (get-buffer-window-list buf nil t))
      (ignore-errors (delete-window w)))))

(with-eval-after-load 'ghostel
  (advice-add 'ghostel--send-encoded :before #'switchboard--note-key)
  (advice-add 'ghostel--send-string :before #'switchboard--note-string)
  (add-hook 'ghostel-exit-functions #'switchboard--on-exit))

(defun switchboard--mark-seen (&rest _)
  "Clear the unread flag of the session in the selected window."
  (when-let* ((s (buffer-local-value 'switchboard--session (window-buffer (selected-window)))))
    (when (switchboard-session-unread s)
      (setf (switchboard-session-unread s) nil)
      (switchboard--schedule-render))))

(defun switchboard--on-select (&rest _)
  "Redraw so the headers and the list's selection show where focus is."
  (when switchboard--sessions (switchboard--schedule-render)))

(add-hook 'window-selection-change-functions #'switchboard--mark-seen)
(add-hook 'window-selection-change-functions #'switchboard--on-select)
(defun switchboard--on-resize (_frame)
  (when switchboard--sessions (switchboard--schedule-render)))

(add-hook 'window-size-change-functions #'switchboard--on-resize)
(add-hook 'window-buffer-change-functions #'switchboard--mark-seen)


;;; Saving

(defcustom switchboard-save-file
  (expand-file-name (locate-user-emacs-file "switchboard/sessions.eld"))
  "Where the session list is saved between Emacs runs."
  :type 'file)

(defconst switchboard--saved-slots
  '(id name title named harness root dir branch task resume-id transcript tokens)
  "The session fields worth keeping; status and buffers are not.")

(defvar switchboard--save-timer nil)

(defun switchboard--save ()
  "Write the session list to `switchboard-save-file'."
  (setq switchboard--save-timer nil)
  (make-directory (file-name-directory switchboard-save-file) t)
  (with-temp-file switchboard-save-file
    (let ((print-length nil) (print-level nil))
      (prin1 (mapcar (lambda (s)
                       (mapcan (lambda (slot)
                                 (list (intern (format ":%s" slot))
                                       (cl-struct-slot-value 'switchboard-session slot s)))
                               switchboard--saved-slots))
                     switchboard--sessions)
             (current-buffer)))))

(defun switchboard--save-soon ()
  (unless switchboard--save-timer
    (setq switchboard--save-timer (run-with-idle-timer 1 nil #'switchboard--save))))

(add-hook 'kill-emacs-hook (lambda () (when switchboard--sessions (switchboard--save))))

(defun switchboard--load ()
  "Add saved sessions that aren't already listed, as EXITED.
Return the ones added.  Sessions whose directory is gone are dropped."
  (when (file-readable-p switchboard-save-file)
    (let ((saved (with-temp-buffer
                   (insert-file-contents switchboard-save-file)
                   (ignore-errors (read (current-buffer)))))
          added)
      (dolist (plist saved)
        (let ((s (apply #'switchboard--session-create :status 'exited plist)))
          (cond ((switchboard--find (switchboard-session-id s)))
                ((not (file-directory-p (switchboard-session-dir s)))
                 (message "Switchboard: dropping %s, %s is gone" (switchboard--title s)
                          (switchboard-session-dir s)))
                (t (push s added)))))
      (setq added (nreverse added)
            switchboard--sessions (append switchboard--sessions added))
      added)))

;;;###autoload
(defun switchboard-resume-all ()
  "Restore saved sessions, then restart every exited one.
Claude sessions continue their conversation; Codex resumes its last one."
  (interactive)
  (switchboard--load)
  (let ((exited (cl-remove-if-not (lambda (s) (eq (switchboard-session-status s) 'exited))
                                  switchboard--sessions)))
    (if (not exited)
        (message "No exited sessions to resume")
      (switchboard--display-sidebar)
      (dolist (s exited) (switchboard-resume s))
      (message "Resumed %d session%s" (length exited) (if (cdr exited) "s" "")))))


;;; The list

(defvar switchboard--group 'project "Group the list by `project' or `status'.")
(defvar switchboard--folded nil "Group keys that are folded.")
(defvar switchboard--filter nil "Only list sessions matching this string.")
(defvar switchboard--render-timer nil)
(defvar switchboard--tick-timer nil)

(defconst switchboard--status-order '(waiting error stopped unread working ready exited))
(defconst switchboard--status-titles
  '((waiting . "Needs you") (error . "Errored") (stopped . "Stopped") (unread . "Finished")
    (working . "Working") (ready . "Ready") (exited . "Exited")))

(defun switchboard--status-key (s)
  (if (and (eq (switchboard-session-status s) 'ready) (switchboard-session-unread s))
      'unread
    (switchboard-session-status s)))

(defun switchboard--face (s)
  (intern (format "switchboard-%s" (switchboard-session-status s))))

(defun switchboard--label (s)
  (pcase (switchboard-session-status s)
    ('working "WORKING") ('waiting "NEEDS YOU") ('error "ERROR")
    ('stopped "STOPPED") ('exited "EXITED")
    (_ (if (switchboard-session-unread s) "READY · NEW" "READY"))))

(defun switchboard--age (s)
  (let ((sec (floor (- (float-time) (switchboard-session-changed s)))))
    (cond ((< sec 60) (format "%ds" sec))
          ((< sec 3600) (format "%dm" (/ sec 60)))
          (t (format "%dh" (/ sec 3600))))))

(defun switchboard--tokens (s)
  (let ((n (switchboard-session-tokens s)))
    (cond ((null n) "")
          ((< n 1000) (number-to-string n))
          (t (format "%dk" (round n 1000))))))

(defun switchboard--matches-p (s)
  (or (null switchboard--filter)
      (cl-every (lambda (word)
                  (string-match-p (regexp-quote word)
                                  (format "%s %s %s %s %s %s"
                                          (switchboard-session-name s)
                                          (or (switchboard-session-title s) "")
                                          (switchboard--project s)
                                          (switchboard-session-harness s)
                                          (switchboard-session-status s)
                                          (or (switchboard-session-task s) ""))))
                (split-string switchboard--filter))))

(defun switchboard--groups ()
  "Return ((KEY TITLE . SESSIONS) ...) in display order."
  (let ((sessions (cl-remove-if-not #'switchboard--matches-p switchboard--sessions)))
    (if (eq switchboard--group 'status)
        (cl-loop for key in switchboard--status-order
                 for members = (cl-remove-if-not (lambda (s) (eq (switchboard--status-key s) key))
                                                 sessions)
                 when members
                 collect (cons key (cons (alist-get key switchboard--status-titles) members)))
      (let (groups)
        (dolist (s sessions)
          (let* ((key (switchboard--project s))
                 (group (assoc key groups)))
            (if group
                (setcdr (cdr group) (append (cddr group) (list s)))
              (push (cons key (cons key (list s))) groups))))
        (nreverse groups)))))

(defun switchboard--shown-p (key s)
  "Folded groups still show sessions that need you or errored."
  (or (not (member key switchboard--folded))
      (memq (switchboard-session-status s) '(waiting error))))

(defun switchboard--visible ()
  "Sessions in list order, as numbered by 1-9."
  (cl-loop for (key _title . members) in (switchboard--groups)
           append (cl-remove-if-not (lambda (s) (switchboard--shown-p key s)) members)))

;;; Drawing
;;
;; The list, its header and each terminal's header are SVG images, styled
;; like the HTML prototype. Text properties can't do rounded corners, boxes
;; sized to their text, or several sizes on one line. Each row is a single
;; image on its own line, so point and line-based keys keep working.

(defcustom switchboard-font nil
  "Font family list for the drawn list; nil uses the default face's family."
  :type '(choice (const :tag "Default face" nil) string))

(defun switchboard--font-family ()
  (or switchboard-font
      (concat (face-attribute 'default :family nil t) ", monospace")))

(defvar switchboard--header-image nil
  "The list's header line, drawn by `switchboard--render'.")

(defvar-local switchboard--terminal-header-image nil
  "This terminal's header line, drawn by `switchboard--render'.")

(defun switchboard--font-size ()
  "Pixel size of the list's main text: 85% of the default font."
  (round (* 0.85 (or (ignore-errors (aref (font-info (face-font 'default)) 2)) 19))))

(defun switchboard--color (face &optional background)
  (or (if background (face-background face nil t) (face-foreground face nil t))
      (if background "#ffffff" "#000000")))

(defun switchboard--palette ()
  (list :bg (switchboard--color 'switchboard-sidebar t)
        :paper (switchboard--color 'default t)
        :fg (switchboard--color 'default)
        :dim (switchboard--color 'shadow)
        :blue (switchboard--color 'switchboard-working)
        :claude (switchboard--color 'font-lock-builtin-face)
        :codex (switchboard--color 'font-lock-constant-face)))

(defun switchboard--text-width (string size)
  "Width of STRING at SIZE pixels, assuming a monospaced font."
  (* 0.6 size (string-width string)))

(defun switchboard--clip (string size pixels)
  "STRING cut with an ellipsis to fit PIXELS at SIZE."
  (let ((chars (floor (/ (max 0 pixels) (* 0.6 size)))))
    (if (<= (string-width string) chars)
        string
      (concat (truncate-string-to-width string (max 0 (1- chars))) "…"))))

(defun switchboard--svg-text (svg string x y size fill &rest props)
  (apply #'svg-text svg string :x x :y y :font-size size :fill fill
         :font-family (switchboard--font-family) props))

(defun switchboard--image (svg)
  ;; :scale 1, or `image-scaling-factor' enlarges it with big fonts.
  (propertize " " 'display (svg-image svg :scale 1 :ascent 'center)))

(defun switchboard--draw-chip (svg s x y w h pal)
  "The status square: filled, or outlined for a READY you've seen."
  (let* ((status (switchboard-session-status s))
         (color (switchboard--color (switchboard--face s)))
         (outline (and (memq status '(ready exited)) (not (switchboard-session-unread s))))
         (ink (if outline color (plist-get pal :paper)))
         (cx (+ x (/ w 2.0)))
         (cy (+ y (/ h 2.0)))
         (glyph (lambda (string)
                  (switchboard--svg-text svg string cx (+ cy (* 0.36 h)) (round (* 0.82 h)) ink
                                         :text-anchor "middle" :font-weight "bold"))))
    (if outline
        (svg-rectangle svg (+ x 0.75) (+ y 0.75) (- w 1.5) (- h 1.5) :rx 4
                       :fill "none" :stroke color :stroke-width 1.5)
      (svg-rectangle svg x y w h :rx 4 :fill color))
    (pcase status
      ('working
       ;; A spinning arc; each redraw advances it.
       (let ((r (* 0.27 h)))
         (svg-circle svg cx cy r :fill "none" :stroke ink :stroke-width 2 :stroke-opacity 0.35)
         (svg-circle svg cx cy r :fill "none" :stroke ink :stroke-width 2
                     :stroke-linecap "round"
                     :stroke-dasharray (format "%.1f 100" (* 1.7 r))
                     :transform (format "rotate(%d %.1f %.1f)"
                                        (mod (round (* 270 (float-time))) 360) cx cy))))
      ('stopped
       (let ((side (* 0.36 h)))
         (svg-rectangle svg (- cx (/ side 2)) (- cy (/ side 2)) side side :rx 1.5 :fill ink)))
      ('waiting (funcall glyph "?"))
      ('error (funcall glyph "!"))
      ('exited (funcall glyph "×"))
      (_ (if (switchboard-session-unread s)
             (funcall glyph "✓")
           (svg-circle svg cx cy 2 :fill color))))))

(defun switchboard--draw-badge (svg s x y size pal)
  "The cc/cx tag with its left edge at X; return its width."
  (let* ((claude (eq (switchboard-session-harness s) 'claude))
         (color (plist-get pal (if claude :claude :codex)))
         (w (+ 8 (switchboard--text-width "cc" size)))
         (h (round (* 1.35 size))))
    (svg-rectangle svg (+ x 0.5) (+ (- y h) 0.5 (* 0.28 size)) (- w 1) (- h 1)
                   :rx 3 :fill "none" :stroke color :stroke-width 1)
    (switchboard--svg-text svg (if claude "cc" "cx") (+ x (/ w 2.0)) y size color
                           :text-anchor "middle")
    w))

(defun switchboard--changes-parts (s)
  "S's uncommitted changes as ((TEXT . FACE) ...), left to right."
  (pcase (switchboard-session-changes s)
    ('nil nil)
    (`(0 0 ,_) '(("•" . switchboard-stopped)))   ; only new, untracked files
    (`(,a ,d ,_) `((,(format "+%d" a) . switchboard-ready)
                   (,(format "−%d" d) . switchboard-error)))))

(defun switchboard--changes-width (s size)
  "Pixels `switchboard--draw-changes' takes for S, with the gap before it."
  (let ((parts (switchboard--changes-parts s)))
    (if parts
        (+ 8 (cl-reduce #'+ (mapcar (lambda (p) (+ 4 (switchboard--text-width (car p) size)))
                                    parts)))
      0)))

(defun switchboard--draw-changes (svg s x y size)
  "Draw S's uncommitted changes ending at X."
  (dolist (part (reverse (switchboard--changes-parts s)))
    (switchboard--svg-text svg (car part) x y size (switchboard--color (cdr part))
                           :text-anchor "end")
    (setq x (- x (switchboard--text-width (car part) size) 4))))

(defun switchboard--row (s index width selected pal)
  "Image for S's row, WIDTH pixels wide.
SELECTED is `focused' on point's row, which highlights it whichever window
has focus; another session on screen (in a split) only gets an outline."
  (let* ((fs (switchboard--font-size))
         (small (round (* 0.74 fs)))
         (sub (round (* 0.8 fs)))
         (status (switchboard-session-status s))
         (color (switchboard--color (switchboard--face s)))
         (l1 (round (* 1.6 fs)))
         (l2 (if switchboard-compact 0 (round (* 1.25 fs))))
         (h (+ l1 l2 (if switchboard-compact 2 6)))
         (y1 (+ 2 (/ (+ l1 (* 0.7 fs)) 2)))
         (y2 (+ l1 (/ (+ l2 (* 0.7 sub)) 2)))
         (chip-h (round (* 1.15 fs)))
         (chip-w (round (* 1.55 fs)))
         (chip-x 28)
         (name-x (+ chip-x chip-w 9))
         (age-x (- width 10))
         (tokens-x (- age-x (switchboard--text-width "00m" small) 8))
         (badge-x (- tokens-x (switchboard--text-width "000k" small) 10
                     (+ 8 (switchboard--text-width "cc" small))))
         (shown (and (buffer-live-p (switchboard-session-buffer s))
                     (get-buffer-window (switchboard-session-buffer s) t)))
         (name-face (cond ((memq status '(waiting error stopped)) color)
                          (t (plist-get pal :fg))))
         (svg (svg-create width h)))
    ;; Each status tints its row.  An outlined row (point's, or one on screen)
    ;; is a rounded card on the plain background, so its tint can't show at
    ;; the corners.  Point's card is yellow, like the focused header.
    (let* ((tint-face (and (memq status '(waiting error stopped working ready))
                           (intern (format "switchboard-row-%s" status))))
           (tint (if tint-face (switchboard--color tint-face t) (plist-get pal :bg)))
           (focused (eq selected 'focused)))
      (svg-rectangle svg 0 0 width h :fill (if (or focused shown) (plist-get pal :bg) tint))
      (when (or focused shown)
        (let ((inset (if focused 2 1.5)))
          (svg-rectangle svg (+ 7 inset) inset (- width 10 inset) (- h inset inset) :rx 4
                         :fill (if focused
                                   (switchboard--color 'switchboard-header-focused t)
                                 tint)
                         :stroke (plist-get pal :blue) :stroke-width (if focused 2 1)))))
    (svg-rectangle svg 0 0 5 h :fill color)
    (when (<= index 9)
      (switchboard--svg-text svg (number-to-string index) 16 y1 small (plist-get pal :dim)))
    (switchboard--draw-chip svg s chip-x (+ 2 (/ (- l1 chip-h) 2)) chip-w chip-h pal)
    (switchboard--svg-text svg (switchboard--clip (concat (if (switchboard-session-marked s) "* " "")
                                                         (switchboard--title s))
                                                 fs (- badge-x name-x 10))
                           name-x y1 fs name-face
                           :font-weight (if (or (switchboard-session-unread s)
                                                (memq status '(waiting error)))
                                            "bold" "normal"))
    (switchboard--draw-badge svg s badge-x y1 small pal)
    (switchboard--svg-text svg (switchboard--tokens s) tokens-x y1 small (plist-get pal :dim)
                           :text-anchor "end")
    (switchboard--svg-text svg (switchboard--age s) age-x y1 small (plist-get pal :dim)
                           :text-anchor "end")
    (unless switchboard-compact
      (let* ((label (switchboard--label s))
             (label-w (* 1.08 (switchboard--text-width label small)))
             (text-x (+ name-x label-w 14))
             ;; What's pending (a permission request, an error), if anything.
             ;; Not the first prompt: Claude's title for the session says it.
             (text (string-replace "\n" " " (or (switchboard-session-detail s) "")))
             (changes-w (switchboard--changes-width s small))
             (branch (and (switchboard-session-branch s)
                          (switchboard--clip (concat "⎇ " (switchboard-session-branch s))
                                             small (* (if (string-empty-p text) 1 0.4)
                                                      (- age-x text-x changes-w)))))
             (branch-w (+ (if branch (+ (switchboard--text-width branch small) 12) 0)
                          changes-w)))
        (switchboard--svg-text svg label name-x y2 small color
                               :font-weight "bold" :letter-spacing "0.06em")
        (switchboard--svg-text svg (switchboard--clip text sub (- age-x text-x branch-w))
                               text-x y2 sub (plist-get pal :dim))
        (when branch
          (switchboard--svg-text svg branch (- age-x changes-w) y2 small (plist-get pal :dim)
                                 :text-anchor "end"))
        (switchboard--draw-changes svg s age-x y2 small)))
    (switchboard--image svg)))

(defun switchboard--group-row (title count folded width pal)
  (let* ((fs (switchboard--font-size))
         (h (round (* 1.7 fs)))
         (y (- h (round (* 0.45 fs))))
         (svg (svg-create width h)))
    (svg-rectangle svg 0 0 width h :fill (plist-get pal :bg))
    (switchboard--svg-text svg (if folded "▸" "▾") 10 y (round (* 0.8 fs)) (plist-get pal :fg))
    (switchboard--svg-text svg title 26 y fs (plist-get pal :fg) :font-weight "bold")
    (switchboard--svg-text svg (number-to-string count)
                           (+ 26 (switchboard--text-width title fs) 8) y
                           (round (* 0.8 fs)) (plist-get pal :dim))
    (switchboard--image svg)))

(defun switchboard--banner (waiting width pal)
  "The inset pill naming the sessions that need you."
  (let* ((fs (switchboard--font-size))
         (size (round (* 0.82 fs)))
         (h (round (* 2.1 fs)))
         (color (switchboard--color 'switchboard-waiting))
         (svg (svg-create width h)))
    (svg-rectangle svg 0 0 width h :fill (plist-get pal :bg))
    (svg-rectangle svg 6 4 (- width 12) (- h 8) :rx 4
                   :fill (switchboard--color 'switchboard-row-waiting t))
    (switchboard--svg-text svg
                           (switchboard--clip
                            (format "▲ %s: %s"
                                    (if (cdr waiting) (format "%d need you" (length waiting))
                                      "needs you")
                                    (mapconcat #'switchboard--title waiting ", "))
                            size (- width 60))
                           14 (+ (/ h 2.0) (* 0.35 size)) size color)
    (switchboard--svg-text svg "M-n" (- width 14) (+ (/ h 2.0) (* 0.35 size))
                           (round (* 0.85 size)) color :text-anchor "end" :opacity 0.7)
    (switchboard--image svg)))

(defun switchboard--header (width pal &optional focused)
  "Header pills: one per non-empty state, short labels if long ones don't fit.
FOCUSED means the list's window is selected."
  (let* ((fs (switchboard--font-size))
         (size (round (* 0.76 fs)))
         (h (round (* 1.9 fs)))
         (count (lambda (key) (cl-count key switchboard--sessions :key #'switchboard--status-key)))
         (pills (delq nil
                      (mapcar (pcase-lambda (`(,n ,face ,long ,short))
                                (and (> n 0) (list n face long short)))
                              `((,(funcall count 'waiting) switchboard-waiting "need you" "?")
                                (,(funcall count 'error) switchboard-error "error" "!")
                                (,(funcall count 'stopped) switchboard-stopped "stopped" "■")
                                (,(funcall count 'working) switchboard-working "working" "◐")
                                (,(+ (funcall count 'ready) (funcall count 'unread))
                                 switchboard-ready "ready" "✓")))))
         (label (lambda (p long) (if long (format "%d %s" (car p) (nth 2 p))
                                   (format "%s %d" (nth 3 p) (car p)))))
         (fits (<= (cl-reduce #'+ (mapcar (lambda (p) (+ 16 (switchboard--text-width
                                                             (funcall label p t) size)))
                                          pills)
                              :initial-value 10)
                   width))
         (svg (svg-create width h))
         (x 8))
    (svg-rectangle svg 0 0 width h :fill (if focused
                                             (switchboard--color 'switchboard-header-focused t)
                                           (plist-get pal :bg)))
    (svg-line svg 0 (- h 0.5) width (- h 0.5) :stroke (plist-get pal :dim) :stroke-opacity 0.25)
    (if (null pills)
        (switchboard--svg-text svg "No sessions" x (+ (/ h 2.0) (* 0.35 size)) size
                               (plist-get pal :dim))
      (dolist (p pills)
        (let* ((text (funcall label p fits))
               (w (+ 10 (switchboard--text-width text size))))
          (svg-rectangle svg x (- (/ h 2.0) (* 0.68 size)) w (* 1.36 size) :rx 3
                         :fill (switchboard--color (nth 1 p)))
          (switchboard--svg-text svg text (+ x 5) (+ (/ h 2.0) (* 0.36 size)) size
                                 (plist-get pal :paper) :font-weight "bold")
          (setq x (+ x w 6)))))
    (let ((hint (if switchboard--filter (concat "/" switchboard--filter) "? help")))
      (when (or switchboard--filter (<= (+ x (switchboard--text-width hint size) 8) width))
        (switchboard--svg-text svg hint (- width 8) (+ (/ h 2.0) (* 0.35 size)) size
                               (plist-get pal :dim) :text-anchor "end")))
    (switchboard--draw-focus svg width h focused pal)
    (switchboard--image svg)))

(defun switchboard--terminal-header (s width pal &optional focused)
  "Header for S's terminal: status square, name, tag, state, branch and path.
FOCUSED means its window is selected."
  (let* ((fs (switchboard--font-size))
         (size (round (* 0.78 fs)))
         (h (round (* 1.9 fs)))
         (y (+ (/ h 2.0) (* 0.35 fs)))
         (chip-h (round (* 1.15 fs)))
         (svg (svg-create width h))
         (x 10))
    (svg-rectangle svg 0 0 width h :fill (if focused
                                             (switchboard--color 'switchboard-header-focused t)
                                           (plist-get pal :paper)))
    (svg-line svg 0 (- h 0.5) width (- h 0.5) :stroke (plist-get pal :dim) :stroke-opacity 0.25)
    (switchboard--draw-chip svg s x (/ (- h chip-h) 2) (round (* 1.55 fs)) chip-h pal)
    (setq x (+ x (round (* 1.55 fs)) 10))
    (switchboard--svg-text svg (switchboard--title s) x y fs (plist-get pal :fg)
                           :font-weight "bold")
    (setq x (+ x (switchboard--text-width (switchboard--title s) fs) 10))
    (setq x (+ x (switchboard--draw-badge svg s x y size pal) 12))
    (let ((label (switchboard--label s)))
      (switchboard--svg-text svg label x y size (switchboard--color (switchboard--face s))
                             :font-weight "bold" :letter-spacing "0.06em")
      (setq x (+ x (* 1.08 (switchboard--text-width label size)) 18)))
    (switchboard--svg-text svg (switchboard--clip
                                (format "⎇ %s   %s" (or (switchboard-session-branch s) "")
                                        (abbreviate-file-name (switchboard-session-dir s)))
                                size (- width x 10))
                           x y size (plist-get pal :dim))
    (switchboard--draw-focus svg width h focused pal)
    (switchboard--image svg)))

(defun switchboard--draw-focus (svg width h focused pal)
  "Underline a header WIDTH by H in blue if FOCUSED."
  (when focused
    (svg-rectangle svg 0 (- h 2) width 2 :fill (plist-get pal :blue))))

(defun switchboard--header-width (win)
  "Pixels a header image needs to span WIN's header line, fringes included."
  (- (window-pixel-width win) (window-right-divider-width win) (window-scroll-bar-width win)))

(defun switchboard--update-terminal-headers (pal)
  (dolist (s switchboard--sessions)
    (when-let* ((buf (switchboard-session-buffer s))
                ((buffer-live-p buf)))
      (let ((win (get-buffer-window buf t)))
        (with-current-buffer buf
          (setq switchboard--terminal-header-image
                (switchboard--terminal-header
                 s (if win (switchboard--header-width win) 1200) pal
                 (and win (eq win (selected-window)))))
          (when win (force-mode-line-update)))))))

(defvar-local switchboard--drawn-selection nil
  "The session drawn as selected, so moving point knows to redraw.")

(defun switchboard--render ()
  (setq switchboard--render-timer nil)
  (let ((pal (switchboard--palette)))
    (switchboard--update-terminal-headers pal)
    (when-let* ((buf (get-buffer switchboard--buffer-name)))
      (with-current-buffer buf
        (let* ((inhibit-read-only t)
               (win (get-buffer-window buf t))
               (width (if win (window-body-width win t)
                        (* switchboard-sidebar-width (frame-char-width))))
               (visible (switchboard--visible))
               (at (get-text-property (point) 'switchboard-session))
               (selected (if (memq at visible) at (car visible)))
               (focused (and win (eq win (selected-window))))
               (waiting (cl-remove-if-not (lambda (s) (eq (switchboard-session-status s) 'waiting))
                                          switchboard--sessions))
               (index 0))
          (setq switchboard--header-image
                (switchboard--header (if win (switchboard--header-width win) width) pal focused))
          (erase-buffer)
          (when waiting
            (insert (propertize (switchboard--banner waiting width pal) 'switchboard-banner t)
                    "\n"))
          (unless switchboard--sessions
            (insert (propertize "\n  c starts a session.\n" 'face 'shadow)))
          (pcase-dolist (`(,key ,title . ,members) (switchboard--groups))
            (insert (propertize (switchboard--group-row title (length members)
                                                        (member key switchboard--folded) width pal)
                                'switchboard-group key)
                    "\n")
            (dolist (s members)
              (when (switchboard--shown-p key s)
                (insert (propertize (switchboard--row s (cl-incf index) width
                                                     (and (eq s selected) 'focused)
                                                     pal)
                                    'switchboard-session s)
                        "\n"))))
          (goto-char (point-min))
          (when-let* ((m (and selected
                              (text-property-search-forward 'switchboard-session selected #'eq))))
            (goto-char (prop-match-beginning m)))
          (setq switchboard--drawn-selection selected)
          (when win (set-window-point win (point)))
          (force-mode-line-update))))))

(defun switchboard--follow-point ()
  "Redraw when point lands on a different session, to move the highlight."
  (unless (eq (get-text-property (point) 'switchboard-session) switchboard--drawn-selection)
    (switchboard--render)))

(defun switchboard--schedule-render ()
  (unless switchboard--render-timer
    (setq switchboard--render-timer (run-at-time 0.05 nil #'switchboard--render))))

(defun switchboard--tick ()
  "Redraw for the spinner and ages while the list is on screen."
  (if (get-buffer-window switchboard--buffer-name t)
      (switchboard--render)
    (cancel-timer switchboard--tick-timer)
    (setq switchboard--tick-timer nil)))

(defvar-keymap switchboard-list-mode-map
  :parent special-mode-map
  "RET" #'switchboard-open
  "<mouse-1>" #'switchboard-click
  "<down-mouse-1>" #'ignore           ; no drag-to-select over the images
  "SPC" #'switchboard-peek
  "j" #'switchboard-next-line
  "k" #'switchboard-previous-line
  "n" #'switchboard-next-attention
  "c" #'switchboard-new
  "m" #'switchboard-mark
  "u" #'switchboard-unmark-all
  "b" #'switchboard-broadcast
  "r" #'switchboard-resume
  "R" #'switchboard-rename
  "x" #'switchboard-kill
  "P" #'switchboard-prune-worktrees
  "d" #'switchboard-magit
  "/" #'switchboard-filter
  "TAB" #'switchboard-fold
  "t" #'switchboard-toggle-grouping
  "v" #'switchboard-toggle-compact
  "s" #'switchboard-split
  "!" #'switchboard-shell
  "z" #'switchboard-zoom
  "g" #'switchboard-refresh
  "?" #'switchboard-help
  "q" #'quit-window
  "1" #'switchboard-jump "2" #'switchboard-jump "3" #'switchboard-jump
  "4" #'switchboard-jump "5" #'switchboard-jump "6" #'switchboard-jump
  "7" #'switchboard-jump "8" #'switchboard-jump "9" #'switchboard-jump)

(define-derived-mode switchboard-list-mode special-mode "Switchboard"
  "The session list.

\\{switchboard-list-mode-map}"
  (setq-local truncate-lines t
              cursor-type nil
              ;; Evil would bring back its box cursor, which draws a second
              ;; border around the row's image.  (nil) means no cursor to it.
              evil-emacs-state-cursor '(nil)
              line-spacing 0
              ;; None, so its bottom lines up with the terminal's, which has none.
              mode-line-format nil
              header-line-format '(:eval switchboard--header-image))
  (face-remap-add-relative 'default 'switchboard-sidebar)
  (face-remap-add-relative 'header-line 'switchboard-sidebar '(:box nil :underline nil))
  (face-remap-add-relative 'fringe 'switchboard-sidebar)
  (add-hook 'post-command-hook #'switchboard--follow-point nil t))

(with-eval-after-load 'evil
  (evil-set-initial-state 'switchboard-list-mode 'emacs))

(defun switchboard--buffer ()
  (or (get-buffer switchboard--buffer-name)
      (with-current-buffer (get-buffer-create switchboard--buffer-name)
        (switchboard-list-mode)
        (current-buffer))))

(defun switchboard--display-sidebar ()
  "Show the list in a left side window; return the window."
  (let ((win (display-buffer-in-side-window
              (switchboard--buffer)
              `((side . left) (slot . 0) (window-width . ,switchboard-sidebar-width)
                (preserve-size . (t . nil))
                (window-parameters . ((no-delete-other-windows . t)))))))
    (set-window-dedicated-p win t)
    (unless switchboard--tick-timer
      (setq switchboard--tick-timer (run-at-time 0.5 0.5 #'switchboard--tick)))
    (switchboard--render)
    win))

;;;###autoload
(defun switchboard ()
  "Show the session list and select it.
After a restart, saved sessions come back as EXITED; r or
`switchboard-resume-all' restarts them."
  (interactive)
  (unless switchboard--sessions
    (when-let* ((added (switchboard--load)))
      (message "Restored %d saved session%s; SPC e r resumes them all"
               (length added) (if (cdr added) "s" ""))))
  (select-window (switchboard--display-sidebar)))

;;;###autoload
(defun switchboard-toggle-focus ()
  "Move between the session list and the terminal you last used."
  (interactive)
  (if (eq (current-buffer) (get-buffer switchboard--buffer-name))
      (select-window (switchboard--main-window))
    (switchboard)))


;;; Drawer: a shell under a session

(defcustom switchboard-drawer-height 0.35
  "Fraction of a session's window that a drawer split takes."
  :type 'float)

(defvar-local switchboard--drawer-of nil
  "The session whose worktree this drawer terminal runs in.
Separate from `switchboard--session' so a drawer's keys and exit never
touch the agent's status.")
(put 'switchboard--drawer-of 'permanent-local t)

(defun switchboard--drawer-window-p (window)
  (buffer-local-value 'switchboard--drawer-of (window-buffer window)))

(defun switchboard--drawer-name (s kind)
  (format "*sb:%s %s*" (switchboard-session-name s) kind))

(defun switchboard--current ()
  "The session meant: at point in the list, this buffer's, or the main window's."
  (or (and (eq (current-buffer) (get-buffer switchboard--buffer-name))
           (get-text-property (point) 'switchboard-session))
      switchboard--session
      switchboard--drawer-of
      (buffer-local-value 'switchboard--session (window-buffer (switchboard--main-window)))
      (user-error "No session here")))

(defun switchboard--open-drawer (s kind program args)
  "Show S's KIND terminal in a split below S, starting PROGRAM if it isn't running."
  (require 'ghostel)
  (unless (buffer-live-p (switchboard-session-buffer s))
    (user-error "%s has no terminal; r resumes it" (switchboard--title s)))
  (let* ((buf (get-buffer-create (switchboard--drawer-name s kind)))
         (fresh (not (process-live-p (get-buffer-process buf))))
         (above (or (get-buffer-window (switchboard-session-buffer s))
                    (switchboard--show s)))
         (win (or (get-buffer-window buf)
                  (seq-find #'switchboard--drawer-window-p (window-list nil 'nomini))
                  (split-window above (- (max 6 (round (* switchboard-drawer-height
                                                          (window-total-height above)))))
                                'below))))
    (set-window-buffer win buf)
    (select-window win)
    (when fresh
      (with-current-buffer buf (setq default-directory (switchboard-session-dir s)))
      (let ((default-directory (switchboard-session-dir s))
            (process-environment
             (cl-remove-if (lambda (v) (string-match-p switchboard--inherited-env v))
                           process-environment)))
        (ghostel-exec buf program args))
      (with-current-buffer buf (setq-local switchboard--drawer-of s)))
    buf))

(defun switchboard--follow-drawer (s)
  "When switching to S, point an open drawer at S's shell, or close it."
  (dolist (w (window-list nil 'nomini))
    (let ((of (switchboard--drawer-window-p w)))
      (when (and of (not (eq of s)))
        (let ((shell (get-buffer (switchboard--drawer-name s "shell"))))
          (if (and shell (process-live-p (get-buffer-process shell)))
              (set-window-buffer w shell)
            (ignore-errors (delete-window w))))))))

;;;###autoload
(defun switchboard-shell ()
  "Toggle a shell in the current session's worktree, in a split below it.
The shell keeps running while hidden; `exit' closes it for good."
  (interactive)
  (let* ((s (switchboard--current))
         (buf (get-buffer (switchboard--drawer-name s "shell")))
         (win (and buf (get-buffer-window buf))))
    (if win
        (progn
          (when (eq win (selected-window))
            (select-window (or (get-buffer-window (switchboard-session-buffer s))
                               (switchboard--main-window))))
          (delete-window win))
      (let ((shell (ensure-list ghostel-shell)))
        (switchboard--open-drawer s "shell" (car shell) (cdr shell))))))

;;;###autoload
(defun switchboard-zoom ()
  "Give the whole area to the agent or its shell, whichever you're in.
Again brings the split back; the hidden one keeps running.  Outside a
session it maximizes the window instead (`doom/window-maximize-buffer')."
  (interactive)
  (let* ((s (or (and (eq (current-buffer) (get-buffer switchboard--buffer-name))
                     (get-text-property (point) 'switchboard-session))
                switchboard--session switchboard--drawer-of))
         (agent (and s (switchboard-session-buffer s)))
         (shell (and s (get-buffer (switchboard--drawer-name s "shell"))))
         (agent-w (and (buffer-live-p agent) (get-buffer-window agent)))
         (shell-w (and shell (get-buffer-window shell))))
    (cond
     ((not s)
      (if (fboundp 'doom/window-maximize-buffer)
          (call-interactively #'doom/window-maximize-buffer)
        (delete-other-windows)))
     ;; Both showing: keep the one you're in.
     ((and agent-w shell-w)
      (delete-window (if (eq (selected-window) shell-w) agent-w shell-w)))
     ;; Agent alone: bring its shell back below.
     (agent-w
      (unless (and shell (process-live-p (get-buffer-process shell)))
        (user-error "No shell to split with; SPC e s opens one"))
      (let ((win (split-window agent-w (- (max 6 (round (* switchboard-drawer-height
                                                            (window-total-height agent-w)))))
                               'below)))
        (set-window-buffer win shell)))
     ;; Shell alone: bring the agent back above, keeping the shell's size.
     (shell-w
      (let ((win (split-window shell-w (- (window-total-height shell-w)
                                          (max 6 (round (* switchboard-drawer-height
                                                           (window-total-height shell-w)))))
                               'below)))
        ;; split-window gives the new window the bottom; swap so the shell stays below.
        (set-window-buffer win shell)
        (set-window-buffer shell-w agent)
        (when (eq (selected-window) shell-w) (select-window win))))
     (t (user-error "%s isn't on screen" (switchboard--title s))))))


;;; Commands

(defun switchboard--at-point ()
  (or (get-text-property (point) 'switchboard-session)
      (user-error "No session on this line")))

(defun switchboard--main-window ()
  "The window sessions open in: the most recent one outside the list and drawers."
  (let ((usable (lambda (w) (not (or (window-parameter w 'window-side)
                                     (switchboard--drawer-window-p w))))))
    (if (funcall usable (selected-window))
        (selected-window)
      (or (car (sort (seq-filter usable (window-list nil 'nomini))
                     (lambda (a b) (> (window-use-time a) (window-use-time b)))))
          (window-main-window)))))

(defun switchboard--show (s &optional select)
  (let ((win (switchboard--main-window)))
    (set-window-buffer win (switchboard-session-buffer s))
    (switchboard--follow-drawer s)
    (when select (select-window win))
    (switchboard--refit win)
    (switchboard--select-row s)
    (switchboard--clear-notification s)
    (switchboard--schedule-render)
    win))

(defun switchboard--select-row (s)
  "Move the list's point to S's row, so the highlight follows what's shown."
  (when-let* ((buf (get-buffer switchboard--buffer-name)))
    (with-current-buffer buf
      (let ((pos (save-excursion
                   (goto-char (point-min))
                   (when-let* ((m (text-property-search-forward 'switchboard-session s #'eq)))
                     (prop-match-beginning m)))))
        (when pos
          (goto-char pos)
          (when-let* ((win (get-buffer-window buf t)))
            (set-window-point win pos)))))))

(defun switchboard--refit (win)
  "Resize WIN's terminal to WIN and scroll it to the bottom.
Sometimes a terminal ends up taller than its window, cutting off the
last rows; this puts it right whenever you switch to it."
  (with-current-buffer (window-buffer win)
    (when (bound-and-true-p ghostel--term)
      (redisplay)                       ; settle the header line's height
      (ghostel--adjust-size win t)
      (ghostel--anchor-window win t))))

(defun switchboard-open (s)
  "Show session S and select its terminal."
  (interactive (list (switchboard--at-point)))
  (if (buffer-live-p (switchboard-session-buffer s))
      (switchboard--show s t)
    (user-error "%s has no buffer; r resumes it" (switchboard--title s))))

(defun switchboard-peek (s)
  "Show session S without leaving the list."
  (interactive (list (switchboard--at-point)))
  (when (buffer-live-p (switchboard-session-buffer s))
    (switchboard--show s)))

(defun switchboard-next-session (&optional n)
  "Show the Nth next session in list order, like switching tabs.
In the list it moves the selection and previews; in a session it shows
the next one in the same window. Anywhere else it is `next-buffer'."
  (interactive "p")
  (let* ((n (or n 1))
         (list-p (eq (current-buffer) (get-buffer switchboard--buffer-name)))
         (here (if list-p (get-text-property (point) 'switchboard-session) switchboard--session))
         (order (cl-remove-if-not (lambda (s) (buffer-live-p (switchboard-session-buffer s)))
                                  (switchboard--visible))))
    (cond
     ((not (or list-p here))
      (if (< n 0) (previous-buffer (- n)) (next-buffer n)))
     (order
      (let* ((i (cl-position here order))
             (next (nth (if i (mod (+ i n) (length order)) (if (> n 0) 0 (1- (length order))))
                        order)))
        (if (not list-p)
            (switchboard--show next t)
          (switchboard-peek next)))))))

(defun switchboard-previous-session (&optional n)
  "Show the Nth previous session in list order; see `switchboard-next-session'."
  (interactive "p")
  (switchboard-next-session (- (or n 1))))

(defun switchboard-click (event)
  "Do what RET, TAB or n would on the line clicked: open, fold, or jump."
  (interactive "e")
  (mouse-set-point event)
  (cond ((get-text-property (point) 'switchboard-session)
         (switchboard-open (get-text-property (point) 'switchboard-session)))
        ((get-text-property (point) 'switchboard-group) (switchboard-fold))
        ((get-text-property (point) 'switchboard-banner) (switchboard-next-attention))))

(defun switchboard-jump ()
  "Open the Nth listed session, N being the digit typed."
  (interactive)
  (let ((s (nth (1- (- last-command-event ?0)) (switchboard--visible))))
    (if s (switchboard-open s) (user-error "No session %c" last-command-event))))

(defun switchboard-next-line (&optional n)
  "Move to the next session row (previous with negative N)."
  (interactive "p")
  (let ((here (get-text-property (point) 'switchboard-session))
        (step (if (< (or n 1) 0) -1 1)))
    (dotimes (_ (abs (or n 1)))
      (while (and (zerop (forward-line step))
                  (not (bobp)) (not (eobp))
                  (let ((s (get-text-property (point) 'switchboard-session)))
                    (or (null s) (eq s here)))))
      (setq here (get-text-property (point) 'switchboard-session)))
    ;; Land on the first line of a two-line row.
    (while (and (not (bobp))
                (eq (get-text-property (1- (line-beginning-position)) 'switchboard-session)
                    (get-text-property (point) 'switchboard-session))
                (get-text-property (point) 'switchboard-session))
      (forward-line -1))))

(defun switchboard-previous-line (&optional n)
  "Move to the previous session row."
  (interactive "p")
  (switchboard-next-line (- (or n 1))))

;;;###autoload
(defun switchboard-next-attention ()
  "Open the next session that needs you.
Order: permission prompts (oldest first), errors, then unread finished turns."
  (interactive)
  (let* ((rank (lambda (s) (pcase (switchboard--status-key s)
                             ('waiting 0) ('error 1) ('unread 2))))
         (queue (sort (cl-remove-if-not rank switchboard--sessions)
                      (lambda (a b) (let ((ra (funcall rank a)) (rb (funcall rank b)))
                                      (if (= ra rb)
                                          (< (switchboard-session-changed a) (switchboard-session-changed b))
                                        (< ra rb))))))
         (current (buffer-local-value 'switchboard--session
                                      (window-buffer (switchboard--main-window))))
         (next (or (cadr (memq current queue)) (car queue))))
    (if (not next)
        (message "Nothing needs you")
      (switchboard-open next)
      (message "%s: %s (%d in queue)" (switchboard--title next)
               (switchboard--label next) (length queue)))))

(defun switchboard-mark (s)
  "Toggle the broadcast mark on S."
  (interactive (list (switchboard--at-point)))
  (setf (switchboard-session-marked s) (not (switchboard-session-marked s)))
  (switchboard--render)
  (switchboard-next-line))

(defun switchboard-unmark-all ()
  "Clear every broadcast mark."
  (interactive)
  (dolist (s switchboard--sessions) (setf (switchboard-session-marked s) nil))
  (switchboard--render))

(defun switchboard--send (s text)
  "Type TEXT into S and press RET."
  (with-current-buffer (switchboard-session-buffer s)
    (ghostel-paste-string text)
    ;; Give the TUI a beat to take the paste before submitting it.
    (run-at-time 0.15 nil (lambda (buf)
                            (when (buffer-live-p buf)
                              (with-current-buffer buf (ghostel-send-key "return"))))
                 (current-buffer))))

(defun switchboard-broadcast (text)
  "Send TEXT to every marked session that is ready for it."
  (interactive
   (let ((targets (cl-remove-if-not #'switchboard-session-marked switchboard--sessions)))
     (unless targets (user-error "Mark sessions with m first"))
     (list (read-string (format "Send to %s: "
                                (mapconcat #'switchboard--title targets ", "))))))
  (let ((sent 0))
    (dolist (s switchboard--sessions)
      (when (and (switchboard-session-marked s)
                 (memq (switchboard-session-status s) '(ready stopped error))
                 (buffer-live-p (switchboard-session-buffer s)))
        (switchboard--send s text)
        (setf (switchboard-session-marked s) nil)
        (cl-incf sent)))
    (message "Sent to %d session%s; busy ones stay marked" sent (if (= sent 1) "" "s"))
    (switchboard--render)))

(defun switchboard-rename (s name)
  "Call session S NAME from now on; Claude's titles no longer replace it.
An empty NAME hands naming back to Claude."
  (interactive (let ((s (switchboard--at-point)))
                 (list s (read-string "Rename to (empty: let Claude name it): "
                                      (switchboard--title s)))))
  (if (string-blank-p name)
      (setf (switchboard-session-named s) nil
            (switchboard-session-title s)
            (switchboard--ai-title (switchboard-session-transcript s)))
    (setf (switchboard-session-title s) name
          (switchboard-session-named s) t))
  (switchboard--rename-buffer s)
  (switchboard--save-soon)
  (switchboard--render))

(defun switchboard-kill (s)
  "Kill session S.
If it had its own worktree, merged and with nothing uncommitted, offer
to remove that and its branch too; otherwise they're kept, for
`switchboard-prune-worktrees' to clear later."
  (interactive (list (switchboard--at-point)))
  (when (y-or-n-p (format "Kill %s%s? " (switchboard--title s)
                          (if (eq (switchboard-session-status s) 'working) " (working!)" "")))
    (when (buffer-live-p (switchboard-session-buffer s))
      (let ((kill-buffer-query-functions nil)
            (ghostel-query-before-killing nil))
        (kill-buffer (switchboard-session-buffer s))))
    (when-let* ((buf (get-buffer (switchboard--drawer-name s "shell"))))
      (let ((kill-buffer-query-functions nil)
            (ghostel-query-before-killing nil))
        (kill-buffer buf)))
    (setq switchboard--sessions (delq s switchboard--sessions))
    (switchboard--save-soon)
    (switchboard--render)
    (switchboard--offer-worktree-removal s)))

(defun switchboard--offer-worktree-removal (s)
  "After killing S, offer to remove its worktree if nothing would be lost."
  (let* ((dir (file-name-as-directory (expand-file-name (switchboard-session-dir s))))
         (own (and (string-prefix-p (expand-file-name switchboard-worktree-directory) dir)
                   (not (cl-find dir switchboard--sessions
                                 :key (lambda (o) (file-name-as-directory
                                                   (expand-file-name (switchboard-session-dir o))))
                                 :test #'equal))))
         (info (and own (file-directory-p dir) (switchboard--worktree-info dir))))
    (pcase info
      ('nil (message "Killed %s" (switchboard--title s)))
      (`(,root ,branch t nil)
       (if (y-or-n-p (format "Killed %s.  Remove its worktree and branch %s too? "
                             (switchboard--title s) branch))
           (if (switchboard--remove-worktree dir root branch t)
               (message "Removed %s and %s" (abbreviate-file-name dir) branch)
             (message "Couldn't remove %s" (abbreviate-file-name dir)))
         (message "Kept %s" (abbreviate-file-name dir))))
      (`(,_ ,branch ,merged ,_)
       (message "Killed %s; kept %s, it has %s.  P removes it later"
                (switchboard--title s) (abbreviate-file-name dir)
                (if merged "uncommitted changes" (format "unmerged work on %s" branch)))))))

(defun switchboard--orphan-worktrees ()
  "Worktrees under `switchboard-worktree-directory' that no session uses.
Return a list of (DIR ROOT BRANCH MERGED DIRTY)."
  (let ((used (mapcar (lambda (s) (file-name-as-directory
                                   (expand-file-name (switchboard-session-dir s))))
                      switchboard--sessions))
        (top (expand-file-name switchboard-worktree-directory)))
    (cl-loop
     for repo in (and (file-directory-p top) (directory-files top t "\\`[^.]"))
     when (file-directory-p repo)
     nconc
     (cl-loop
      for dir in (directory-files repo t "\\`[^.]")
      for dir/ = (file-name-as-directory dir)
      for info = (and (file-directory-p dir) (not (member dir/ used))
                      (switchboard--worktree-info dir/))
      when info collect (cons dir/ info)))))

(defun switchboard--worktree-info (dir)
  "(ROOT BRANCH MERGED DIRTY) for the worktree at DIR, or nil if it isn't one.
ROOT is its project's main checkout; MERGED means BRANCH is merged into
the branch checked out there; DIRTY, that DIR has uncommitted changes."
  (when-let* ((common (switchboard--git dir "rev-parse" "--path-format=absolute"
                                        "--git-common-dir")))
    (let ((root (file-name-as-directory (file-name-directory (directory-file-name common))))
          (branch (switchboard--git dir "branch" "--show-current")))
      (list root branch
            (and branch (not (string-empty-p branch))
                 (switchboard--git root "merge-base" "--is-ancestor" branch "HEAD") t)
            (not (string-empty-p (or (switchboard--git dir "status" "--porcelain") "")))))))

(defun switchboard--remove-worktree (dir root branch delete-branch)
  "Remove the worktree at DIR from ROOT, and BRANCH too if DELETE-BRANCH.
Return non-nil if the worktree went."
  (when (switchboard--git root "worktree" "remove" "--force" dir)
    (when (and delete-branch branch (not (string-empty-p branch)))
      (switchboard--git root "branch" "-D" branch))
    t))

(defun switchboard-prune-worktrees ()
  "Remove worktrees that no session uses, with their branches.
You pick which; each is marked merged (into its project's checked-out
branch) or not, and dirty if it has uncommitted changes.  A dirty
worktree or an unmerged branch is only removed once you confirm it."
  (interactive)
  (let* ((orphans (or (switchboard--orphan-worktrees)
                      (user-error "No worktrees without a session")))
         (label (pcase-lambda (`(,dir ,_ ,branch ,merged ,dirty))
                  (format "%s  %s%s%s" (abbreviate-file-name (directory-file-name dir))
                          (or branch "?") (if merged "  merged" "  unmerged")
                          (if dirty "  dirty" ""))))
         (choices (mapcar (lambda (o) (cons (funcall label o) o)) orphans))
         (picked (completing-read-multiple "Remove worktrees: " choices nil t))
         (removed 0))
    (dolist (o (mapcar (lambda (p) (cdr (assoc p choices))) picked))
      (pcase-let ((`(,dir ,root ,branch ,merged ,dirty) o))
        (when (or (not dirty)
                  (yes-or-no-p (format "%s has uncommitted changes.  Throw them away? "
                                       (abbreviate-file-name dir))))
          (if (switchboard--remove-worktree
               dir root branch
               (or merged (and branch (not (string-empty-p branch))
                               (y-or-n-p (format "%s isn't merged.  Delete it anyway? " branch)))))
              (cl-incf removed)
            (message "Switchboard: couldn't remove %s" (abbreviate-file-name dir))))))
    (message "Removed %d worktree%s" removed (if (= removed 1) "" "s"))))

(defun switchboard-resume (s)
  "Restart an exited session S where its conversation left off."
  (interactive (list (switchboard--at-point)))
  (when (and (buffer-live-p (switchboard-session-buffer s))
             (process-live-p (get-buffer-process (switchboard-session-buffer s))))
    (user-error "%s is still running" (switchboard--title s)))
  (setf (switchboard-session-detail s) nil)   ; why it exited
  (switchboard--set s 'ready)
  (switchboard--update-git s)        ; it may have moved while nothing ran
  (switchboard--spawn s nil t))

(defun switchboard-magit (s)
  "Open magit-status for S's worktree in the main window."
  (interactive (list (switchboard--at-point)))
  (select-window (switchboard--main-window))
  (magit-status-setup-buffer (switchboard-session-dir s)))

(defun switchboard-filter (text)
  "Only list sessions matching every word of TEXT; empty clears it."
  (interactive (list (read-string "Filter: " switchboard--filter)))
  (setq switchboard--filter (and (not (string-blank-p text)) text))
  (switchboard--render))

(defun switchboard-fold ()
  "Fold or unfold the group at point."
  (interactive)
  (let ((key (or (get-text-property (point) 'switchboard-group)
                 (let ((s (switchboard--at-point)))
                   (if (eq switchboard--group 'status)
                       (switchboard--status-key s)
                     (switchboard--project s))))))
    (setq switchboard--folded (if (member key switchboard--folded)
                                  (delete key switchboard--folded)
                                (cons key switchboard--folded)))
    (switchboard--render)))

(defun switchboard-toggle-grouping ()
  "Group the list by project or by status."
  (interactive)
  (setq switchboard--group (if (eq switchboard--group 'project) 'status 'project)
        switchboard--folded nil)
  (switchboard--render))

(defun switchboard-toggle-compact ()
  "Switch between one- and two-line rows."
  (interactive)
  (setq switchboard-compact (not switchboard-compact))
  (switchboard--render))

(defun switchboard-split (s)
  "Show S in a new window beside the main one."
  (interactive (list (switchboard--at-point)))
  (unless (buffer-live-p (switchboard-session-buffer s))
    (user-error "%s has no buffer" (switchboard--title s)))
  (let ((win (split-window (switchboard--main-window) nil 'right)))
    (set-window-buffer win (switchboard-session-buffer s))
    (select-window win)))

(defun switchboard--slots ()
  "The session record's slot names, in order, tag slot first."
  (mapcar #'car (cl-struct-slot-info 'switchboard-session)))

(defun switchboard--migrate-sessions (old-slots)
  "Rebuild sessions made with OLD-SLOTS in the current record layout.
A reload that adds or moves a slot leaves running sessions in the old
layout, which the new accessors would read shifted.  Copy each slot by
name, and point the terminal and drawer buffers at the new records."
  (unless (equal old-slots (switchboard--slots))
    (let ((map nil))
      (setq switchboard--sessions
            (mapcar
             (lambda (s)
               (let ((new (apply #'switchboard--session-create
                                 (cl-loop for slot in (cdr old-slots) for i from 1
                                          when (memq slot (switchboard--slots))
                                          nconc (list (intern (format ":%s" slot)) (aref s i))))))
                 (push (cons s new) map)
                 new))
             switchboard--sessions))
      (dolist (buf (buffer-list))
        (with-current-buffer buf
          (when-let* ((new (cdr (assq switchboard--session map))))
            (setq switchboard--session new))
          (when-let* ((new (cdr (assq switchboard--drawer-of map))))
            (setq switchboard--drawer-of new))))
      (message "Switchboard: rebuilt %d session%s for the new layout"
               (length map) (if (cdr map) "s" "")))))

(defun switchboard-reload ()
  "Load this file again, picking up changes; running sessions carry on.
A plain `load-file' would miss changes to the list's keys and faces,
since `defvar-keymap' and `defface' keep existing values."
  (interactive)
  (let ((file (expand-file-name "switchboard.el" (file-name-directory switchboard--hook-script)))
        (slots (switchboard--slots)))
    (makunbound 'switchboard-list-mode-map)
    (dolist (face (get 'switchboard 'custom-group))
      (when (eq (cadr face) 'custom-face) (put (car face) 'face-defface-spec nil)))
    (load file nil t t)
    (switchboard--migrate-sessions slots)
    (dolist (face (get 'switchboard 'custom-group))
      (when (eq (cadr face) 'custom-face)
        (face-spec-set (car face) (get (car face) 'face-defface-spec) 'face-defface-spec)))
    (when-let* ((buf (get-buffer switchboard--buffer-name)))
      (with-current-buffer buf (switchboard-list-mode)))
    (switchboard--render)
    (message "Switchboard reloaded; %d session%s untouched" (length switchboard--sessions)
             (if (= 1 (length switchboard--sessions)) "" "s"))))

(defun switchboard-refresh ()
  "Redraw the list."
  (interactive)
  (switchboard--render))

(defconst switchboard--help
  '((switchboard-open "Open the session at point")
    (switchboard-peek "Show it without leaving the list")
    (switchboard-jump "Open the Nth listed session")
    (switchboard-next-line "Next row")
    (switchboard-previous-line "Previous row")
    (switchboard-next-attention "Open the next session that needs you")
    (switchboard-new "Start a new session")
    (switchboard-rename "Rename the session at point")
    (switchboard-resume "Resume an exited session")
    (switchboard-kill "Kill it, offering to remove its worktree if merged")
    (switchboard-prune-worktrees "Remove worktrees no session uses")
    (switchboard-mark "Toggle its broadcast mark")
    (switchboard-unmark-all "Clear every broadcast mark")
    (switchboard-broadcast "Send text to every marked session")
    (switchboard-split "Show it in a split beside the main window")
    (switchboard-shell "Toggle a shell in its worktree")
    (switchboard-zoom "Give the whole area to the agent or its shell")
    (switchboard-magit "Magit status for its worktree")
    (switchboard-filter "Filter the list; empty clears it")
    (switchboard-fold "Fold or unfold the group at point")
    (switchboard-toggle-grouping "Group by project or by status")
    (switchboard-toggle-compact "One- or two-line rows")
    (switchboard-refresh "Redraw")
    (switchboard-help "This help")
    (quit-window "Hide the list"))
  "The list's commands, in the order `switchboard-help' shows them.")

(defun switchboard-help ()
  "List the session list's keys."
  (interactive)
  (help-setup-xref (list #'switchboard-help) (called-interactively-p 'interactive))
  (with-help-window (help-buffer)
    (princ "Switchboard keys\n\n")
    (pcase-dolist (`(,cmd ,desc) switchboard--help)
      (when-let* ((keys (sort (seq-remove (lambda (k) (mouse-event-p (aref k 0)))
                                          (where-is-internal cmd (list switchboard-list-mode-map)))
                              :key #'key-description)))
        (princ (format "  %-8s %s\n"
                       (if (cdr keys)   ; the digits
                           (concat (key-description (car keys)) "-"
                                   (key-description (car (last keys))))
                         (key-description (car keys)))
                       desc))))))

(provide 'switchboard)
;;; switchboard.el ends here
