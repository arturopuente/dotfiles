#!/bin/sh
# Reports Claude Code, Codex and Pi events to Switchboard in Emacs.
#
#   Claude Code hook:  switchboard-hook.sh claude <Event>      (payload JSON on stdin)
#   Codex hook:        switchboard-hook.sh codex-hook <Event>  (payload JSON on stdin)
#   Codex notify:      switchboard-hook.sh codex <payload-json>
#   Pi extension:      switchboard-hook.sh pi <Event>          (payload JSON on stdin)
#
# Switchboard sets SWITCHBOARD_ID (and the server socket) in each session's
# environment; anything started outside Switchboard is ignored. Payloads go
# through a temp file, not argv, because they can be large. Most tool events
# carry no payload, Emacs only needs to know they happened; a subagent's, or
# an Agent or Codex spawn_agent call starting one, does. Their fields come before the tool's
# input, so the first 2 KB tell. Those that do are cut down, with jq, to
# the fields Switchboard reads: a tool's input and output can run to
# megabytes, which Emacs would parse only to throw away.

[ -n "$SWITCHBOARD_ID" ] || exit 0

case "$1" in
  claude|codex-hook)
    # Codex's hooks share Claude Code's event names and payload fields.
    event=$2
    case "$event" in
      SessionStart|UserPromptSubmit|PermissionRequest|Stop|StopFailure|SubagentStart|SubagentStop|Interrupt)
        payload=$(mktemp -t switchboard) && cat > "$payload" ;;
      PreToolUse|PostToolUse|PostToolUseFailure)
        payload=$(mktemp -t switchboard) && cat > "$payload" &&
          { head -c 2048 "$payload" | grep -qE '"agent_id":|"tool_name":"(Agent|Task|[a-z_]*spawn_agent)"' ||
              { rm -f "$payload"; payload=; }; } &&
          if [ -n "$payload" ] && command -v jq > /dev/null &&
               jq -c '{session_id, cwd, agent_id, agent_type, tool_name}
                      + if .tool_name == "Agent" or .tool_name == "Task"
                        then {tool_input: (.tool_input
                                           | {subagent_type, description, run_in_background})}
                        elif (.tool_name // "" | endswith("spawn_agent"))
                        then {tool_input: (.tool_input | {agent_type, task_name})}
                        else {} end' "$payload" > "$payload.min" 2> /dev/null
          then mv "$payload.min" "$payload"
          else rm -f "$payload.min"
          fi ;;
      *)
        cat > /dev/null; payload= ;;
    esac ;;
  pi)
    # switchboard-pi.ts sends Claude Code's event names, already cut down.
    event=$2
    payload=$(mktemp -t switchboard) && cat > "$payload" ;;
  codex)
    event=notify
    payload=$(mktemp -t switchboard) && printf '%s' "$2" > "$payload" ;;
  *)
    exit 0 ;;
esac

"${SWITCHBOARD_EMACSCLIENT:-emacsclient}" \
  ${SWITCHBOARD_SOCKET:+--socket-name="$SWITCHBOARD_SOCKET"} --no-wait \
  --eval "(switchboard-hook \"$SWITCHBOARD_ID\" \"$event\" \"$payload\")" \
  > /dev/null 2>&1
exit 0
