#!/bin/sh
# Reports Claude Code and Codex events to Switchboard in Emacs.
#
#   Claude Code hook:  switchboard-hook.sh claude <Event>      (payload JSON on stdin)
#   Codex notify:      switchboard-hook.sh codex <payload-json>
#
# Switchboard sets SWITCHBOARD_ID (and the server socket) in each session's
# environment; anything started outside Switchboard is ignored. Payloads go
# through a temp file, not argv, because they can be large. Most tool events
# carry no payload, Emacs only needs to know they happened; a subagent's, or
# an Agent call starting one, does. Their fields come before the tool's
# input, so the first 2 KB tell.

[ -n "$SWITCHBOARD_ID" ] || exit 0

case "$1" in
  claude)
    event=$2
    case "$event" in
      SessionStart|UserPromptSubmit|PermissionRequest|Stop|StopFailure|SubagentStart|SubagentStop)
        payload=$(mktemp -t switchboard) && cat > "$payload" ;;
      PreToolUse|PostToolUse|PostToolUseFailure)
        payload=$(mktemp -t switchboard) && cat > "$payload" &&
          { head -c 2048 "$payload" | grep -qE '"agent_id":|"tool_name":"(Agent|Task)"' ||
              { rm -f "$payload"; payload=; }; } ;;
      *)
        cat > /dev/null; payload= ;;
    esac ;;
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
