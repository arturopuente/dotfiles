#!/bin/sh
# Reports Claude Code and Codex events to Switchboard in Emacs.
#
#   Claude Code hook:  switchboard-hook.sh claude <Event>      (payload JSON on stdin)
#   Codex notify:      switchboard-hook.sh codex <payload-json>
#
# Switchboard sets SWITCHBOARD_ID (and the server socket) in each session's
# environment; anything started outside Switchboard is ignored. Payloads go
# through a temp file, not argv, because they can be large. Tool events carry
# no payload: Emacs only needs to know they happened.

[ -n "$SWITCHBOARD_ID" ] || exit 0

case "$1" in
  claude)
    event=$2
    case "$event" in
      SessionStart|UserPromptSubmit|PermissionRequest|Stop|StopFailure)
        payload=$(mktemp -t switchboard) && cat > "$payload" ;;
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
