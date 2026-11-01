#!/bin/sh
input=$(cat)

used=$(echo "$input" | jq -r '.context_window.used_percentage // empty')
# Tokens currently in the context window (input incl. cache) — same basis as used_percentage.
ctx_tokens=$(echo "$input" | jq -r '.context_window.total_input_tokens // 0')
transcript=$(echo "$input" | jq -r '.transcript_path // empty')
# Subscription usage in the current 5-hour window; absent before the first response.
five_h=$(echo "$input" | jq -r '.rate_limits.five_hour.used_percentage // empty')
five_reset=$(echo "$input" | jq -r '.rate_limits.five_hour.resets_at // empty')

# Green under 50%, yellow from 50%, red from 80%.
pct_color() {
  if [ "$1" -ge 80 ]; then printf '\033[31m'
  elif [ "$1" -ge 50 ]; then printf '\033[33m'
  else printf '\033[32m'; fi
}

if [ "$ctx_tokens" -ge 1000000 ]; then
  tok_display=$(echo "$ctx_tokens" | awk '{printf "%.1fM", $1/1000000}')
elif [ "$ctx_tokens" -ge 1000 ]; then
  tok_display="$((ctx_tokens / 1000))k"
else
  tok_display="$ctx_tokens"
fi

if [ -n "$used" ]; then
  used_int=$(printf "%.0f" "$used")
  ctx_part=$(printf "$(pct_color "$used_int")ctx: %s%% / %s\033[0m" "$used_int" "$tok_display")
else
  ctx_part=$(printf "\033[90mctx: --\033[0m")
fi

five_part=""
if [ -n "$five_h" ]; then
  five_int=$(printf "%.0f" "$five_h")
  # From 50% on, add the time until the window resets: 1h40m or 12m.
  reset_part=""
  if [ -n "$five_reset" ] && [ "$five_int" -ge 50 ]; then
    left=$(( ${five_reset%.*} - $(date +%s) ))
    if [ "$left" -gt 0 ]; then
      h=$((left / 3600)); m=$(((left % 3600) / 60))
      if [ "$h" -gt 0 ]; then reset_part=" ${h}h${m}m"; else reset_part=" ${m}m"; fi
    fi
  fi
  five_part=$(printf "  \033[36m5h: %s%%%s\033[0m" "$five_int" "$reset_part")
fi

tool_part=""
if [ -n "$transcript" ] && [ -f "$transcript" ]; then
  last_tool=$(tail -200 "$transcript" \
    | jq -r 'select(.type == "assistant") | .message.content[]? | select(.type == "tool_use") | .name' 2>/dev/null \
    | tail -1)
  if [ -n "$last_tool" ]; then
    tool_part=$(printf "  \033[35mtool: %s\033[0m" "$last_tool")
  fi
fi

printf "%s%s%s" "$ctx_part" "$five_part" "$tool_part"
