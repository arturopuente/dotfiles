#!/usr/bin/env bash
# Run Switchboard's tests, with Doom's packages (transient and its
# dependencies) on the load path.
set -euo pipefail
cd "$(dirname "$0")"

load_path=()
for build in "${EMACSDIR:-$HOME/.config/emacs}"/.local/straight/build-*/; do
  for dir in "$build"*/; do load_path+=(-L "$dir"); done
done

# Prefer the .el: Emacs compiles switchboard.elc here, and it can be stale.
# -q, not -Q: -Q also skips emacs-plus's site-start.el, which points native
# compilation at the current gcc; without it, linking fails (emutls_w).
exec emacs -q --batch --eval '(setq load-prefer-newer t)' "${load_path[@]}" -L . -l switchboard-test.el \
     -f ert-run-tests-batch-and-exit
