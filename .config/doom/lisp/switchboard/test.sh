#!/usr/bin/env bash
# Run Switchboard's tests, with Doom's packages (transient and its
# dependencies) on the load path.
set -euo pipefail
cd "$(dirname "$0")"

load_path=()
for build in "${EMACSDIR:-$HOME/.config/emacs}"/.local/straight/build-*/; do
  for dir in "$build"*/; do load_path+=(-L "$dir"); done
done

exec emacs -Q --batch "${load_path[@]}" -L . -l switchboard-test.el \
     -f ert-run-tests-batch-and-exit
