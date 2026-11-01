alias l="ls -lah"
alias cda="cd ~/dev/arturo"
alias cdd="cd ~/Downloads"
alias cdcf="cd ~/dev/cf"
alias cdsp="cd ~/dev/sp"

# Ruby
alias b="bundle exec"

# Git
alias gs="git status"
alias gl="git lg"

# GUI Emacs on macOS starts in ~ when its stdin isn't a terminal (as with
# `emacs . &`), taking itself for a Dock launch; `.` then opens ~. Keep the
# shell's directory. Run it from its real path: through the symlinks
# (/Applications/Emacs.app, /opt/homebrew/opt) macOS prints
# "sandbox_extension_issue_file_to_process failed" on every launch.
function emacs --wraps emacs
    set -l bin (path resolve /Applications/Emacs.app)/Contents/MacOS/Emacs
    test -x $bin; or set bin (command -s emacs)
    $bin --chdir $PWD $argv
end
