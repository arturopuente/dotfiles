set fish_greeting
set fish_color_cwd "bbbbbb"

# Paths. -g keeps these in config instead of the universal fish_user_paths,
# so a fresh machine gets the same PATH from this file alone.
fish_add_path -g /opt/homebrew/bin /opt/homebrew/sbin
fish_add_path -g $HOME/.local/bin $HOME/.cargo/bin
fish_add_path -g $HOME/.config/emacs/bin

set -gx EDITOR vim

# chruby + auto-switching come from chruby-fish (brew) via vendor_conf.d

# pyenv
set -gx PYENV_ROOT $HOME/.pyenv
command -q pyenv; and pyenv init - fish | source
