# Local Bin
set PATH "/usr/local/bin" $PATH
set PATH $HOME/bin $PATH
set EDITOR vim

# Rbenv bin and shims of the current ruby
set PATH ~/.rbenv/bin $PATH
set PATH ~/.rbenv/shims $PATH

# nvm

export NVM_DIR="$HOME/.nvm"
# . "/usr/local/opt/nvm/nvm.sh"

# Rehash rbenv to get the latest rubies installed
rbenv rehash >/dev/null ^&1

set fish_greeting ""
set fish_color_cwd "bbbbbb"
