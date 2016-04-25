# Local Bin
set PATH "/usr/local/bin:/usr/local/sbin:" $PATH
set PATH $HOME/bin $PATH
set EDITOR vim

set PATH /opt/boxen/homebrew/bin $PATH

# Rbenv bin and shims of the current ruby
set PATH ~/.rbenv/bin $PATH
set PATH ~/.rbenv/shims $PATH

# Boxen binaries
set PATH /opt/boxen/local/bin $PATH
set PATH /opt/boxen/homebrew/sbin $PATH


set NVM_DIR ~/.nvm
bass source (brew --prefix nvm)/nvm.sh

# Postgres

set PATH /Applications/Postgres.app/Contents/MacOS $PATH

# Rehash rbenv to get the latest rubies installed
rbenv rehash >/dev/null ^&1

set PATH "/usr/local/mysql/bin" $PATH
set PATH "/opt/nginx/sbin" $PATH
set PATH "/opt/local/bin" $PATH
set PATH "/opt/local/sbin" $PATH

set fish_greeting ""
set fish_color_cwd "bbbbbb"
