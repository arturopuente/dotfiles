# Local Bin
set PATH "/usr/local/bin:/usr/local/sbin" $PATH

# Rbenv bin and shims of the current ruby
set PATH $HOME/.rbenv/bin $PATH
set PATH $HOME/.rbenv/shims $PATH
set PATH $HOME/.local/bin $PATH

# Rehash rbenv to get the latest rubies installed
rbenv rehash >/dev/null ^&1

set PATH "/usr/local/mysql/bin" $PATH
set PATH "/opt/nginx/sbin" $PATH
set PATH "/opt/local/bin" $PATH
set PATH "/opt/local/sbin" $PATH
set PATH $HOME/bin $PATH

set fish_greeting ""
set fish_color_cwd "bbbbbb"
