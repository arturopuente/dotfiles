set fish_greeting
set fish_color_cwd "bbbbbb"

# Local Bin
set PATH "/usr/local/bin" $PATH
set PATH "/opt/homebrew/bin" $PATH
set PATH "/Users/arturo/.emacs.d/bin" $PATH
set PATH "/Users/arturo/.yarn/bin" $PATH
set EDITOR vim

# chruby config
# source /usr/local/share/chruby/chruby.fish
# source /usr/local/share/chruby/auto.fish
# the paths at /usr/local seem to the default for intel-based systems
# using the new apple silicon paths below
set CHRUBY_ROOT "/opt/homebrew/Cellar/chruby/0.3.9" 
source /opt/homebrew/Cellar/chruby-fish/0.8.2/share/chruby/chruby.fish
source /opt/homebrew/Cellar/chruby-fish/0.8.2/share/chruby/auto.fish

# For compilers to find zlib and sqlite you may need to set:
# export LDFLAGS="$LDFLAGS -L/usr/local/opt/zlib/lib"
# export LDFLAGS="$LDFLAGS -L/usr/local/opt/sqlite/lib"
# export CPPFLAGS="$CPPFLAGS -I/usr/local/opt/zlib/include"
# export CPPFLAGS="$CPPFLAGS -I/usr/local/opt/sqlite/include"
# For pkg-config to find zlib and sqlite you may need to set:
# export PKG_CONFIG_PATH="$PKG_CONFIG_PATH /usr/local/opt/zlib/lib/pkgconfig"
# export PKG_CONFIG_PATH="$PKG_CONFIG_PATH /usr/local/opt/sqlite/lib/pkgconfig"
# export LC_ALL='en_US.UTF-8'
# export LANG='en_US.UTF-8'
# export LDFLAGS="-L/opt/homebrew/lib -L/opt/homebrew/opt/openssl/lib"

set PYENV_ROOT $HOME/.pyenv
set -x PATH $PYENV_ROOT/shims $PYENV_ROOT/bin $PATH
pyenv rehash