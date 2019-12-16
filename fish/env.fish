set fish_greeting
set fish_color_cwd "bbbbbb"

# Local Bin
set PATH "/usr/local/bin" $PATH
set EDITOR vim

# chruby config
source /usr/local/share/chruby/chruby.fish
source /usr/local/share/chruby/auto.fish

# For compilers to find zlib and sqlite you may need to set:
export LDFLAGS="$LDFLAGS -L/usr/local/opt/zlib/lib"
export LDFLAGS="$LDFLAGS -L/usr/local/opt/sqlite/lib"
export CPPFLAGS="$CPPFLAGS -I/usr/local/opt/zlib/include"
export CPPFLAGS="$CPPFLAGS -I/usr/local/opt/sqlite/include"
# For pkg-config to find zlib and sqlite you may need to set:
export PKG_CONFIG_PATH="$PKG_CONFIG_PATH /usr/local/opt/zlib/lib/pkgconfig"
export PKG_CONFIG_PATH="$PKG_CONFIG_PATH /usr/local/opt/sqlite/lib/pkgconfig"
export LC_ALL='en_US.UTF-8'
export LANG='en_US.UTF-8'
