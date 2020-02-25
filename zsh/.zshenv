export XDG_CONFIG_HOME=$HOME/.config
export XDG_CACHE_HOME=$HOME/.cache
export XDG_DATA_HOME=$HOME/.local/share
export GOPATH=$XDG_DATA_HOME/go
export NPMPATH=$XDG_DATA_HOME/npm

typeset -U path
path=(/usr/local/bin /usr/bin /usr/sbin /bin /sbin $path ~/.local/bin $NPMPATH/bin $GOPATH/bin /var/lib/snapd/snap/bin)
export PATH
