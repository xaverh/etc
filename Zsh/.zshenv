typeset -U path
export GOPATH=$HOME/.go
path=(/usr/local/bin /usr/bin /usr/sbin /bin /sbin ~/.local/bin ~/.npm-packages/bin $path $GOPATH/bin /var/lib/snapd/snap/bin)
export PATH
