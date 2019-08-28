typeset -U path
path=(/usr/local/bin /usr/bin /usr/sbin /bin /sbin ~/.npm-packages/bin ~/bin $path ~/.go)
export PATH
export GOPATH=$HOME/.go