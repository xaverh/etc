fpath=($fpath $HOME/.zsh/func)
typeset -U fpath

# set environment vars for Intel®  C++  Compiler
# Intel() {
# 	emulate -L sh
# 	. /opt/intel/bin/compilervars.sh intel64 2> /dev/null
# }

# User specific environment and startup programs

PATH="$PATH:$HOME/bin:$HOME/.npm-packages/bin"

export PATH

GOPATH="$HOME/.gopath/"
export GOPATH
