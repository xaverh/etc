# Get the aliases and functions
if [ -f ~/.zshrc ]; then
	. ~/.zshrc
fi

# set environment vars for Intel®  C++  Compiler
# Intel() {
# 	emulate -L sh
# 	. /opt/intel/bin/compilervars.sh intel64 2> /dev/null
# }

# User specific environment and startup programs

PATH="$HOME/.npm-global/bin:$PATH"

export PATH
