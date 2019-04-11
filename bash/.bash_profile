#
# ~/.bash_profile
#

test -z "$PROFILEREAD" && . /etc/profile || true

[[ -f ~/.bashrc ]] && . ~/.bashrc

PATH="$PATH:/usr/local/bin:/usr/bin:$HOME/bin:$HOME/.npm-packages/bin:$HOME/.luarocks/bin"
export PATH

export XDG_CONFIG_HOME="$HOME/.config"
export XDG_CONFIG_DIRS=/usr/etc/xdg:/etc/xdg

SSH_AUTH_SOCK="$XDG_RUNTIME_DIR/ssh-agent.socket"
export SSH_AUTH_SOCK

GOPATH="$HOME/.gopath"
export GOPATH

export MAN_POSIXLY_CORRECT=1
export EDITOR=vim
export VISUAL=vim
export PAGER=less
export TERMINAL=urxvtc
export AWS_REGION=eu-central-1

# Following automatically calls "startx" when you login:
[[ -z $DISPLAY && $XDG_VTNR -eq 1 ]] && exec startx -- -keeptty -nolisten tcp > ~/.xorg.log 2>&1

# (( EUID != 0 )) && umask 0077

