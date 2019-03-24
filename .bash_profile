#
# ~/.bash_profile
#

test -z "$PROFILEREAD" && . /etc/profile || true

[[ -f ~/.bashrc ]] && . ~/.bashrc

PATH="$PATH:/usr/local/bin:/usr/bin:$HOME/bin:$HOME/.npm-packages/bin:${ANDROID_HOME}/emulator:${ANDROID_HOME}/tools/bin:${ANDROID_HOME}/tools:${ANDROID_HOME}/platform-tools"
export PATH

export XDG_CONFIG_HOME="$HOME/.config"
export XDG_CONFIG_DIRS=/usr/etc/xdg:/etc/xdg

SSH_AUTH_SOCK="$XDG_RUNTIME_DIR/ssh-agent.socket"
export SSH_AUTH_SOCK

GOPATH="$HOME/.gopath/"
export GOPATH

export MAN_POSIXLY_CORRECT=1
export EDITOR=nvim
export VISUAL=nvim
export PAGER=less
export TERMINAL=urxvt256c-mlc
export AWS_REGION=eu-central-1

# Following automatically calls "startx" when you login:
[[ -z $DISPLAY && $XDG_VTNR -eq 1 ]] && exec startx -- -keeptty -nolisten tcp > ~/.xorg.log 2>&1

# (( EUID != 0 )) && umask 0077

