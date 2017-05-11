#
# ~/.bash_profile
#

[[ -f ~/.bashrc ]] && . ~/.bashrc

ANDROID_HOME="$HOME/.android-sdk"
export ANDROID_HOME

PATH="$PATH:/usr/local/bin:/usr/bin:$HOME/bin:$HOME/.npm-packages/bin:${ANDROID_HOME}/emulator:${ANDROID_HOME}/tools/bin:${ANDROID_HOME}/tools:${ANDROID_HOME}/platform-tools"
if which ruby >/dev/null && which gem >/dev/null; then
	    PATH="$PATH:$(ruby -rubygems -e 'puts Gem.user_dir')/bin"
fi
export PATH

export XDG_CONFIG_HOME="$HOME/.config"
export XDG_CONFIG_DIRS=/usr/etc/xdg:/etc/xdg

SSH_AUTH_SOCK="$XDG_RUNTIME_DIR/ssh-agent.socket"
export SSH_AUTH_SOCK

GOPATH="$HOME/.gopath/"
export GOPATH


export EDITOR=vim
export PAGER=less

# Following automatically calls "startx" when you login:
[[ -z $DISPLAY && $XDG_VTNR -eq 1 ]] && exec startx -- -keeptty -nolisten tcp > ~/.xorg.log 2>&1

(( EUID != 0 )) && umask 0077

