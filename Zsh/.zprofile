PATH="$PATH:/usr/local/bin:/usr/bin:$HOME/bin:$HOME/.npm-packages/bin:${ANDROID_HOME}/tools:${ANDROID_HOME}/platform-tools"
export PATH
export XDG_CONFIG_HOME="$HOME/.config"
export XDG_CONFIG_DIRS=/usr/etc/xdg:/etc/xdg

SSH_AUTH_SOCK="$XDG_RUNTIME_DIR/ssh-agent.socket"
export SSH_AUTH_SOCK

# Following automatically calls "startx" when you login:
[[ -z $DISPLAY && $XDG_VTNR -eq 1 ]] && exec startx -- -keeptty -nolisten tcp > ~/.xorg.log 2>&1

(( EUID != 0 )) && umask 0077
