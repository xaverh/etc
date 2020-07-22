export SSH_AUTH_SOCK="${XDG_RUNTIME_DIR}/ssh-agent.socket"
export MAN_POSIXLY_CORRECT=1
export EDITOR=vim ###
export VISUAL=vim
export VIMINIT=":source $XDG_CONFIG_HOME"/vim/init.vim ###
export PAGER=less ###
export AWS_REGION=eu-central-1
export CM_LAUNCHER=rofi
export CM_DIR="$XDG_RUNTIME_DIR"
export READNULLCMD=less
export WEECHAT_HOME="${XDG_CONFIG_HOME:-$HOME/.config}/weechat"
export npm_config_userconfig="${XDG_CONFIG_HOME:-$HOME/.config}/npm/.npmrc"
export npm_config_prefix="$NPMPATH"
export npm_config_cache="${XDG_CACHE_HOME:-$HOME/.cache}/npm"
export NODE_REPL_HISTORY="${XDG_CACHE_HOME:-$HOME/.cache}/node_repl_history"
export LESSHISTFILE="${XDG_CACHE_HOME:-$HOME/.cache}/lesshst"
export GNUPGHOME="${XDG_CONFIG_HOME:-$HOME/.config}/gnupg"
export ABDUCO_SOCKET_DIR="${XDG_RUNTIME_DIR}"
export XAUTHORITY="$XDG_RUNTIME_DIR"/Xauthority ###
export FVWM_USERDIR="${XDG_CONFIG_HOME:-$HOME/.config}/fvwm"
export NNN_COLORS=4216
export NNN_OPTS=xe
export NNN_PLUG='i:imgview;c:-_code -r $nnn*;x:sx;d:-hexview;v:-_|mpv $nnn;V:-_mpv --shuffle $nnn*;u:-uidgid'
export NNN_BMS='c:~/.config;t:/tmp'

# manpage colors
export LESS_TERMCAP_mb=$'\E[00;32m'     # begin blinking
export LESS_TERMCAP_md=$'\E[00;94m'     # begin bold
export LESS_TERMCAP_us=$'\E[01;95m'     # begin underline
export LESS_TERMCAP_so=$'\E[00;100;2m'  # begin standout-mode
export LESS_TERMCAP_me=$'\E[0m'         # end bold
export LESS_TERMCAP_ue=$'\E[0m'         # end underline
export LESS_TERMCAP_se=$'\E[0m'         # end standout-mode
export GROFF_NO_SGR=1

# (( EUID != 0 )) && umask 0077
