SSH_AUTH_SOCK="$XDG_RUNTIME_DIR/ssh-agent.socket"
export SSH_AUTH_SOCK

export MAN_POSIXLY_CORRECT=1
export EDITOR=nvim
export VISUAL=nvim
export PAGER=less
export TERMINAL=alacritty
export AWS_REGION=eu-central-1
export CM_LAUNCHER=dmenu
export CM_DIR="$XDG_RUNTIME_DIR"
export READNULLCMD=less
export WEECHAT_HOME=${XDG_CONFIG_HOME:-$HOME/.config}/weechat
export npm_config_userconfig="${XDG_CONFIG_HOME:-$HOME/.config}/npm/.npmrc"
export npm_config_prefix="$NPMPATH"
export npm_config_cache="${XDG_CACHE_HOME:-$HOME/.cache}/npm"
export LESSHISTFILE="${XDG_CACHE_HOME:-$HOME/.cache}/lesshst"
export GNUPGHOME="${XDG_CONFIG_HOME:-$HOME/.config}/gnupg"
# https://github.com/alacritty/alacritty/issues/1501
export WINIT_HIDPI_FACTOR=1
export XCURSOR_THEME="DMZ-White"

eval `dircolors "${ZDOTDIR:-$HOME}/.dir_colors"`

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
