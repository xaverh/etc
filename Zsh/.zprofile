SSH_AUTH_SOCK="$XDG_RUNTIME_DIR/ssh-agent.socket"
export SSH_AUTH_SOCK

export MAN_POSIXLY_CORRECT=1
export EDITOR=vim
export VISUAL=vim
export PAGER=less
export AWS_REGION=eu-central-1
export CM_LAUNCHER=dmenu
export CM_DIR=$XDG_RUNTIME_DIR
export READNULLCMD=less
export GO111MODULE=on

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
