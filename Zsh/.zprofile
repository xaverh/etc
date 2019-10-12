SSH_AUTH_SOCK="$XDG_RUNTIME_DIR/ssh-agent.socket"
export SSH_AUTH_SOCK
[[ -x /usr/libexec/openssh/x11-ssh-askpass ]] && export SSH_ASKPASS="/usr/libexec/openssh/x11-ssh-askpass"
[[ -x /usr/lib/ssh/gnome-ssh-askpass ]] && export SSH_ASKPASS="/usr/lib/ssh/gnome-ssh-askpass"

export MAN_POSIXLY_CORRECT=1
export EDITOR=vim
export VISUAL=vim
export PAGER=less
export AWS_REGION=eu-central-1
export CM_LAUNCHER=rofi
export CM_DIR=$XDG_RUNTIME_DIR

# (( EUID != 0 )) && umask 0077

