export EDITOR=/usr/bin/vi
export PAGER=less
export SSH_AUTH_SOCK="${XDG_RUNTIME_DIR}/ssh-agent.socket"
export MAN_POSIXLY_CORRECT=1
export AWS_REGION=eu-central-1
export CM_DIR="$XDG_RUNTIME_DIR"
export LESSHISTFILE="${XDG_CACHE_HOME:-$HOME/.cache}/lesshst"
export GNUPGHOME="${XDG_CONFIG_HOME:-$HOME/.config}/gnupg"
export XAUTHORITY="$XDG_RUNTIME_DIR"/Xauthority

export GROFF_NO_SGR=1

if [ -z "${DISPLAY}" ] && [ "${XDG_VTNR}" -eq 1 ]; then
  exec startx
fi

if [ -e /home/xha/.nix-profile/etc/profile.d/nix.sh ]; then . /home/xha/.nix-profile/etc/profile.d/nix.sh; fi


