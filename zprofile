export GOPATH="$XDG_DATA_HOME/go"
export NPMPATH="$XDG_DATA_HOME/npm"
export npm_config_userconfig="$XDG_CONFIG_HOME/npmrc"
export npm_config_prefix="$NPMPATH"
export npm_config_cache="$XDG_CACHE_HOME/npm"
export NODE_REPL_HISTORY="$XDG_CACHE_HOME/node_repl_history"
export EDITOR=/usr/bin/vim
export PAGER=less
#export SSH_AUTH_SOCK="${XDG_RUNTIME_DIR}/ssh-agent.socket"
export MAN_POSIXLY_CORRECT=1
export AWS_REGION=eu-central-1
#export CM_DIR="$XDG_RUNTIME_DIR"
export LESSHISTFILE="${XDG_CACHE_HOME:-$HOME/.cache}/lesshst"
export GNUPGHOME="${XDG_CONFIG_HOME:-$HOME/.config}/gnupg"
export XAUTHORITY="$XDG_DATA_HOME/Xauthority"
export XENVIRONMENT="$XDG_CONFIG_HOME/Xdefaults"
export CCACHE_DIR="$XDG_CACHE_HOME/ccache"
export GNUPGHOME="$XDG_DATA_HOME/gnupg"
export TERMINFO_DIRS=/usr/local/share/terminfo:
export LESS_TERMCAP_mb="[00;34m"
export LESS_TERMCAP_md="[01;36m"
export LESS_TERMCAP_us="[01;35m"
export LESS_TERMCAP_ue="[0m"
export LESS_TERMCAP_me="[0m"

export GROFF_NO_SGR=1

if [ -z "${DISPLAY}" ] && [ "${XDG_VTNR}" -eq 1 ]; then
  exec startx
fi

. /home/xha/.nix-profile/etc/profile.d/nix.sh


