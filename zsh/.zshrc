HISTFILE=~/.local/zsh_history
HISTSIZE=2147483647
SAVEHIST=$HISTSIZE

setopt noaliasfuncdef \
appendcreate \
appendhistory \
autocd \
autopushd \
cbases \
beep \
noclobber \
extendedglob \
noflowcontrol \
globstarshort \
histignorealldups \
histignorespace \
histreduceblanks \
histsavenodups \
interactivecomments \
longlistjobs \
nomultifuncdef \
nolistbeep \
nomatch \
nonotify \
octalzeroes \
promptpercent \
pushdignoredups \
norcquotes \
normstarsilent \
sharehistory \
transientrprompt \
warncreateglobal
# TODO: options from Input/Output in zshoptions(1) onward

bindkey -e
zstyle :compinstall filename "${ZDOTDIR:-$HOME}/.zshrc"

autoload -Uz compinit
compinit
[[ -r "$NPMPATH"/lib/node_modules/gulp-cli/completion/zsh  ]] && . "$NPMPATH"/lib/node_modules/gulp-cli/completion/zsh

autoload -U colors && colors

if [[ -n $SSH_CONNECTION ]]; then
	PROMPT="%F{green}%m:%B%/ %#%b%f "
else
	PROMPT="%B%~ %#%b "
fi

RPROMPT="%(?..%{$fg[red]%}%?%{$reset_color%})"

# use colored filenames in completion
zstyle ':completion:*:default' list-colors ${(s.:.)LS_COLORS}

# menu style completion
zstyle ':completion:*:default' menu select=2

# rehash commands all the time
# https://unix.stackexchange.com/questions/2179/rebuild-auto-complete-index-or-whatever-its-called-and-binaries-in-path-cach
zstyle ":completion:*:commands" rehash 1

alias vi=vim
alias grep="grep --color=auto"
alias ...='../..'
alias ....='../../..'
alias .....='../../../..'
alias ......='../../../../..'
alias .......='../../../../../..'
if [[ $OSTYPE == "linux-gnu" ]] then
	alias ls='ls --classify --color=auto --dereference-command-line-symlink-to-dir'
	alias ll='ls -l --si '
	alias la='ll --almost-all'
	alias l='ll --group-directories-first'
	alias lx='ll -X'
else
	alias ls='ls -GFp'
	alias ll='ls -lh'
	alias la='ll -A'
fi
alias lss='ls -S'
alias las='la -S'
alias lls='ll -S'
alias mv='mv -i'

alias f='df -H -T'
alias -g G='|& grep -i --colour=auto'
alias d='dirs -v'
alias -g IX="| curl -F 'f:1=<-' ix.io"
alias u='du -s --si'

alias -s {epub,pdf,ps,djvu,cbz,PDF}=zathura

alias xxup="nix-env -u"
alias xxrm="nix-env -e"
alias xxse="nix-env -qaP --description | grep"
function xxin () {
	nix-env -iA nixos.${^*}
}

chpwd () {print -Pn "\e]0;$TERM: ($USERNAME@$HOST) $0 %~\a"}
preexec () {print -n "\e]0;$TERM: ($USERNAME@$HOST) $SHELL: $2 \a"}

function sx () {
	local f
	for f in "$@"; do
		if [[ -f "$f" ]]; then
			case "$f" in
				*.tar.bz2)	tar -xvjf "$f"		;;
				*.tar.gz)	tar -xvzf "$f"		;;
				*.tar.lz)   	tar --lzip -xvf "$f"	;;
				*.tar.xz)	tar -xvJf "$f"		;;
				*.tar.zst)	tar --zstd -xvf "$f"	;;
				*.7z)		7z x "$f"		;;
				*.7z.001)	7z x "$f"		;;
				*.lzma)		unlzma "$f"		;;
				*.rar)		unrar x "$f"		;;
				*.deb)		ar -x "$f"		;;
				*.bz2)		bzip2 -d "$f"		;;
				*.lzh)		lha x "$f"		;;
				*.gz)		gunzip -d --verbose "$f";;
				*.tar)		tar -xvf "$f"		;;
				*.tgz)		tar -xvzf "$f"		;;
				*.tbz2)		tar -xvjf "$f"		;;
				*.txz)		tar -xvJf "$f"		;;
				*.xz)		7z x "$f"		;;
				*.zip)		unzip "$f"		;;
				*.zst)		unzstd "$f"		;;
				*.Z)		uncompress "$f"		;;
				*)		echo "'$f' Error: compression type unknown to $0." ;;
			esac
		else
			echo "Error: '$f' is not a valid file"
		fi
	done
}

function clinton () {
	local i
	for i in "$@"; do
		sed -i "/$i/d" "$HISTFILE"
	done
}

alias clinton=" clinton"

autoload -U zmv

function n ()
{
    if [[ -n $NNNLVL ]] && [[ "${NNNLVL:-0}" -ge 1 ]]; then
        echo "nnn is already running"
        return
    fi

    local NNN_TMPFILE="${XDG_CONFIG_HOME:-$HOME/.config}/nnn/.lastd"

    nnn "$@"

    if [[ -f "$NNN_TMPFILE" ]]; then
            . "$NNN_TMPFILE"
            rm -f "$NNN_TMPFILE" > /dev/null
    fi
}

alias nnn=n
