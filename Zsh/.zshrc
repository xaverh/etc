HISTFILE=~/.history
HISTSIZE=2147483647
SAVEHIST=$HISTSIZE

setopt appendhistory \
autocd \
autopushd \
nobeep \
extendedglob \
globstarshort \
histignorealldups \
histignorespace \
histreduceblanks \
histsavenodups \
longlistjobs \
nomatch \
nonotify \
pushdignoredups \
sharehistory \
warncreateglobal
# TODO: options from Input/Output in zshoptions(1) onward

bindkey -v
bindkey -M vicmd '/' history-incremental-search-backward
bindkey -M vicmd '?' history-incremental-search-forward
# The following lines were added by compinstall
zstyle :compinstall filename '/home/xha/.zshrc'

autoload -Uz compinit
compinit
# End of lines added by compinstall

autoload -U colors && colors
PROMPT='%B%~ %#%b '
RPROMPT="%(?..%{$fg[red]%}%?%{$reset_color%})"

# use colored filenames in completion
zstyle ':completion:*:default' list-colors ${(s.:.)LS_COLORS}

# menu style completion
zstyle ':completion:*:default' menu select=2

# manpage colors
export LESS_TERMCAP_mb=$'\E[00;32m'     # begin blinking
export LESS_TERMCAP_md=$'\E[00;94m'     # begin bold
export LESS_TERMCAP_us=$'\E[01;95m'     # begin underline
export LESS_TERMCAP_so=$'\E[00;100;2m'  # begin standout-mode
export LESS_TERMCAP_me=$'\E[0m'         # end bold
export LESS_TERMCAP_ue=$'\E[0m'         # end underline
export LESS_TERMCAP_se=$'\E[0m'         # end standout-mode
export GROFF_NO_SGR=1

unalias you 2> /dev/null
unalias ls 2> /dev/null
alias grep="grep --color=auto"
alias ...='../..'
alias ....='../../..'
if ls --color=auto &> /dev/null; then
	alias ls='ls --classify --color=auto'
	alias la='ls -l --almost-all --human-readable --group-directories-first'
	alias ll='ls -l --human-readable --group-directories-first'
	alias l='ls -l --human-readable --almost-all'
	alias lx='ll -X'
else
	alias ls='ls -GFp'
	alias la='ls -lhA'
	alias ll='ls -lh'
fi

alias dfh='df -H -T'
alias -g G='|& grep -i --colour=auto'
alias d='dirs -v'
alias j='jobs -l'
alias -g IX="| curl -F 'f:1=<-' ix.io"

chpwd () {print -Pn "\e]0;%n@%m: $0 %~ ($TERM)\a"}
precmd () {print -Pn "\e]0;%n@%m: $0 %~ ($TERM)\a"}
preexec () {print -Pn "\e]0;%n@%m: $2 ($TERM)\a"}

sx simple-extract () {
	for f in "$@"; do
		if [[ -f "$f" ]]; then
			case "$f" in
				*.tar.bz2)	tar -xvjf "$f"		;;
				*.tar.gz)	tar -xvzf "$f"		;;
				*.tar.lz)   	tar --lzip -xvf "$f"	;;
				*.tar.xz)	tar -xvJf "$f"		;;
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
				*.Z)		uncompress "$f"		;;
				*)		echo "'$f' Error: compression type unknown to sx." ;;
			esac
		else
			echo "Error: '$f' is not a valid file"
		fi
	done
}

# required for st
# http://zsh.sourceforge.net/FAQ/zshfaq03.html#l25
# if [[ $TERM == (tmux*|st*) ]]; then
	# function zle-line-init () { echoti smkx }
	# function zle-line-finish () { echoti rmkx }
	# zle -N zle-line-init
	# zle -N zle-line-finish
# fi

autoload -U zmv

autoload -U tetris
zle -N tetris
bindkey "^Xt" tetris

export NNN_TMPFILE="/tmp/nnn"
export NNN_USE_EDITOR=1

n()
{
        nnn "$@"

        if [ -f $NNN_TMPFILE ]; then
                . $NNN_TMPFILE
                rm $NNN_TMPFILE
        fi
}

ripsite()
{
	wkhtmltopdf --images --no-stop-slow-scripts "$@" `systemd-escape $@`".pdf"
}

alias nnn=n
