#
# ~/.bashrc
#

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

# directory colors
eval `dircolors $HOME/.dircolors 2> /dev/null` || eval `dircolors`
# manpage colors
export LESS_TERMCAP_mb=$'\E[00;32m'     # begin blinking
export LESS_TERMCAP_md=$'\E[00;94m'     # begin bold
export LESS_TERMCAP_us=$'\E[01;95m'     # begin underline
export LESS_TERMCAP_so=$'\E[00;100;2m'  # begin standout-mode
export LESS_TERMCAP_me=$'\E[0m'         # end bold
export LESS_TERMCAP_ue=$'\E[0m'         # end underline
export LESS_TERMCAP_se=$'\E[0m'         # end standout-mode
export GROFF_NO_SGR=1

export HISTTIMEFORMAT="%y.%m.%d %T"

# https://superuser.com/questions/479726/how-to-get-infinite-command-history-in-bash
export HISTSIZE=""
export HISTFILESIZE=""

# http://unix.stackexchange.com/questions/18212/bash-history-ignoredups-and-erasedups-setting-conflict-with-common-history/18443#18443
export HISTCONTROL=ignoredups:erasedups
shopt -s histappend
export PROMPT_COMMAND="${PROMPT_COMMAND:+$PROMPT_COMMAND$'\n'}history -a; history -c; history -r"
export HISTCONTROL=ignoreboth

set -o emacs
shopt -u checkhash compat31 compat32 compat40 compat41 compat42 compat43 dirspell dotglob globasciiranges lastpipe lithist no_empty_cmd_completion nocaseglob nocasematch
shopt -s autocd cdable_vars cdspell checkjobs checkwinsize complete_fullquote \
direxpand expand_aliases extglob extquote failglob globstar histreedit \
histverify interactive_comments progcomp promptvars sourcepath cmdhist
shopt -s hostcomplete

alias grep="grep -i --color=auto"
alias ...='../..'
alias ....='../../..'
if ls --color=auto &> /dev/null; then
	alias ls='ls --classify --color=auto'
	alias la='ls -l --almost-all --human-readable --group-directories-first'
	alias ll='ls -l --human-readable --group-directories-first'
	alias lx='ll -X'
else
	alias ls='ls -GFp'
	alias la='ls -lhA'
	alias ll='ls -lh'
fi
alias dfh='df -H'
alias wget='wget -U="Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/52.0.2743.116 Safari/537.36"'
alias 7zultra='7z a -t7z -mx=9 -mfb=64 -md=32m -ms=on'
alias d='dirs -v'
alias j='jobs -l'

PS1='\[\e[1m\]\w \$\[\e[0m\] '


h() {
	grep -a $@ $HISTFILE
}

sx () {
	for f in "$@"; do
		if [[ -f "$f" ]]; then
			case "$f" in
				*.7z)		7z x "$f"		;;
				*.7z.001)	7z x "$f"		;;
				*.lzma)		unlzma "$f"		;;
				*.tar.bz2)	tar -xvjf "$f"		;;
				*.tar.gz)	tar -xvzf "$f"		;;
				*.rar)		unrar x "$f"		;;
				*.deb)		ar -x "$f"		;;
				*.bz2)		bzip2 -d "$f"		;;
				*.lzh)		lha x "$f"		;;
				*.gz)		gunzip -d --verbose "$f";;
				*.tar)		tar -xvf "$f"		;;
				*.tgz)		tar -xvzf "$f"		;;
				*.tbz2)		tar -xvjf "$f"		;;
				*.txz)		tar -xvJf "$f"		;;
				*.tar.xz)	tar -xvJf "$f"		;;
				*.tar.lz)   	tar --lzip -xvf "$f"	;;
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

# tabtab source for electron-forge package
# uninstall by removing these lines or running `tabtab uninstall electron-forge`
[ -f /home/xha/.config/yarn/global/node_modules/tabtab/.completions/electron-forge.bash ] && . /home/xha/.config/yarn/global/node_modules/tabtab/.completions/electron-forge.bash