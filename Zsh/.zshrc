HISTFILE=~/.history
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
zstyle :compinstall filename "$HOME/.zshrc"

vscodecompdir="$HOME"/.local/share/VSCode-linux-x64/resources/completions/zsh
[[ -d $vscodecompdir && -x $vscodecompdir ]] && fpath=($fpath $vscodecompdir)
autoload -Uz compinit
compinit

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
	alias ls='ls --classify --color=auto --dereference-command-line-symlink-to-dir'
	alias ll='ls -l --human-readable --si '
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

alias f='df -H -T'
alias -g G='|& grep -i --colour=auto'
alias d='dirs -v'
alias j='jobs -l'
alias -g IX="| curl -F 'f:1=<-' ix.io"
alias o='less'
alias u='du -s --si'

chpwd () {print -Pn "\e]0;%n@%m: $0 %~ ($TERM)\a"}
precmd () {print -Pn "\e]0;%n@%m: $0 %~ ($TERM)\a"}
preexec () {print -Pn "\e]0;%n@%m: $2 ($TERM)\a"}

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

function clinton_server () {
	local i
	for i in "$@"; do
		sed -i "/$i/d" $HISTFILE
	done
}

alias clinton_server=" clinton_server"

autoload -U zmv

autoload -U tetris
zle -N tetris
bindkey "^Xt" tetris

export NNN_TMPFILE="/tmp/nnn"
export NNN_USE_EDITOR=1

function n ()
{
        nnn "$@"

        if [ -f $NNN_TMPFILE ]; then
                . $NNN_TMPFILE
                rm $NNN_TMPFILE
        fi
}

alias nnn=n

function ripsite()
{
	wkhtmltopdf --images --no-stop-slow-scripts "$@" `systemd-escape $@`".pdf"
}

function Install-VSCode()
{
	rm -r "$HOME/.local/share/VSCode-linux-x64/" || mkdir -p "$HOME/.local/share"
	curl -L 'https://update.code.visualstudio.com/latest/linux-x64/stable' | tar xz -C "$HOME/.local/share/"
}
