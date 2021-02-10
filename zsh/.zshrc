HISTFILE="$XDG_DATA_HOME/zsh_history"
HISTSIZE=2147483647
SAVEHIST=$HISTSIZE

export NNN_COLORS=4256
export NNN_OPTS=xe
export NNN_FCOLORS=0b0304010f0e060740020a08
export NNN_PLUG='i:imgview;c:-_code -r $nnn*;x:sx;h:-hexview;v:-_mpv --force-window=yes $nnn*;V:-_mpv --shuffle --force-window=yes $nnn*;u:-uidgid;G:getplugs'

setopt appendhistory \
       autocd \
       autopushd \
       beep \
       no_clobber \
       completeinword \
       correct \
       extendedglob \
       extendedhistory \
       no_globdots \
       hashlistall \
       histignorealldups \
       histignorespace \
       histreduceblanks \
       histsavenodups \
       no_hup \
       longlistjobs \
       notify \
       printexitvalue \
       pushdignoredups \
       sharehistory \
       no_shwordsplit \
       unset
unsetopt nomatch
bindkey -e

# setopt noaliasfuncdef \
# appendcreate \
# cbases \
# noflowcontrol \
# globstarshort \
# interactivecomments \
# nomultifuncdef \
# nolistbeep \
# nomatch \
# nonotify \
# octalzeroes \
# promptpercent \
# norcquotes \
# normstarsilent \
# transientrprompt \
# warncreateglobal

autoload -U colors && colors

zmodload zsh/complist
autoload -Uz compinit && compinit

zstyle ':completion:*' menu select=2

zstyle ':completion:*:corrections' format $'%{[0;31m%}%d (errors: %e)%{[0m%}'
zstyle ':completion:*:descriptions' format $'%{[0;36m%}completing %B%d%b%{[0m%}'
zstyle ':completion:*:correct:*' insert-unambiguous true
zstyle ':completion:*:approximate:' max-errors 'reply=( $((($#PREFIX+$#SUFFIX)/3 )) numeric )'
zstyle ':completion:*:correct:*' original true

# activate color-completion
zstyle ':completion:*:default' list-colors ${(s.:.)LS_COLORS}

# automatically complete 'cd -<tab>' and 'cd -<ctrl-d>' with menu
zstyle ':completion:*:*:cd:*:directory-stack' menu yes select

# insert all expansions for expand completer
zstyle ':completion:*:expand:*' tag-order all-expansions
zstyle ':completion:*:history-words' list false

# activate menu
zstyle ':completion:*:history-words' menu yes

# ignore duplicate entries
zstyle ':completion:*:history-words' remove-all-dups yes
zstyle ':completion:*:history-words' stop yes

# match uppercase from lowercase
zstyle ':completion:*' matcher-list 'm:{a-z}={A-Z}'

# separate matches into groups
zstyle ':completion:*:matches' group 'yes'
zstyle ':completion:*' group-name ''

zstyle ':completion:*:messages' format '%d'
zstyle ':completion:*:options' auto-description '%d'

# describe options in full
zstyle ':completion:*:options' description 'yes'

# on processes completion complete all user processes
zstyle ':completion:*:processes' command 'ps -au$USER'

# offer indexes before parameters in subscripts
zstyle ':completion:*:*:-subscript-:*' tag-order indexes parameters

# provide verbose completion information
zstyle ':completion:*' verbose true

# set format for warnings
zstyle ':completion:*:warnings' format $'%{[0;31m%}No matches for:%{[0m%} %d'

# define files to ignore for zcompile
zstyle ':completion:*:*:zcompile:*' ignored-patterns '(*~|*.zwc)'
zstyle ':completion:correct:' prompt 'correct to: %e'

# Ignore completion functions for commands you don't have:
zstyle ':completion::(^approximate*):*:functions' ignored-patterns '_*'

# Provide more processes in completion of programs like killall:
zstyle ':completion:*:processes-names' command 'ps c -u ${USER} -o command | uniq'

# complete manual by their section
zstyle ':completion:*:manuals' separate-sections true
zstyle ':completion:*:manuals.*' insert-sections true
zstyle ':completion:*:man:*' menu yes select

# provide .. as a completion
zstyle ':completion:*' special-dirs ..

# try to be smart about when to use what completer...
zstyle -e ':completion:*' completer '
            if [[ $_last_try != "$HISTNO$BUFFER$CURSOR" ]] ; then
                _last_try="$HISTNO$BUFFER$CURSOR"
                reply=(_complete _match _ignored _prefix _files)
            else
                if [[ $words[1] == (rm|mv) ]] ; then
                    reply=(_complete _files)
                else
                    reply=(_oldlist _expand _complete _ignored _correct _approximate _files)
                fi
            fi'

# command for process lists, the local web server details and host completion
zstyle ':completion:*:urls' local 'www' '/var/www/' 'public_html'

if [[ ! -d $XDG_CACHE_HOME/zsh ]]; then
	command mkdir -p "$XDG_CACHE_HOME/zsh"
fi

zstyle ':completion:*' use-cache on
zstyle ':completion:*' cache-path "$XDG_CACHE_HOME/zsh"

[[ -r ~/.ssh/config ]] && _ssh_config_hosts=(${${(s: :)${(ps:\t:)${${(@M)${(f)"$(<$HOME/.ssh/config)"}:#Host *}#Host }}}:#*[*?]*}) || _ssh_config_hosts=()
[[ -r ~/.ssh/known_hosts ]] && _ssh_hosts=(${${${${(f)"$(<$HOME/.ssh/known_hosts)"}:#[\|]*}%%\ *}%%,*}) || _ssh_hosts=()
[[ -r /etc/hosts ]] && : ${(A)_etc_hosts:=${(s: :)${(ps:\t:)${${(f)~~"$(</etc/hosts)"}%%\#*}##[:blank:]#[^[:blank:]]#}}} || _etc_hosts=()

local localname
if whence hostname >/dev/null ; then
	localname=$(hostname)
elif whence hostnamectl >/dev/null ; then
	localname=$(hostnamectl --static)
else
	localname="$(uname -n)"
fi

hosts=(
	"${localname}"
	"$_ssh_config_hosts[@]"
	"$_ssh_hosts[@]"
	"$_etc_hosts[@]"
	localhost
)

zstyle ':completion:*:hosts' hosts $hosts

for compcom in cp df feh fetchipac gpasswd head hnb ipacsum mv pal stow uname ; do
	[[ -z ${_comps[$compcom]} ]] && compdef _gnu_generic ${compcom}
done; unset compcom

compdef _hosts upgrade

bindkey -M menuselect 'i' accept-and-menu-complete

#zstyle ':completion:*' completer _list _oldlist _expand _complete _ignored _match _correct _approximate _prefix
#zstyle ':completion:*' completions 1
#zstyle ':completion:*' expand prefix suffix
#zstyle ':completion:*' file-sort access
#zstyle ':completion:*' glob 1
#zstyle ':completion:*' ignore-parents parent pwd .. directory
#zstyle ':completion:*' list-prompt %SAt %p: Hit TAB for more, or the character to insert%s
#zstyle ':completion:*' list-suffixes true
#zstyle ':completion:*' match-original both
#zstyle ':completion:*' matcher-list '' 'm:{[:lower:]}={[:upper:]}' '+r:|[._-]=* r:|=*' '+l:|=* r:|=*'
#zstyle ':completion:*' menu select=long
#zstyle ':completion:*' preserve-prefix '//[^/]##/'
#zstyle ':completion:*' select-prompt %SScrolling active: current selection at %p%s
#zstyle ':completion:*' squeeze-slashes true
#zstyle ':completion:*' substitute 1
## https://unix.stackexchange.com/questions/2179/rebuild-auto-complete-index-or-whatever-its-called-and-binaries-in-path-cach

if [[ -n $SSH_CONNECTION ]]; then
	PROMPT="%F{green}%m:%B%/ %#%b%f "
else
	PROMPT="%B%~ %#%b "
fi

# RPROMPT="%(?..%{$fg[red]%}%?%{$reset_color%})"

# FIXME does not work within tmux sessions
chpwd () { print -Pn "\e]0;$TERM: ($USERNAME@$HOST) $0 %~\a"; }
preexec () { print -n "\e]0;$TERM: ($USERNAME@$HOST) $SHELL: $2 \a"; }

hash -d etc=~/src/github.com/xaverh/etc
hash -d void=~/src/github.com/void-linux/void-packages

if [[ -x /usr/bin/exa ]]; then
       alias ls="exa -F"
       alias ll="exa -Fl@ --git"
       alias la="exa -Fl@a --git"
       alias l="exa -Fl@ --git --group-directories-first"
       alias lx="exa -Fl@ --sort=ext --git --group-directories-first"
else
       alias ls="ls --color=auto --classify --dereference-command-line-symlink-to-dir"
       alias ll="ls -l --si"
       alias la="ll --almost-all"
       alias l="ll --group-directories-first"
       alias lx="ll -X"
fi

alias ip="ip --color=auto"
alias grep="grep --exclude-dir=node_modules --color=auto"
alias mv="mv -i"
alias f="df -H -T"
alias d="dirs -v"
alias u="du -s --si"
alias p="ps aux | grep"
alias -g ...=../..
alias -g ....=../../..
alias -g .....=../../../..
alias -g ......=../../../../..
alias -g .......=../../../../../..

function unz sx() {
	local f
	for f in "$@"; do
		if [[ -f "$f" ]]; then
			case "$f" in
				*.tar.bz2|*.tbz2)
					tar -xvjf "$f"
				;;
				*.tar.gz|*.tgz)
					tar -xvzf "$f"
				;;
				*.tar.lz)
					tar --lzip -xvf "$f"
				;;
				*.tar.xz|*.txz)
					tar -xvJf "$f"
				;;
				*.tar.zst)
					tar --zstd -xvf "$f"
				;;
				*.7z|*.7z.001)
					7z x "$f"
				;;
				*.bz2)
					bzip2 -d "$f"
				;;
				*.cpio)
					cpio -rvd < "$f"
				;;
				*.deb|*.ar)
					ar -x "$f"
				;;
				*.gz)
					gunzip -d --verbose "$f"
				;;
				*.lzh)
					lha x "$f"
				;;
				*.lzma)
					unlzma "$f"
				;;
				*.pax)
					pax -r < "$f"
				;;
				*.rar)
					unrar x "$f"
				;;
				*.rpm)
					7z x "$f"
				;;
				*.tar)
					tar -xvf "$f"
				;;
				*.xz)
					xz -d "$f"
				;;
				*.zip)
					unzip "$f"
				;;
				*.zst)
					unzstd "$f"
				;;
				*.Z)
					uncompress "$f"
				;;
				*)
					echo "$f: Error: compression type unknown." && return 1
				;;
			esac
		else
			echo "Error: '$f' is not a valid file" && return 1
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

    local NNN_TMPFILE="${XDG_CACHE_HOME:-$HOME/.cache}/nnn/.lastd"

    nnn "$@"

    if [[ -f "$NNN_TMPFILE" ]]; then
            . "$NNN_TMPFILE"
            rm -f "$NNN_TMPFILE" > /dev/null
    fi
}

alias nnn=n
