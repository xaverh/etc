HISTFILE="$HOME/.zsh_history"
HISTSIZE=2147483647
SAVEHIST=$HISTSIZE

alias mv='nocorrect mv -iv'
alias cp='nocorrect cp -iv'
alias mkdir='nocorrect mkdir'
alias rmdir=' rmdir -v'
alias rm=' nocorrect rm -v'
alias chmod="chmod -c"
alias chown="chown -c"
alias ln='ln -v'

alias ls="ls --color=auto -F -H"
alias l="ls -l -h --group-directories-first"
alias la="l -A"
alias lx="l -X"
function ll () {
	l $1 | less -F
}

#alias ip="ip --color=auto"
#alias grep="grep --exclude-dir=node_modules --color=auto"
alias grep="grep --color=auto"
alias f="df -h -T"
alias d="dirs -v"
alias u="du -s --si"
alias p="ps aux | grep"
alias -g ...=../..
alias -g ....=../../..
alias -g .....=../../../..
alias -g ......=../../../../..
alias -g .......=../../../../../..

hash -d config=$HOME/src/github.com/xaverh/.config

autoload -U colors && colors

if [[ -n $SSH_CONNECTION ]]; then
	PROMPT="%F{green}%m:%B%/ %#%b%f "
elif [[ -n $GUIX_ENVIRONMENT ]]; then
	PROMPT="%F{yellow}($(basename $GUIX_ENVIRONMENT)) %B%~ %#%b%f "
else
	PROMPT="%B%~ %#%b "
fi
RPROMPT="%(?..%F{red}%?%f)"

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
       interactivecomments \
       no_hup \
       longlistjobs \
       no_nomatch \
       notify \
       pushdignoredups \
       sharehistory \
       no_shwordsplit \
       unset

# disable -p '^'

zmodload zsh/complist
autoload -Uz compinit && compinit

bindkey -e
bindkey ' ' magic-space       # also do history expansion on space

# Enable past command match search with up/down arrows
autoload -Uz up-line-or-beginning-search down-line-or-beginning-search
zle -N up-line-or-beginning-search
zle -N down-line-or-beginning-search
[[ -n "$key[Up]"   ]] && bindkey -- "$key[Up]"   up-line-or-beginning-search
[[ -n "$key[Down]" ]] && bindkey -- "$key[Down]" down-line-or-beginning-search

# Enable Ctrl-R history search
bindkey '^R' history-incremental-search-backward

bindkey "^H" backward-kill-word # control + backspace

# Enable home/end keys
# http://www.zshwiki.org/home/zle/bindkeys#reading_terminfo
# create a zkbd compatible hash;
# to add other keys to this hash, see: man 5 terminfo
typeset -A key

key[Home]="$terminfo[khome]"
key[End]="$terminfo[kend]"
key[Insert]="$terminfo[kich1]"
key[Backspace]="$terminfo[kbs]"
key[Delete]="$terminfo[kdch1]"
key[Up]="$terminfo[kcuu1]"
key[Down]="$terminfo[kcud1]"
key[Left]="$terminfo[kcub1]"
key[Right]="$terminfo[kcuf1]"
key[PageUp]="$terminfo[kpp]"
key[PageDown]="$terminfo[knp]"

# setup key accordingly
[[ -n "$key[Home]"      ]] && bindkey -- "$key[Home]"      beginning-of-line
[[ -n "$key[End]"       ]] && bindkey -- "$key[End]"       end-of-line
[[ -n "$key[Insert]"    ]] && bindkey -- "$key[Insert]"    overwrite-mode
[[ -n "$key[Backspace]" ]] && bindkey -- "$key[Backspace]" backward-delete-char
[[ -n "$key[Delete]"    ]] && bindkey -- "$key[Delete]"    delete-char
[[ -n "$key[Up]"        ]] && bindkey -- "$key[Up]"        up-line-or-history
[[ -n "$key[Down]"      ]] && bindkey -- "$key[Down]"      down-line-or-history
[[ -n "$key[Left]"      ]] && bindkey -- "$key[Left]"      backward-char
[[ -n "$key[Right]"     ]] && bindkey -- "$key[Right]"     forward-char

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

if [[ ! -d ${HOME}/.cache/zsh ]]; then
	command mkdir -p "${HOME}/.cache/zsh"
fi

zstyle ':completion:*' use-cache on
zstyle ':completion:*' cache-path "$XDG_CACHE_HOME/zsh"

[[ -r ~/.ssh/config ]] && _ssh_config_hosts=(${${(s: :)${(ps:\t:)${${(@M)${(f)"$(<$HOME/.ssh/config)"}:#Host *}#Host }}}:#*[*?]*}) || _ssh_config_hosts=()
[[ -r ~/.ssh/known_hosts ]] && _ssh_hosts=(${${${${(f)"$(<$HOME/.ssh/known_hosts)"}:#[\|]*}%%\ *}%%,*}) || _ssh_hosts=()
# [[ -r /etc/hosts ]] && : ${(A)_etc_hosts:=${(s: :)${(ps:\t:)${${(f)~~"$(</etc/hosts)"}%%\#*}##[:blank:]#[^[:blank:]]#}}} || _etc_hosts=()

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
#zstyle ':completion:*' preserve-prefix '//[^/]##/'
#zstyle ':completion:*' select-prompt %SScrolling active: current selection at %p%s
#zstyle ':completion:*' squeeze-slashes true
#zstyle ':completion:*' substitute 1
## https://unix.stackexchange.com/questions/2179/rebuild-auto-complete-index-or-whatever-its-called-and-binaries-in-path-cach

# FIXME does not work within tmux sessions
chpwd () { print -Pn "\e]0;$USERNAME@$HOST: $0 %~\a"; }
preexec () { print -n "\e]0;$USERNAME@$HOST: $SHELL: $2 \a"; }


function unz() {
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

# Simple script to generate iwd network files.
function putin () {

	# Consider all characters as single-byte.
	local LC_CTYPE=C

	if [[ $# -eq 0 || $# -gt 2 ]]; then
	    print 'usage:' >&2
	    print 'open WiFi: putin ssid' >&2
	    print 'secure WiFi: putin ssid pass' >&2
	    return 1
	fi

	# The SSID appears verbatim in the name if it contains
	# only alphanumeric characters, spaces, underscores or
	# minus signs. Otherwise it is encoded as an equal sign
	# followed by the lower-case hex encoding of the name.
	local ssid
	case $1 in
	    *[!A-Za-z0-9_' '-]*)
		ssid="=$(printf %s "$1" | od -vA n -t x1 | tr -d '\n ')"
	    ;;
	    *)
		ssid=$1
	    ;;
	esac

	if [[ $# -eq 1 ]]; then
		print "Creating /var/lib/iwd/$ssid.open" >&2
		sudo touch "/var/lib/iwd/$ssid.open"
	else
		print "Creating /var/lib/iwd/$ssid.psk" >&2
		print "[Security]\nPassphrase=${2}" | sudo tee "/var/lib/iwd/$ssid.psk"
	fi
}
alias putin=" putin"

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

# Finally, make sure the terminal is in application mode, when zle is
# active. Only then are the values from $terminfo valid.
if (( ${+terminfo[smkx]} )) && (( ${+terminfo[rmkx]} )); then
	function zle-line-init () {
		echoti smkx
	}
	function zle-line-finish () {
	    echoti rmkx
	}
	zle -N zle-line-init
	zle -N zle-line-finish
fi

function 16c () {
	printf '		'
	for i in 0 1 2 3 4 5 6 7; do
    	printf '[4%sm  [m' "$i"
	done

	printf '\n		'

	for i in 0 1 2 3 4 5 6 7; do
    	printf '[10%sm  [m' "$i"
	done

	printf '\n'
}
