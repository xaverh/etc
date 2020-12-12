export LESS_TERMCAP_mb="[00;34m"
export LESS_TERMCAP_md="[01;36m"
export LESS_TERMCAP_us="[01;35m"
export LESS_TERMCAP_ue="[0m"
export LESS_TERMCAP_me="[0m"
export NNN_COLORS=4256
export NNN_OPTS=xe
export NNN_FCOLORS=0b0304010f0e060740020a08
export NNN_PLUG='i:imgview;c:-_code -r $nnn*;x:sx;h:-hexview;v:-_mpv --force-window=yes $nnn*;V:-_mpv --shuffle --force-window=yes $nnn*;u:-uidgid;G:getplugs'
export GCC_COLORS="error=01;31:warning=01;35:note=01;36:caret=01;32:locus=01:quote=01"
export LS_COLORS='rs=0:di=1;34:tw=1;3;94:ow=1;94:st=1;3;34:ex=1;31:sg=1;3;31:su=1;3;91:ca=1;4;31:ln=36:mh=96:or=38;5;64:mi=37:bd=93:cd=33:pi=32:so=92:do=4;92:*.js=38;2;23;23;23;48;2;221;224;90:*.jsx=38;2;23;23;23;48;2;221;224;90:*.ts=48;2;43;116;137;38;2;229;230;230:*.tsx=48;2;43;116;137;38;2;229;230;230:*.vue=38;2;44;62;80;48;2;65;184;131:*.cpp=48;2;243;75;125:*.cxx=48;2;243;75;125:*.cc=48;2;243;75;125:*.hpp=48;2;243;75;125:*.hxx=48;2;243;75;125:*.hh=48;2;243;75;125:*.c=7:*.h=7:*.go=38;2;229;230;230;48;2;0;173;216:*.hs=38;2;94;80;134;48;2;235;228;243:*.svelte=48;2;229;230;230;38;2;255;62;0:*.lua=48;2;0;0;128;38;2;229;230;230:*.html=38;2;229;230;230;48;2;227;76;38:*.htm=38;2;229;230;230;48;2;227;76;38:*.xhtml=38;2;229;230;230;48;2;227;76;38:*.css=38;2;229;230;230;48;2;86;61;124:*.scss=38;2;229;230;230;48;2;207;100;154:*.sass=38;2;229;230;230;48;2;207;100;154:*.nix=48;2;126;126;255:*.vim=48;2;25;159;75;38;2;204;204;153:*vimrc=48;2;25;159;75;38;2;204;204;153:*Makefile.in=37:*CMakeCache.txt=37:*.la=37:*.o=37:*.lo=37:*.dyn_hi=37:*.cache=37:*.dyn_o=37:*.hi=37:*.errors=37:*.class=37:*.aux=37:*.bbl=37:*.ilg=37:*.idx=37:*.blg=37:*.out=37:*.toc=37:*.ind=37:*.sty=37:*.synctex.gz=37:*.fdb_latexmk=37:*.fls=37:*.bcf=37:*.bc=37:*.pyc=37:*.rlib=37:*.sconsign.dblite=37:*.scons_opt=37:*.git=37:*package-lock.json=37:*.avi=38;5;68:*.flv=38;5;68:*.m4v=38;5;68:*.mkv=38;5;68:*.mov=38;5;68:*.mp4=38;5;68:*.mpeg=38;5;68:*.mpg=38;5;68:*.ogv=38;5;68:*.vid=38;5;68:*.webm=38;5;68:*.wmv=38;5;68:*.aac=38;5;70:*.aup=38;5;70:*.flac=38;5;70:*.m4a=38;5;70:*.mp3=38;5;70:*.oga=38;5;70:*.ogg=38;5;70:*.opus=38;5;70:*.wav=38;5;70:*.wma=38;5;70:*.wv=38;5;70:*.jpeg=38;5;44:*.cbr=38;5;33:*.cbz=38;5;33:*.epub=38;5;33:*.7z=35:*.a=35:*.ace=35:*.alz=35:*.apk=35:*.arc=35:*.arj=35:*.bz=35:*.bz2=35:*.cab=35:*.cpio=35:*.deb=35:*.gz=35:*.jar=35:*.lha=35:*.lz=35:*.lzh=35:*.lzma=35:*.lzo=35:*.nar=35:*.pax=35:*.rar=35:*.rpm=35:*.rz=35:*.t7z=35:*.tar=35:*.tbz=35:*.tbz2=35:*.tgz=35:*.tlz=35:*.txz=35:*.tZ=35:*.tzo=35:*.war=35:*.xbps=35:*.xpi=35:*.xz=35:*.Z=35:*.zip=35:*.zstd=35:*.pid=90:*.swp=90:*.tmp=90:*.bak=90:*.orig=90:*.lock=90:*.log=90:*~=90:*COPYRIGHT=90:*LICENSE=90:*LICENSE-MIT=90:*COPYING=90:*LICENSE-APACHE=90:'

HISTFILE=$XDG_DATA_HOME/zsh_history
HISTSIZE=2147483647
SAVEHIST=$HISTSIZE


alias mv='nocorrect mv -i'
alias cp='nocorrect cp'
alias mkdir='nocorrect mkdir'
alias rmdir=' rmdir'
alias rm=' nocorrect rm'

# if [[ -x /usr/local/bin/exa ]]; then
       # alias ls="exa -F"
       # alias ll="exa -Fl@ --git"
       # alias la="exa -Fl@a --git"
       # alias l="exa -Fl@ --git --group-directories-first"
       # alias lx="exa -Fl@ --sort=ext --git --group-directories-first"
# else
alias ls="ls --color=auto --classify --dereference-command-line-symlink-to-dir"
alias ll="ls -l --si"
alias la="ll --almost-all"
alias l="ll --group-directories-first"
alias lx="ll -X"
# fi

alias ip="ip --color=auto"
alias grep="grep --exclude-dir=node_modules --color=auto"
alias f="df -H -T"
alias d="dirs -v"
alias u="du -s --si"
alias p="ps aux | grep"
alias -g ...=../..
alias -g ....=../../..
alias -g .....=../../../..
alias -g ......=../../../../..
alias -g .......=../../../../../..

#hash -d etc=~/src/github.com/xaverh/etc
#hash -d void=~/src/github.com/void-linux/void-packages

autoload -U colors && colors

if [[ -n $SSH_CONNECTION ]]; then
	PROMPT="%F{green}%m:%B%/ %#%b%f "
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

disable -p '^'

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
#zstyle ':completion:*' preserve-prefix '//[^/]##/'
#zstyle ':completion:*' select-prompt %SScrolling active: current selection at %p%s
#zstyle ':completion:*' squeeze-slashes true
#zstyle ':completion:*' substitute 1
## https://unix.stackexchange.com/questions/2179/rebuild-auto-complete-index-or-whatever-its-called-and-binaries-in-path-cach

# FIXME does not work within tmux sessions
chpwd () { print -Pn "\e]0;$TERM: ($USERNAME@$HOST) $0 %~\a"; }
preexec () { print -n "\e]0;$TERM: ($USERNAME@$HOST) $SHELL: $2 \a"; }


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

. /usr/share/defaults/etc/profile.d/10-command-not-found.sh 

# Don't show '%' for EOL missing '\n'
PROMPT_EOL_MARK=""

umask 022

