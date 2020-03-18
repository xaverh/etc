HISTFILE=~/.local/share/zsh_history
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

export LS_COLORS='no=00:rs=0:fi=00:di=01;34:ln=36:mh=04;36:pi=04;01;36:so=04;33:do=04;01;36:bd=01;33:cd=33:or=31:mi=01;37;41:ex=01;36:su=01;04;37:sg=01;04;37:ca=01;37:tw=01;37;44:ow=01;04;34:st=04;37;44:*.7z=01;32:*.ace=01;32:*.alz=01;32:*.arc=01;32:*.arj=01;32:*.bz=01;32:*.bz2=01;32:*.cab=01;32:*.cpio=01;32:*.deb=01;32:*.dz=01;32:*.ear=01;32:*.gz=01;32:*.jar=01;32:*.lha=01;32:*.lrz=01;32:*.lz=01;32:*.lz4=01;32:*.lzh=01;32:*.lzma=01;32:*.lzo=01;32:*.rar=01;32:*.rpm=01;32:*.rz=01;32:*.sar=01;32:*.t7z=01;32:*.tar=01;32:*.taz=01;32:*.tbz=01;32:*.tbz2=01;32:*.tgz=01;32:*.tlz=01;32:*.txz=01;32:*.tz=01;32:*.tzo=01;32:*.tzst=01;32:*.war=01;32:*.xz=01;32:*.z=01;32:*.Z=01;32:*.zip=01;32:*.zoo=01;32:*.zst=01;32:*.aac=32:*.au=32:*.flac=32:*.m4a=32:*.m4b=32:*.mid=32:*.midi=32:*.mka=32:*.mp3=32:*.mpa=32:*.mpeg=32:*.mpg=32:*.ogg=32:*.opus=32:*.ra=32:*.wav=32:*.3des=01;35:*.aes=01;35:*.gpg=01;35:*.pgp=01;35:*.doc=32:*.docx=32:*.dot=32:*.odg=32:*.odp=32:*.ods=32:*.odt=32:*.otg=32:*.otp=32:*.ots=32:*.ott=32:*.pdf=32:*.ppt=32:*.pptx=32:*.xls=32:*.xlsx=32:*.app=01;36:*.bat=01;36:*.btm=01;36:*.cmd=01;36:*.com=01;36:*.exe=01;36:*.reg=01;36:*~=02;37:*.bak=02;37:*.BAK=02;37:*.log=02;37:*.log=02;37:*.old=02;37:*.OLD=02;37:*.orig=02;37:*.ORIG=02;37:*.swo=02;37:*.swp=02;37:*.bmp=32:*.cgm=32:*.dl=32:*.dvi=32:*.emf=32:*.eps=32:*.gif=32:*.jpeg=32:*.jpg=32:*.JPG=32:*.mng=32:*.pbm=32:*.pcx=32:*.pgm=32:*.png=32:*.PNG=32:*.ppm=32:*.pps=32:*.ppsx=32:*.ps=32:*.svg=32:*.svgz=32:*.tga=32:*.tif=32:*.tiff=32:*.webp=32:*.xbm=32:*.xcf=32:*.xpm=32:*.xwd=32:*.xwd=32:*.yuv=32:*.anx=32:*.asf=32:*.avi=32:*.axv=32:*.flc=32:*.fli=32:*.flv=32:*.gl=32:*.m2v=32:*.m4v=32:*.mkv=32:*.mov=32:*.MOV=32:*.mp4=32:*.mpeg=32:*.mpg=32:*.nuv=32:*.ogm=32:*.ogv=32:*.ogx=32:*.qt=32:*.rm=32:*.rmvb=32:*.swf=32:*.vob=32:*.webm=32:*.wmv=32:'

bindkey -e
zstyle :compinstall filename "${ZDOTDIR:-$HOME}/.zshrc"

autoload -Uz compinit
compinit
[[ -r "$NPMPATH"/lib/node_modules/gulp-cli/completion/zsh  ]] && . "$NPMPATH"/lib/node_modules/gulp-cli/completion/zsh

_kitty() {
    local src
    # Send all words up to the word the cursor is currently on
    src=$(printf "%s
" "${(@)words[1,$CURRENT]}" | kitty +complete zsh)
    if [[ $? == 0 ]]; then
        eval ${src}
    fi
}
compdef _kitty kitty

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

alias vi=nvim
alias grep="grep --color=auto"
alias ...='../..'
alias ....='../../..'
alias .....='../../../..'
alias ......='../../../../..'
alias .......='../../../../../..'
if ls --color=auto &> /dev/null; then
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
alias j='jobs -l'
alias -g IX="| curl -F 'f:1=<-' ix.io"
alias u='du -s --si'

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
    if [ -n $NNNLVL ] && [ "${NNNLVL:-0}" -ge 1 ]; then
        echo "nnn is already running"
        return
    fi

    local NNN_TMPFILE="${XDG_CONFIG_HOME:-$HOME/.config}/nnn/.lastd"

    nnn "$@"

    if [ -f "$NNN_TMPFILE" ]; then
            . "$NNN_TMPFILE"
            rm -f "$NNN_TMPFILE" > /dev/null
    fi
}

alias nnn='n'
