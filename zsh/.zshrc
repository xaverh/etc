unsetopt allexport autoresume bashautolist bsdecho correctall cshjunkiehistory cshjunkiequotes cshnullcmd cshnullglob dvorak kshautoload listbeep promptbang  pushdsilent pushdtohome recexact
setopt aliases alwayslastprompt autolist autoparamkeys autoparamslash autoremoveslash badpattern banghist bareglobqual bgnice caseglob casematch equals functionargzero globalrcs hashlistall listambiguous listtypes promptcr promptpercent promptsp rcs transientrprompt autocd beep completeinword correct rmstarwait braceccl autopushd pushdminus pushdignoredups nomatch noglobdots extendedglob noclobber histallowclobber multios checkjobs nohup autocontinue longlistjobs notify sharehistory appendhistory extendedhistory histnostore histignorealldups histignorespace globcomplete automenu menucomplete completealiases alwaystoend listpacked listrowsfirst autonamedirs cbases cdablevars chasedots chaselinks flowcontrol promptsubst

HISTFILE=~/.zsh_history
HISTSIZE=9223372036854775807
SAVEHIST=9223372036854775807
eval `dircolors $HOME/.dircolors 2> /dev/null` || eval `dircolors`
zmodload -i zsh/complist
autoload -Uz compinit && compinit
export EDITOR=vim
export PAGER=less
autoload -U colors && colors
autoload -U zmv
autoload -U keeper && keeper
bindkey '^Xk' insert-kept-result
bindkey '^Xe' expand-word
bindkey -M menuselect 'i' accept-and-menu-complete
autoload -U tetris
zle -N tetris
bindkey "^Xt" tetris
autoload -U history-search-end
zle -N history-beginning-search-backward-end history-search-end
zle -N history-beginning-search-forward-end history-search-end
bindkey "\e[A"  history-beginning-search-backward-end
bindkey "\e[B"  history-beginning-search-forward-end

PROMPT='%n@%m:%B%~ %#%b '

bindkey -e

_bkdate() { BUFFER="$BUFFER$(date '+%F')"; CURSOR=$#BUFFER; }
zle -N _bkdate
bindkey '^Ed' _bkdate

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
alias aus="su -c 'shutdown -h now'"
alias lsbig='ls -Slh | head'
alias lssmall='ls -Slhr | head'
alias lsnew='ls -tlh | head'
alias lsold='ls -tlh | tail'
alias dfh='df -H'
alias wget='wget -U="Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/52.0.2743.116 Safari/537.36"'
alias 7zultra='7z a -t7z -mx=9 -mfb=64 -md=32m -ms=on'
alias -s txt=vim
alias -s de=$BROWSER
alias -s com=$BROWSER
alias -s org=$BROWSER
alias -s odt=lowriter
alias -s doc=lowriter
alias -s docx=lowriter
alias -g G='|& grep -i --colour=auto'
alias -g P='| $PAGER'
alias -g T='| tail'
alias -g H='| head'
alias d='dirs -v'
alias j='jobs -l'
alias dropbox_reset="echo fs.inotify.max_user_watches=100000 | sudo tee -a /etc/sysctl.conf; sudo sysctl -p"
alias Nohidden="dconf reset /org/gtk/settings/file-chooser/show-hidden &> /dev/null || (defaults write com.apple.finder AppleShowAllFiles NO && killall Finder)"
alias Showhidden="defaults write com.apple.finder AppleShowAllFiles YES &&killall Finder"
alias Fix_dotnet="find /opt/dotnet -name '*.so' -type f -print | xargs ldd | grep 'not found'"
alias -g IX="| curl -F 'f:1=<-' ix.io"
alias Aur='repose -vf aur -r /var/cache/pacman/aur -p /var/cache/pacman/aur'
alias gist='gist -c'

Fix_steam() {
	find ~/.steam/root/ \( -name "libgcc_s.so*" -o -name "libstdc++.so*" -o -name "libxcb.so*" \) -print -delete
	find ~/.steam/root/ -name "libgpg-error.so*" -print -delete
}

Why_no_steam() {
	cd ~/.local/share/Steam/ubuntu12_32
	LD_LIBRARY_PATH=".:${LD_LIBRARY_PATH}" ldd $(file *|sed '/ELF/!d;s/:.*//g') | grep 'not found' | sort | uniq
	cd ~1
}

global-alias-space(){
	local ga="$LBUFFER[(w)-1]"
	[[ -n $ga ]] && LBUFFER[(w)-1]="${${galiases[$ga]}:-$ga}"
	zle self-insert
}
zle -N global-alias-space
bindkey ' ' global-alias-space

chpwd() {
	[[ $TERM == (xterm*|*rxvt*|st*) ]] && print -Pn "\e]0;zsh%1(j|(%j)|): %~ ($TERM)\a"
	[[ $TERM == (tmux*|screen*) ]] && printf "\033kzsh\033\\"
}

preexec() {
	[[ $TERM == (xterm*|*rxvt*|st*) ]] && print -Pn "\e]0;zsh%1(j|(%j)|): $2 ($TERM)\a"
	[[ $TERM == (tmux*|screen*) ]] && printf "\033k$1\033\\"
}

# A case for a suffix alias?
sx simple-extract () {
	for f in "$@"; do
		if [[ -f $f ]]; then
			case $f in
				*.7z)		7z x $f						;;
				*.7z.001)	7z x $f						;;
				*.lzma)		unlzma $f					;;
				*.tar.bz2)	tar -xvjf $f				;;
				*.tar.gz)	tar -xvzf $f				;;
				*.rar)		unrar x $f					;;
				*.deb)		ar -x $f					;;
				*.bz2)		bzip2 -d $f					;;
				*.lzh)		lha x $f					;;
				*.gz)		gunzip -d --verbose $f  	;;
				*.tar)		tar -xvf $f					;;
				*.tgz)		tar -xvzf $f				;;
				*.tbz2)		tar -xvjf $f				;;
				*.txz)		tar -xvJf $f				;;
				*.tar.xz)	tar -xvJf $f				;;
				*.tar.lz)   tar --lzip -xvf $f          ;;
				*.xz)		7z x $f						;;
				*.zip)		unzip $f					;;
				*.Z)		uncompress $f				;;
				*)			echo "'$f' Error. This is no compression type \
			      				known to simple-extract." ;;
			esac
		else
			echo "'$f' is not a valid file"
		fi
	done
}

# braucht mencoder und mplayer
Join_avi () {
	# TODO checken ob mencoder da ist!
	cat $* > movie_tmp.avi; # TODO Frage nach finalem Namen
	mencoder -forceidx -oac copy -ovc copy movie_tmp.avi -o ./movie_final.avi
}

for c in shred rm wipe rmdir; do
	eval "alias $c=' $c'"
done

zstyle ':completion:*' format 'Completing %d'
zstyle ':completion:*' verbose yes
zstyle ':completion:*' group-name ''
zstyle ':completion:*:kill:*' command \
	'ps xf -u $USER -o pid,%cpu,cmd'
zstyle ':completion:*:*:kill:*:processes' \
	list-colors '=(#b) #([0-9]#)*=0=01;31'
zstyle ':completion:*:descriptions' format '%S%d%s'
zstyle ':completion:*:corrections' format $'%S%d (errors: %e)%s'
zstyle ':completion:*:messages' format '%S%d%s'
zstyle ':completion:*:warnings' format '%S%d%s'
#zstyle ':completion:(^rm):(all-|)files' ignored-patterns "(*.o|*.aux|*.toc|*.swp|*~)"
#zstyle ':completion:rm:(all-|)files' ignored-patterns '' # verhindert, daß die oben zu ignorierenden Dateien auch bei rm ignoriert werden
zstyle ':completion:*' menu select=2
zstyle ':completion:*:default' select-prompt '%S Treffer %M	%P%s'
zstyle ':completion:*:default' list-colors ${(s.:.)LS_COLORS}
zstyle ':completion:*' completer _complete _match _ignored _correct _approximate _prefix _history
zstyle ':completion:*:approximate:*' max-errors 'reply=( $(( ($#PREFIX+$#SUFFIX)/3 )) numeric )'
zstyle ':completion::complete:*' rehash true
zstyle ':completion::*:ssh:*:*' tag-order 'users hosts'
zstyle ':completion::*:-command-:*:*' tag-order '! aliases functions'
zstyle ':completion:*:*:rm:*' file-patterns \
	'*.o:object-files:object\ files
*(~|.(old|bak|BAK)):backup-files:backup\ files
*~*(~|.(o|old|bak|BAK)):all-files:all\ files'
zstyle ':completion:*' matcher-list 'm:{a-zA-Z}={A-Za-z}'
zstyle ':completion:*:rm:*' ignore-line yes
zstyle ':completion:*:pacman:*' force-list always
zstyle ':completion:*:*:pacman:*' menu yes select
zstyle ':completion:*:*:killall:*' menu yes select
zstyle ':completion:*:killall:*'   force-list always
# complete manual by their section
zstyle ':completion:*:manuals'    separate-sections true
zstyle ':completion:*:manuals.*'  insert-sections   false
zstyle ':completion:*:man:*'      menu yes select




# manpage colors
# export LESS_TERMCAP_mb=$'\E[00;34m'     # begin blinking
export LESS_TERMCAP_mb=$'\E[00;31m'     # begin blinking
# export LESS_TERMCAP_md=$'\E[00;94m'     # begin bold
export LESS_TERMCAP_md=$'\E[00;33m'     # begin bold
export LESS_TERMCAP_us=$'\E[00;34m'     # begin underline
export LESS_TERMCAP_me=$'\E[0m'         # end mode
export LESS_TERMCAP_se=$'\E[0m'         # end standout-mode
export LESS_TERMCAP_so=$'\E[00;40;39m' # begin standout-mode - info box
#export LESS_TERMCAP_so=$'\E[00;102;37m' # begin standout-mode - info box
export LESS_TERMCAP_ue=$'\E[0m'         # end underline
# export LESS_TERMCAP_us=$'\E[00;31m'     # begin underline
export GROFF_NO_SGR=1

# Erstellt aus .flac-Dateien .m4a-Dateien:
transcode.m4a() {
while [[ $# -gt 0 ]]; do
    if [[ -f $1 ]]
    then
        flac --decode --stdout $1 | faac -q 192 -o $1.m4a -;
        shift;
    else
        # TODO error handling
        exit
    fi
done
}

Hitparade() {
    fc -ln 1|awk '{print $1}' |sort|uniq -c | sort -rn |head -n 10
}

if [[ -x /usr/lib/command-not-found || -x /usr/share/command-not-found/command-not-found ]]; then
    function command_not_found_handler {
    if [[ -x /usr/lib/command-not-found ]]; then
        /usr/bin/python /usr/lib/command-not-found -- "$1"
        return $?
    elif [[ -x /usr/share/command-not-found/command-not-found ]]; then
        /usr/bin/python /usr/share/command-not-found/command-not-found -- "$1"
        return $?
    else
        printf "%s: command not found\n" "$1" >&2
        return 127
    fi
}
fi

if [[ -e /usr/share/zsh/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh ]]; then
	source /usr/share/zsh/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
fi

# autoload -U promptinit
# promptinit

# PURE_PROMPT_SYMBOL=%%

# prompt grb

# required for st
# http://zsh.sourceforge.net/FAQ/zshfaq03.html#l25
function zle-line-init () { echoti smkx }
function zle-line-finish () { echoti rmkx }
zle -N zle-line-init
zle -N zle-line-finish

