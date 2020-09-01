if [ -e /home/xha/.nix-profile/etc/profile.d/nix.sh ]; then . /home/xha/.nix-profile/etc/profile.d/nix.sh; fi
export KISS_PATH=$HOME/etc/repos/kiss/hotter_than_hell:$HOME/etc/repos/kiss/repo/core:$HOME/etc/repos/kiss/repo/extra:$HOME/etc/repos/kiss/repo/xorg:$HOME/etc/repos/kiss/community/community
export KISS_HOOK=$HOME/.config/kh
export CFLAGS='-march=native -pipe -O3 -fno-math-errno -fno-strict-aliasing -flto'
export CXXFLAGS="$CFLAGS"
export MAKEFLAGS='-j5'
export CCACHE_DIR=$HOME/.cache/ccache
export PF_INFO='ascii title os host shell editor wm de kernel uptime pkgs memory palette'
export GOPATH=$HOME/.local/share/go
export npm_config_prefix=$HOME/.local/share/npm
export npm_config_userconfig=$HOME/etc/npmrc
export npm_config_cache=$HOME/.cache/npm
export NODE_REPL_HISTORY=$HOME/.cache/node_repl_history
export NNN_COLORS=4256
export NNN_OPTS=xe
export NNN_FCOLORS=0b0304010f0e060740020a08
export NNN_PLUG='i:imgview;c:-_code -r $nnn*;x:sx;h:-hexview;v:-_mpv --force-window=yes $nnn*;V:-_mpv --shuffle --force-window=yes $nnn*;u:-uidgid;G:getplugs'
export LESS_TERMCAP_mb="[00;34m"
export LESS_TERMCAP_md="[01;32m"
export LESS_TERMCAP_us="[01;35m"
export LESS_TERMCAP_ue="[0m"
export LESS_TERMCAP_me="[0m"
export GROFF_NO_SGR=1
export LS_COLORS='rs=0:di=1;34:tw=1;3;94:ow=1;94:st=1;3;34:ex=1;31:sg=1;3;31:su=1;3;91:ca=1;4;31:ln=36:mh=96:or=38;5;64:mi=37:bd=93:cd=33:pi=32:so=92:do=4;92:*.js=38;2;23;23;23;48;2;221;224;90:*.jsx=38;2;23;23;23;48;2;221;224;90:*.ts=48;2;43;116;137;38;2;229;230;230:*.tsx=48;2;43;116;137;38;2;229;230;230:*.vue=38;2;44;62;80;48;2;65;184;131:*.cpp=48;2;243;75;125:*.cxx=48;2;243;75;125:*.cc=48;2;243;75;125:*.hpp=48;2;243;75;125:*.hxx=48;2;243;75;125:*.hh=48;2;243;75;125:*.c=7:*.h=7:*.go=38;2;229;230;230;48;2;0;173;216:*.hs=38;2;94;80;134;48;2;235;228;243:*.svelte=48;2;229;230;230;38;2;255;62;0:*.lua=48;2;0;0;128;38;2;229;230;230:*.html=38;2;229;230;230;48;2;227;76;38:*.htm=38;2;229;230;230;48;2;227;76;38:*.xhtml=38;2;229;230;230;48;2;227;76;38:*.css=38;2;229;230;230;48;2;86;61;124:*.scss=38;2;229;230;230;48;2;207;100;154:*.sass=38;2;229;230;230;48;2;207;100;154:*.nix=48;2;126;126;255:*.vim=48;2;25;159;75;38;2;204;204;153:*vimrc=48;2;25;159;75;38;2;204;204;153:*Makefile.in=37:*CMakeCache.txt=37:*.la=37:*.o=37:*.lo=37:*.dyn_hi=37:*.cache=37:*.dyn_o=37:*.hi=37:*.errors=37:*.class=37:*.aux=37:*.bbl=37:*.ilg=37:*.idx=37:*.blg=37:*.out=37:*.toc=37:*.ind=37:*.sty=37:*.synctex.gz=37:*.fdb_latexmk=37:*.fls=37:*.bcf=37:*.bc=37:*.pyc=37:*.rlib=37:*.sconsign.dblite=37:*.scons_opt=37:*.git=37:*package-lock.json=37:*.pid=90:*.swp=90:*.tmp=90:*.bak=90:*.orig=90:*.lock=90:*.log=90:*~=90:*COPYRIGHT=90:*LICENSE=90:*LICENSE-MIT=90:*COPYING=90:*LICENSE-APACHE=90:'
export EDITOR=vim
