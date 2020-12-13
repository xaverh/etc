export XDG_CONFIG_HOME="$HOME/.config"
export XDG_CACHE_HOME="$HOME/.cache"
export XDG_DATA_HOME="$HOME/.local/share"
export GOPATH="$XDG_DATA_HOME/go"
export NPMPATH="$XDG_DATA_HOME/npm"
export npm_config_userconfig="$XDG_CONFIG_HOME/npmrc"
export npm_config_prefix="$NPMPATH"
export npm_config_cache="$XDG_CACHE_HOME/npm"
export NODE_REPL_HISTORY="$XDG_CACHE_HOME/node_repl_history"
export FIREFOX_INSTALL_DIR="$XDG_DATA_HOME/firefox"
export TERMINFO_DIRS=/usr/local/share/terminfo:
local ARCHITECTURE=haswell

typeset -U path

path=(/usr/local/bin /usr/bin/haswell /usr/bin /opt/3rd-party/bin ~/src/github.com/xaverh/kiss ~/src/github.com/xaverh/kiss/contrib $npm_config_prefix/bin $GOPATH/bin)

#if grep -q "flags.*:.* avx512bw" /proc/cpuinfo; then
#        case ":${PATH:-}:" in
#               *:/usr/bin/haswell/avx512_1:*) ;;
#               *) PATH="/usr/bin/haswell/avx512_1:/usr/bin/haswell:$PATH" ;;
#        esac
#elif grep -q "flags.*:.* fma .* avx2" /proc/cpuinfo; then
#        case ":${PATH:-}:" in
#               *:/usr/bin/haswell:*) ;;
#               *) PATH="/usr/bin/haswell:$PATH" ;;
#        esac
#fi

if [[ -x /usr/bin/ccache ]] ; then
	path=(/usr/lib64/ccache/bin $path)
fi

test -z "$TERM" && TERM="xterm" # Basic terminal capab. For screen etc.

CFLAGS="-g -O3 -feliminate-unused-debug-types -pipe -Wall -Wp,-D_FORTIFY_SOURCE=2 -fexceptions -fstack-protector --param=ssp-buffer-size=32 -Wformat -Wformat-security -m64 -fasynchronous-unwind-tables -Wp,-D_REENTRANT -ftree-loop-distribute-patterns -Wl,-z -Wl,now -Wl,-z -Wl,relro -fno-semantic-interposition -ffat-lto-objects -fno-trapping-math -Wl,-sort-common -Wl,--enable-new-dtags -mtune=skylake -Wa,-mbranches-within-32B-boundaries"
FFLAGS="-g -O3 -feliminate-unused-debug-types -pipe -Wall -Wp,-D_FORTIFY_SOURCE=2 -fexceptions -fstack-protector --param=ssp-buffer-size=32 -m64 -fasynchronous-unwind-tables -Wp,-D_REENTRANT -ftree-loop-distribute-patterns -Wl,-z -Wl,now -Wl,-z -Wl,relro -malign-data=abi -fno-semantic-interposition -ftree-vectorize -ftree-loop-vectorize -Wl,--enable-new-dtags -Wa,-mbranches-within-32B-boundaries "
FCFLAGS="-g -O3 -feliminate-unused-debug-types -pipe -Wall -Wp,-D_FORTIFY_SOURCE=2 -fexceptions -fstack-protector --param=ssp-buffer-size=32 -m64 -fasynchronous-unwind-tables -Wp,-D_REENTRANT -ftree-loop-distribute-patterns -Wl,-z -Wl,now -Wl,-z -Wl,relro -malign-data=abi -fno-semantic-interposition -ftree-vectorize -ftree-loop-vectorize -Wl,-sort-common -Wl,--enable-new-dtags "

MAKEFLAGS='-j4'

CFLAGS="$CFLAGS -march=$ARCHITECTURE -mtune=$ARCHITECTURE -fno-math-errno -falign-functions=32 -fno-trapping-math -malign-data=abi -ftree-vectorize -ftree-loop-vectorize "
CFLAGS="$CFLAGS -flto=auto -fgraphite-identity -floop-nest-optimize -funroll-loops -ftracer -mfpmath=both -fipa-pta -fdevirtualize-at-ltrans -fno-strict-aliasing "
# XBPS_CFLAGS="-flto=auto -fgraphite-identity -floop-nest-optimize -funroll-loops -ftracer -mfpmath=both -fipa-pta -fdevirtualize-at-ltrans -ffat-lto-objects -flto-partition=none  -fno-strict-aliasing"
FFLAGS="$FFLAGS -march=$ARCHITECTURE -mtune=$ARCHITECTURE "
FCFLAGS="$FCFLAGS -march=$ARCHITECTURE -mtune=$ARCHITECTURE "

export RUSTFLAGS="-C target-cpu=$ARCHITECTURE -C opt-level=3 -g -Clink-args=-Wl,-z,relro,-z,now"

CXXFLAGS="$CFLAGS -fvisibility-inlines-hidden -Wl,--enable-new-dtags "

LDFLAGS="${LDFLAGS} ${CFLAGS} -Wl,--as-needed -Wl,-O1 -L/usr/local/lib "

export AR=gcc-ar
export RANLIB=gcc-ranlib
export NM=gcc-nm
export LA_VERSION="OpenBLAS"
export LA_LIBS=/usr/lib64/libopenblas.so.0
export LA_INCLUDE=/usr/include
export LA_PATH=/usr/lib64/
export MPI_CC=/usr/bin/mpicc
export MPI_LIBS=/usr/lib64/libmpi.so
export MPI_INCLUDE=/usr/include/
export MPI_PATH=/usr/lib64/
export MPI_VERSION=3.2
export THEANO_FLAGS='floatX=float32,openmp=true,gcc.cxxflags="-ftree-vectorize -mavx"'
export CC=gcc
export CXX=g++
export PYTHONIOENCODING=utf-8:surrogateescape
export MESA_GLSL_CACHE_DISABLE=0
export GTK_IM_MODULE="ibus"
export PKG_CONFIG_PATH='/usr/local/lib/pkgconfig'

XDG_CONFIG_DIRS=/usr/share/xdg:/etc/xdg

export CCACHE_DIR=$XDG_CACHE_HOME/ccache

export GNUPGHOME=$XDG_DATA_HOME/gnupg

for langfile in /usr/share/defaults/etc/locale.conf /etc/locale.conf "$HOME/.i18n" ; do
	[ -f $langfile ] && . $langfile 
done
export LANG LANGUAGE LC_CTYPE LC_NUMERIC LC_TIME LC_COLLATE LC_MONETARY LC_MESSAGES LC_PAPER LC_NAME LC_ADDRESS LC_TELEPHONE LC_MEASUREMENT LC_IDENTIFICATION

export TERM CFLAGS CXXFLAGS FCFLAGS FFLAGS XDG_CONFIG_DIRS LDFLAGS

umask 022

