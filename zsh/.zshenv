typeset -U path
path=(/usr/local/bin /usr/bin $npm_config_prefix/bin $GOPATH/bin)

if [[ -x /usr/bin/ccache ]] ; then
	path=(/usr/lib64/ccache/bin $path)
fi

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

# export AR=gcc-ar
# export RANLIB=gcc-ranlib
# export NM=gcc-nm
# export LA_VERSION="OpenBLAS"
# export LA_LIBS=/usr/lib64/libopenblas.so.0
# export LA_INCLUDE=/usr/include
# export LA_PATH=/usr/lib64/
# export MPI_CC=/usr/bin/mpicc
# export MPI_LIBS=/usr/lib64/libmpi.so
# export MPI_INCLUDE=/usr/include/
# export MPI_PATH=/usr/lib64/
# export MPI_VERSION=3.2
# export THEANO_FLAGS='floatX=float32,openmp=true,gcc.cxxflags="-ftree-vectorize -mavx"'
# export CC=gcc
# export CXX=g++
export PYTHONIOENCODING=utf-8:surrogateescape
export MESA_GLSL_CACHE_DISABLE=0
export PKG_CONFIG_PATH='/usr/local/lib/pkgconfig'

export CFLAGS CXXFLAGS FCFLAGS FFLAGS LDFLAGS MAKEFLAGS

umask 077

# export KISS_PATH=/home/xha/src/github.com/xaverh/repo:/home/xha/src/github.com/kisslinux/repo/core:/home/xha/src/github.com/kisslinux/repo/extra:/home/xha/src/github.com/kisslinux/repo/xorg:/home/xha/src/github.com/kisslinux/community/community
# export KISS_COMPRESS=zst
