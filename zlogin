typeset -U path
path=(/usr/local/bin /usr/bin $path $npm_config_prefix/bin $GOPATH/bin)
if [[ -x /usr/bin/ccache ]] ; then
	path=(/usr/lib64/ccache/bin $path)
fi

