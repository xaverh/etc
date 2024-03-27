vim9script


&compatible = 0

&backupdir = expand('~/.vim/backup//')
&directory = expand('~/.vim/swap//')
&undodir = expand('~/.vim/undo//')
&undofile = 1

if !isdirectory(&undodir)
	call mkdir(&undodir, 'p', 0o700)
endif

if !isdirectory(&backupdir)
	call mkdir(&backupdir, 'p', 0o700)
endif

if !isdirectory(&directory)
	call mkdir(&directory, 'p', 0o700)
endif

# let &t_SI .= "\<Esc>[5 q"
# let &t_SR .= "\<Esc>[3 q"
# let &t_EI .= "\<Esc>[1 q"

&autoindent = 1
&autoread = 1
&backspace = 'indent,eol,start'
&backup = 1
&display = 'lastline'
&encoding = 'utf-8'
# fillchars
set formatoptions+=j
&joinspaces = 0
&laststatus = 2
&number = 1
&ruler = 1

if has('syntax') && !exists('g:syntax_on')
    syntax enable
endif


