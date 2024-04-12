vim9script

colorscheme quiet

&compatible = 0

# There must a be a better place for this on Windows, cf. Neovim
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

set nrformats-=octal
set nrformats+=alpha

if has('syntax') && !exists('g:syntax_on')
    syntax enable
endif

set pastetoggle=<F8>
set textwidth=80
set colorcolumn=+1
set hlsearch
set incsearch

# nnoremap <silent> <C-l> :<C-u>nohlsearch<CR><C-l>
# wozu brauchen wir das hintere <C-l> ? wozu das <C-u> ?
nnoremap <C-l> :<C-u>nohlsearch<CR><C-l>
# nnoremap <F3> :set hlsearch! hlsearch?<CR>
nnoremap <F5> :echo getcwd()<CR>
