if !has('nvim')
	if &term =~ "^tmux"
		let &t_SI .= "\<Esc>Ptmux;\<Esc>\<Esc>[5 q\<Esc>\\"
		let &t_SR .= "\<Esc>Ptmux;\<Esc>\<Esc>[3 q\<Esc>\\"
		let &t_EI .= "\<Esc>Ptmux;\<Esc>\<Esc>[1 q\<Esc>\\"
	else
		let &t_SI .= "\<Esc>[5 q"
		let &t_SR .= "\<Esc>[3 q"
		let &t_EI .= "\<Esc>[1 q"
	endif
endif

if !has('nvim')
	if has('unix')
		if !isdirectory($XDG_CACHE_HOME . "/vim")
    			call mkdir($XDG_CACHE_HOME . "/vim", "p", 0700)
		endif
		set viminfofile=$XDG_CACHE_HOME/vim/viminfo

		if !isdirectory($XDG_DATA_HOME . "/vim/swap")
    			call mkdir($XDG_DATA_HOME . "/vim/swap", "p", 0700)
		endif
		set directory=$XDG_DATA_HOME/vim/swap//

		if !isdirectory($XDG_DATA_HOME . "/vim/backup")
    			call mkdir($XDG_DATA_HOME . "/vim/backup", "p", 0700)
		endif
		set backupdir=$XDG_DATA_HOME/vim/backup//

		if !isdirectory($XDG_DATA_HOME . "/vim/undo")
    			call mkdir($XDG_DATA_HOME . "/vim/undo", "p", 0700)
		endif
		set undodir=$XDG_DATA_HOME/vim/undo

		set runtimepath=$XDG_CONFIG_HOME/vim,$VIM/vimfiles,$VIMRUNTIME,$VIM/vimfiles/after,$XDG_CONFIG_HOME/vim/after
	endif
endif

if has('nvim') && has('unix')
	if !isdirectory($XDG_DATA_HOME . "nvim/backup")
    		call mkdir($XDG_DATA_HOME . "/nvim/backup", "p", 0700)
	endif
	set backupdir-=.
endif

set undofile

set autoindent
set autoread
set backspace=indent,eol,start
set belloff=all
set complete+=i
set cscopeverbose
set display+=lastline
if !has('nvim')
	set fillchars="vert:│,fold:·,sep:│"
endif
set formatoptions+=j
set nofsync
set history=10000
set hlsearch
set incsearch
set nolangremap
set laststatus=2
set listchars=eol:¬,extends:»,tab:▸\ ,trail:·,nbsp:°
set mouse=a
set nrformats+=bin
set nrformats+=hex
set nrformats+=alpha
set nrformats+=octal
set ruler
set scrolloff=1
set sessionoptions-=options
set shortmess=atToOFS
set showcmd
set sidescroll=1
set sidescrolloff=5
set smarttab
set nostartofline
set tabpagemax=50
set tags-=./tags
set tags^=./tags;
set tags-=TAGS
set tags-=./TAGS
set ttyfast
set viewoptions-=options
set viminfo^=!
set wildmenu

if has('autocmd')
	filetype plugin indent on
endif
if has('syntax') && !exists('g:syntax_on')
	syntax enable
endif

" Use <C-L> to clear the highlighting of :set hlsearch.
if maparg('<C-L>', 'n') ==# ''
	nnoremap <silent> <C-L> :nohlsearch<C-R>=has('diff')?'<Bar>diffupdate':''<CR><CR><C-L>
endif
" TODO do they both do the same?
" Make delete key in Normal mode remove the persistently highlighted matches
nmap <silent>  <BS>  :nohlsearch<CR>

" Allow color schemes to do bright colors without forcing bold.
if &t_Co == 8 && $TERM !~# '^Eterm'
  	set t_Co=16
endif

inoremap <C-U> <C-G>u<C-U>
inoremap <C-W> <C-G>u<C-W>

imap <F6> <c-o>:set list!<CR>
map <F6> :set list!<CR>

cmap w!! w !sudo tee > /dev/null %

" Make :help appear in a full-screen tab, instead of a window
augroup HelpInTabs
	autocmd!
	autocmd BufEnter  *.txt,*.txt.gz   call HelpInNewTab()
augroup END
function! HelpInNewTab ()
	if &buftype == 'help'
		"Convert the help window to a tab...
		execute "normal \<C-W>T"
	endif
endfunction

" Delete trailing whitespace on save
func! DeleteTrailingWS()
	exe "normal mz"
	%s/\s\+$//ge
	exe "normal `z"
endfunc
autocmd BufWrite *.py :call DeleteTrailingWS()
autocmd BufWrite *.coffee :call DeleteTrailingWS()
autocmd BufWrite *.hs :call DeleteTrailingWS()

" Automatically have Regexes in 'Very Magical Mode'
nnoremap / /\v

" Clang-Format & gofmt
if has("win32") || has("win64")
	map <leader>i :%pyfile C:\\Program Files\ (x86)\\LLVM\\share\\clang\\clang-format." py<cr>
	" imap <C-I> <c-o>:%pyfile C:\\Program Files\ (x86)" \\LLVM\\share\\clang\\clang-format.py<cr>
else
	map <leader>i :%pyfile /usr/share/clang/clang-format.py<cr>
	" imap <C-I> <c-o>:%pyfile /usr/share/clang/clang-format.py<cr>
endif
autocmd FileType go map <C-I> :GoFmt<cr>
autocmd FileType go imap <C-I> <C-O>:GoFmt<cr>

" Returns true if paste mode is enabled
function! HasPaste()
	if &paste
		return 'PASTE MODE  '
	endif
	return ''
endfunction

set cindent
set number
set numberwidth=1
set pastetoggle=<F8>
set relativenumber
set title
set wrap

" Comma as leader (default is \)
" let mapleader = ","
" let g:mapleader = ","
" "set tabline=%M\ %t
" set cinoptions=(0,u0,g0,:0,j1,J1,)200
" set clipboard=unnamed
" " set colorcolumn=+1
" set copyindent
" set fixendofline
" set foldclose=all
" set foldcolumn=1
" set foldopen=all
" set guitablabel=%M\ %t
" set ignorecase
" set linebreak
" set matchpairs+=«:»
" set matchtime=2
" set noerrorbells
" set noexpandtab
" set nohidden
" set pyxversion=3
" set shiftwidth=4
" set showmatch
" set showtabline=2
" set smartcase
" set softtabstop=0
" set statusline=%{HasPaste()}%F%M%R%H%W%Y\ [%{&ff}]\ \ %<CWD:\ %r%{getcwd()}%h\ %=Line:\ " %l\ \ \ Column:\ %v\ \ \ %p%%
" set tabstop=4
" set textwidth=0
" set timeoutlen=1000
" set visualbell t_vb=
" set whichwrap+=<,>,[,],h,l
" " http://stackoverflow.com/questions/526858/" how-do-i-make-vim-do-normal-bash-like-tab-completion-for-file-names
" set wildmode=longest,list,full
" set writebackup
"
" " Useful mappings for managing tabs
" map <leader>tn :tabnew<cr>
" map <leader>to :tabonly<cr>
" map <leader>tc :tabclose<cr>
" map <leader>tm :tabmove
" "map <M-PageDown> :tabnext<cr>
" "map <M-PageUp> :tabprevious<cr>
"
" " Treat long lines as break lines (useful when moving around in them)
" map j gj
" map k gk
"
" map Q gQ
"
" " Map jj to the Esc functionality
" inoremap jj <Esc>
"
" " Show hightlighting groups for current word
" nmap <C-S-P> :call <SID>SynStack()<CR>
" function! <SID>SynStack()
" 	if !exists("*synstack")
" 		return
" 	endif
" 	echo map(synstack(line('.'), col('.')), 'synIDattr(v:val, "name")')
" endfunc
"
" " Return to last edit position when opening files
" autocmd BufReadPost *
" 			\ if line("'\"") > 0 && line("'\"") <= line("$") |
" 			\   exe "normal! g`\"" |
" 			\ endif
"
"
" " Update in insert mode
" nmap <F2> :update<CR>
" vmap <F2> <Esc><F2>gv
" imap <F2> <c-o><F2>
"
