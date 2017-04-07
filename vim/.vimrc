set nocompatible                   " required for vundle
filetype off                       " required for vundle
set rtp+=~/.vim/bundle/Vundle.vim  " required for vundle

call vundle#begin()

Plugin 'VundleVim/Vundle.vim'
Plugin 'mhinz/vim-randomtag'

call vundle#end()

filetype plugin indent on
syntax enable

" Comma as leader
let mapleader = ","
let g:mapleader = ","

if has("win32") || has("win64")
 	let &undodir=$HOME . '\AppData\Local\VIM_UNDO_FILES\'
else
 	let &undodir='/tmp/'
endif

set t_ts=k
set t_fs=\
" let &t_8f = "\<Esc>[38;2;%lu;%lu;%lum"
" let &t_8b = "\<Esc>[48;2;%lu;%lu;%lum"
" let &t_ZH = "\e[3m"
" let &t_ZR = "\e[23m"

"set cpoptions=q
"set tabline=%M\ %t
set autoindent
set autoread
set backspace=indent,eol,start
set backup
set backupdir=~/tmp,/tmp
set backupskip=/tmp/*,~/tmp/*
set cindent
set cinoptions=(0,u0,g0,:0,j1,J1,)200
set clipboard=unnamed
" set colorcolumn=+1
set copyindent
"set cursorline
set directory=~/tmp,/tmp
set display=truncate
set encoding=utf8
set fixendofline
set foldclose=all
set foldcolumn=1
set foldopen=all
set guioptions+=T
set guioptions+=m
set guioptions-=e
set guioptions-=r
set guioptions-=t
set guitablabel=%M\ %t
set history=1000
set hlsearch
set ignorecase
set incsearch
set laststatus=2
set lazyredraw
set linebreak
set listchars=eol:¬,extends:»,tab:▸\ ,trail:·
set matchpairs+=«:»
set matchtime=2
set noerrorbells
set noexpandtab
set nohidden
set nostartofline
set number
set numberwidth=1
set pastetoggle=<F8>
set noruler
set scrolloff=5
set shiftwidth=4
set shortmess=at
set showcmd
set showmatch
set showtabline=2
set smartcase
set smarttab
set softtabstop=0
set statusline=%{HasPaste()}%F%M%R%H%W%Y\ [%{&ff}]\ \ %<CWD:\ %r%{getcwd()}%h\ %=Line:\ %l\ \ \ Column:\ %v\ \ \ %p%%
set tabstop=4
set textwidth=80
set timeoutlen=1000
set title
" set ttymouse=urxvt
set undofile
set undolevels=5000
set visualbell t_vb=
set whichwrap+=<,>,[,],h,l
" http://stackoverflow.com/questions/526858/how-do-i-make-vim-do-normal-bash-like-tab-completion-for-file-names
set wildmode=longest,list,full
set wildmenu
set wrap
set writebackup

" save session
nnoremap <leader>s :mksession<CR>

" toggle between number and relativenumber
function! ToggleNumber()
    if(&relativenumber == 1)
        set norelativenumber
        set number
    else
        set relativenumber
    endif
endfunc

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

" Returns true if paste mode is enabled
function! HasPaste()
	if &paste
		return 'PASTE MODE  '
	endif
	return ''
endfunction

" Automatically have Regexes in 'Very Magical Mode'
nnoremap / /\v

" Make delete key in Normal mode remove the persistently highlighted matches
nmap <silent>  <BS>  :nohlsearch<CR>

" Mappings for dragvisuals
vmap  <expr>  <S-LEFT>   DVB_Drag('left')
vmap  <expr>  <S-RIGHT>  DVB_Drag('right')
vmap  <expr>  <S-DOWN>   DVB_Drag('down')
vmap  <expr>  <S-UP>     DVB_Drag('up')
vmap  <expr>  D          DVB_Duplicate()

" Colors & Fonts & Appearance
set background=dark
let c_comment_strings=1

" Clang-Format & gofmt
if has("win32") || has("win64")
	map <leader>i :%pyfile C:\\Program Files\ (x86)\\LLVM\\share\\clang\\clang-format.py<cr>
	" imap <C-I> <c-o>:%pyfile C:\\Program Files\ (x86)\\LLVM\\share\\clang\\clang-format.py<cr>
else
	map <leader>i :%pyfile /usr/share/clang/clang-format.py<cr>
	" imap <C-I> <c-o>:%pyfile /usr/share/clang/clang-format.py<cr>
endif

autocmd FileType go map <C-I> :GoFmt<cr>
autocmd FileType go imap <C-I> <C-O>:GoFmt<cr>

" Useful mappings for managing tabs
map <leader>tn :tabnew<cr>
map <leader>to :tabonly<cr>
map <leader>tc :tabclose<cr>
map <leader>tm :tabmove
"map <M-PageDown> :tabnext<cr>
"map <M-PageUp> :tabprevious<cr>

" Treat long lines as break lines (useful when moving around in them)
map j gj
map k gk

" Map öö to the Esc functionality
inoremap öö <Esc>

" Always show filetypes in menu
if has('gui_running')
	let do_syntax_sel_menu = 1|runtime~! synmenu.vim|aunmenu &Syntax.&Show\ filetypes\ in\ menu
endif

" Show hightlighting groups for current word
nmap <C-S-P> :call <SID>SynStack()<CR>
function! <SID>SynStack()
	if !exists("*synstack")
		return
	endif
	echo map(synstack(line('.'), col('.')), 'synIDattr(v:val, "name")')
endfunc

" Return to last edit position when opening files
autocmd BufReadPost *
			\ if line("'\"") > 0 && line("'\"") <= line("$") |
			\   exe "normal! g`\"" |
			\ endif

" Delete trailing whitespace on save
func! DeleteTrailingWS()
	exe "normal mz"
	%s/\s\+$//ge
	exe "normal `z"
endfunc
autocmd BufWrite *.py :call DeleteTrailingWS()
autocmd BufWrite *.coffee :call DeleteTrailingWS()
autocmd BufWrite *.hs :call DeleteTrailingWS()
autocmd BufWrite *.hamlet :call DeleteTrailingWS()
autocmd BufWrite *.julius :call DeleteTrailingWS()
autocmd BufWrite *.cassius :call DeleteTrailingWS()
autocmd BufWrite *.lucius :call DeleteTrailingWS()

" Update in insert mode
nmap <F2> :update<CR>
vmap <F2> <Esc><F2>gv
imap <F2> <c-o><F2>

" Toggle naughty characters
map <F6> :set list!<CR>
imap <F6> <c-o>:set list!<CR>

" Make it easy to navigate errors (and vimgreps)...
" nmap <silent> <RIGHT>         :cnext<CR>
" nmap <silent> <RIGHT><RIGHT>  :cnf<CR><C-G>
" nmap <silent> <LEFT>          :cprev<CR>
" nmap <silent> <LEFT><LEFT>    :cpf<CR><C-G>

" http://stackoverflow.com/questions/235439/vim-80-column-layout-concerns
highlight OverLength ctermbg=red ctermfg=white guibg=#592929
match OverLength /\%81v.\+/
