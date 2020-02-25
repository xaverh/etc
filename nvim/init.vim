" " Blinking cursor
" if &term =~ '^xterm\|rxvt\|^st'
" 	let &t_SI = "\<Esc>[5 q"
" 	let &t_SR = "\<Esc>[3 q"
" 	let &t_EI = "\<Esc>[0 q"
" endif
"
" " Comma as leader (default is \)
" " let mapleader = ","
" " let g:mapleader = ","
"
" if has("win32") || has("win64")
" 	let &undodir=$HOME . '\AppData\Local\VIM_UNDO_FILES\'
" else
" 	let &undodir='/tmp/'
" endif
"
" " set t_ts=^[]2;
" " set t_fs=^[\\
" " set t_ts=k
" " set t_fs=\
" " let &t_8f = "\<Esc>[38;2;%lu;%lu;%lum"
" " let &t_8b = "\<Esc>[48;2;%lu;%lu;%lum"
" " let &t_ZH = "\e[3m"
" " let &t_ZR = "\e[23m"
"
" "set cpoptions=q
" "set tabline=%M\ %t
" set autoindent
" set autoread
" set backspace=indent,eol,start
" set backup
" set backupdir=~/.vim/.tmp/,/tmp
" set backupskip=/tmp/*,~/.vim/.tmp/*
" set cindent
" set cinoptions=(0,u0,g0,:0,j1,J1,)200
" set clipboard=unnamed
" " set colorcolumn=+1
" set copyindent
" " XXX
" " set cryptmethod=blowfish2
" "set cursorline
" set directory=~/tmp,/tmp
" set display=truncate
" set encoding=utf8
" set fixendofline
" set foldclose=all
" set foldcolumn=1
" set foldopen=all
" set guioptions+=T
" set guioptions+=m
" set guioptions-=e
" set guioptions-=r
" set guioptions-=t
" set guitablabel=%M\ %t
" set history=1000
" set hlsearch
" set ignorecase
" set incsearch
" set laststatus=2
" set lazyredraw
" set linebreak
" set listchars=eol:¬,extends:»,tab:▸\ ,trail:·
" set matchpairs+=«:»
" set matchtime=2
" set noerrorbells
" set noexpandtab
" set nohidden
" set nostartofline
" set number
" set numberwidth=1
" set pastetoggle=<F8>
" set pyxversion=3
" set noruler
" set scrolloff=5
" set shiftwidth=4
" set shortmess=at
" set showcmd
" set showmatch
" set showtabline=2
" set smartcase
" set smarttab
" set softtabstop=0
" set statusline=%{HasPaste()}%F%M%R%H%W%Y\ [%{&ff}]\ \ %<CWD:\ %r%{getcwd()}%h\ %=Line:\ " %l\ \ \ Column:\ %v\ \ \ %p%%
" set tabstop=4
" set textwidth=0
" set timeoutlen=1000
" set title
" " set ttymouse=urxvt
" set undofile
" set undolevels=5000
" set visualbell t_vb=
" set whichwrap+=<,>,[,],h,l
" " http://stackoverflow.com/questions/526858/" how-do-i-make-vim-do-normal-bash-like-tab-completion-for-file-names
" set wildmode=longest,list,full
" set wildmenu
" set wrap
" set writebackup
"
" " save session
" nnoremap <leader>s :mksession<CR>
"
" " toggle between number and relativenumber
" function! ToggleNumber()
" 	if(&relativenumber == 1)
" 		set norelativenumber
" 		set number
" 	else
" 		set relativenumber
" 	endif
" endfunc
"
" " Make :help appear in a full-screen tab, instead of a window
" augroup HelpInTabs
" 	autocmd!
" 	autocmd BufEnter  *.txt,*.txt.gz   call HelpInNewTab()
" augroup END
" function! HelpInNewTab ()
" 	if &buftype == 'help'
" 		"Convert the help window to a tab...
" 		execute "normal \<C-W>T"
" 	endif
" endfunction
"
" " Returns true if paste mode is enabled
" function! HasPaste()
" 	if &paste
" 		return 'PASTE MODE  '
" 	endif
" 	return ''
" endfunction
"
" " Automatically have Regexes in 'Very Magical Mode'
" nnoremap / /\v
"
" " Make delete key in Normal mode remove the persistently highlighted matches
" nmap <silent>  <BS>  :nohlsearch<CR>
"
" " Mappings for dragvisuals
" vmap  <expr>  <S-LEFT>   DVB_Drag('left')
" vmap  <expr>  <S-RIGHT>  DVB_Drag('right')
" vmap  <expr>  <S-DOWN>   DVB_Drag('down')
" vmap  <expr>  <S-UP>     DVB_Drag('up')
" vmap  <expr>  D          DVB_Duplicate()
"
" " Colors & Fonts & Appearance
" set background=light
" let c_comment_strings=1
"
" " Clang-Format & gofmt
" if has("win32") || has("win64")
" 	map <leader>i :%pyfile C:\\Program Files\ (x86)\\LLVM\\share\\clang\\clang-format." py<cr>
" 	" imap <C-I> <c-o>:%pyfile C:\\Program Files\ (x86)" \\LLVM\\share\\clang\\clang-format.py<cr>
" else
" 	map <leader>i :%pyfile /usr/share/clang/clang-format.py<cr>
" 	" imap <C-I> <c-o>:%pyfile /usr/share/clang/clang-format.py<cr>
" endif
"
" autocmd FileType go map <C-I> :GoFmt<cr>
" autocmd FileType go imap <C-I> <C-O>:GoFmt<cr>
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
" " Map jj to the Esc functionality
" inoremap jj <Esc>
"
" " Always show filetypes in menu
" if has('gui_running')
" 	let do_syntax_sel_menu = 1|runtime~! synmenu.vim|aunmenu &Syntax.&Show\ filetypes\ " in\ menu
" endif
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
" " Delete trailing whitespace on save
" func! DeleteTrailingWS()
" 	exe "normal mz"
" 	%s/\s\+$//ge
" 	exe "normal `z"
" endfunc
" autocmd BufWrite *.py :call DeleteTrailingWS()
" autocmd BufWrite *.coffee :call DeleteTrailingWS()
" autocmd BufWrite *.hs :call DeleteTrailingWS()
"
" " Update in insert mode
" nmap <F2> :update<CR>
" vmap <F2> <Esc><F2>gv
" imap <F2> <c-o><F2>
"
" " Forgot sudo while opening a file?
" cmap w!! w !sudo tee > /dev/null %
"
" " Toggle naughty characters
" map <F6> :set list!<CR>
" imap <F6> <c-o>:set list!<CR>
"
" " Make it easy to navigate errors (and vimgreps)...
" " nmap <silent> <RIGHT>         :cnext<CR>
" " nmap <silent> <RIGHT><RIGHT>  :cnf<CR><C-G>
" " nmap <silent> <LEFT>          :cprev<CR>
" " nmap <silent> <LEFT><LEFT>    :cpf<CR><C-G>
"
" " http://stackoverflow.com/questions/235439/vim-80-column-layout-concerns
" " highlight OverLength ctermbg=red ctermfg=white guibg=#592929
" " match OverLength /\%81v.\+/
"
" " sensible.vim - Defaults everyone can agree on
" " Maintainer:   Tim Pope <http://tpo.pe/>
" " Version:      1.2
"
" if exists('g:loaded_sensible') || &compatible
"   finish
" else
"   let g:loaded_sensible = 'yes'
" endif
"
" if has('autocmd')
"   filetype plugin indent on
" endif
" if has('syntax') && !exists('g:syntax_on')
"   syntax enable
" endif
"
" " Use :help 'option' to see the documentation for the given option.
"
" set autoindent
" set backspace=indent,eol,start
" set complete-=i
" set smarttab
"
" set nrformats-=octal
"
" if !has('nvim') && &ttimeoutlen == -1
"   set ttimeout
"   set ttimeoutlen=100
" endif
"
" set incsearch
" " Use <C-L> to clear the highlighting of :set hlsearch.
" if maparg('<C-L>', 'n') ==# ''
"   nnoremap <silent> <C-L> :nohlsearch<C-R>=has('diff')?'<Bar>diffupdate':''<CR><CR><C-L>
" endif
"
" set laststatus=2
" set ruler
" set wildmenu
"
" if !&scrolloff
"   set scrolloff=1
" endif
" if !&sidescrolloff
"   set sidescrolloff=5
" endif
" set display+=lastline
"
" if &encoding ==# 'latin1' && has('gui_running')
"   set encoding=utf-8
" endif
"
" if &listchars ==# 'eol:$'
"   set listchars=tab:>\ ,trail:-,extends:>,precedes:<,nbsp:+
" endif
"
" if v:version > 703 || v:version == 703 && has("patch541")
"   set formatoptions+=j " Delete comment character when joining commented lines
" endif
"
" if has('path_extra')
"   setglobal tags-=./tags tags-=./tags; tags^=./tags;
" endif
"
" if &shell =~# 'fish$' && (v:version < 704 || v:version == 704 && !has('patch276'))
"   set shell=/usr/bin/env\ bash
" endif
"
" set autoread
"
" if &history < 1000
"   set history=1000
" endif
" if &tabpagemax < 50
"   set tabpagemax=50
" endif
" if !empty(&viminfo)
"   set viminfo^=!
" endif
" set sessionoptions-=options
"
" " Allow color schemes to do bright colors without forcing bold.
" if &t_Co == 8 && $TERM !~# '^linux\|^Eterm'
"   set t_Co=16
" endif
"
" " Load matchit.vim, but only if the user hasn't installed a newer version.
" if !exists('g:loaded_matchit') && findfile('plugin/matchit.vim', &rtp) ==# ''
"   runtime! macros/matchit.vim
" endif
"
" inoremap <C-U> <C-G>u<C-U>
"
" " vim:set ft=vim et sw=2:
"
"
" 