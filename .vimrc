""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Use Vim settings, rather then Vi settings (much better!).
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"automatic reloading of vimrc"
autocmd! bufwritepost .vimrc source %

hi StatusLine guifg=black guibg=white
set nocompatible    " Unable Vi compatibility
set ttyfast
set wildmode=longest,list:longest
set autowrite
set textauto
set winminheight=0

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Global Settings
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
set showcmd                       " Show incomplete cmds down the bottom
set showmode                      " Show current mode down the bottom
set clipboard=autoselectplus      " save the selection into the system clipboard
set ruler                         " Ruler on
set number                        " Display line numbers
set timeoutlen=5                  " Time to wait after ESC (default causes an annoying delay)
set nobackup
set noswapfile
set hlsearch                      " Highlight search strings
set nowritebackup
set winaltkeys=yes
set modeline
set shortmess+=filmnrxoOtT
set viewoptions=folds,options,cursor,unix,slash " better unix / windows compatibility
set history=1000
set gdefault
set autoread
set magic                         " change the way backslashes are used in search patterns
set confirm
syntax enable

"set list
"set listchars=tab:>.,trail:.,extends:#,nbsp:.

"buffer check
set hidden
set ttimeout
set ttimeoutlen=50

set background=dark     " enable for dark terminals
colorscheme railscasts

set lazyredraw
set wildmenu
set numberwidth=5
set scrolloff=10
"set statusline=%<%F%h%m%r%h%w\ %y\ %{&ff}\ %{strftime(\"%c\",getftime(expand(\"%:p\")))}%=\ %{fugitive#statusline()}\ lin:%l\,%L\ col:%c%V\ pos:%o\ ascii:%b\ %P
"set statusline=%<%F%h%m%r%h%w%=\ %{fugitive#statusline()}\ lin:%l\,%L\ col:%c%V\ %P
set formatoptions=rq
set t_Co=256

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Stupid shift key fixes
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
cmap W w
cmap WQ wq
cmap wQ wq
cmap Q q
cmap Tabe tabe

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Ag - code Search tool
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
let g:agprg="ag --column --smart-case --hidden --stats"
let g:aghighlight=1


""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Improved status bar - airline
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
set noshowmode                    " get rid of the default mode indicator
let g:airline#extensions#tabline#enabled = 1
set fillchars+=stl:\ ,stlnc:\
let g:airline_section_c = '%F'
let g:airline_section_y = '-%L-'

let g:bufferline_echo = 0
let g:airline_powerline_fonts = 1
let g:airline#extensions#ctrlp#color_template = 'visual'
let g:airline#extensions#tabline#show_tab_nr = 1
let g:airline#extensions#tabline#tab_nr_type = 1 " tab number
let g:airline#extensions#whitespace#enabled = 1
let g:airline#extensions#csv#enabled = 1
let g:airline#extensions#syntastic#enabled = 1
let g:airline#extensions#branch#empty_message = 'git it!!'


if !exists('g:airline_symbols')
  let g:airline_symbols = {}
endif
let g:airline_symbols.space = "\ua0"

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" CLIPBOARD
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"vmap <C-c> y:call system("xclip -i -selection clipboard", getreg("\""))<CR>:call system("xclip -i", getreg("\""))<CR>
"nmap <C-v> :call setreg("\"",system("xclip -o -selection clipboard"))<CR>p

nmap <F1> @q
nnoremap <silent><C-n> :tabnext<CR>
nnoremap <silent><C-p> :tabprevious<CR>
noremap <C-j> :bprev<CR>
noremap <C-k> :bnext<CR>

let g:buftabs_only_basename=1
let g:buftabs_in_statusline=1

if &diff
  nnoremap <silent><C-j> :diffget LOCAL<CR>
"  nnoremap <silent><C-.> :diffget BASE<CR>
  nnoremap <silent><C-k> :diffget REMOTE<CR>
endif

map <F3> :TlistToggle<CR>
map <F5> :SyntasticCheck<CR>
map <F4> :SyntasticToggleMode<CR>
map <F10> :set paste<CR>
map <F9>  :set wrap!<Bar>set wrap?<CR>
nmap <F8> :TagbarToggle<CR>

map <C-L> :!php -l %<CR>

map <C-\> :tab split<CR>:exec("tag ".expand("<cword>"))<CR>
map <A-]> :vsp <CR>:exec("tag ".expand("<cword>"))<CR>


""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Completion Settings
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
set complete=.,b,u,]
set completeopt=menu,preview

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Visual Settings
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
set showmatch       " Show matching brackets.
set matchtime=8     " Bracket blinking.
set novisualbell    " No blinking .
set noerrorbells    " No noise.
set vb t_vb="."
set laststatus=2    " Always show status line.
set tabpagemax=50   " set maximum number of tabs
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" Indentation
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
filetype plugin indent on

"set tabstop=4                                   " Tabs are 2 spaces
set expandtab
set shiftwidth=4                                " Define the width of a shift for the<<  and>>  commands. (Tabs under smart indent)
set softtabstop=4                               " Define what tabstop  is to be simulated when Tab is pressed
set autoindent                                  " Automatically indent eache line like previous one
set smartindent                                 " Automatically indent when adding a curly bracket, etc.
set backspace=indent,eol,start                  " Allow backspacing over everything in insert mode
"set smarttab                                    " Insert indents at the beginning of a line
set cinwords=if,else,while,do,for,switch,case   " Define keywords that cause an extra indent
set lbr
set nowrap

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Encoding
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
set encoding=utf-8

set autochdir
set fileformats=unix,mac,dos
set iskeyword+=_,$,@,%,#


""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Folding Settings
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
set foldmethod=indent   " fold based on indent
set foldnestmax=3       " deepest fold is 3 levels
set nofoldenable        " dont fold by default

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" syntax stuff
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

  let html_number_lines = 0
  let html_use_css = 0
  let use_xhtml = 0
  let html_wrong_comments=1
  let php_sql_query = 1
  let php_baselib = 1
  let php_htmlInStrings = 1
  let hs_highlight_delimiters = 1
  let hs_highlight_boolean = 1
  let hs_highlight_types = 1
  let java_javascript=1
  let java_css=1
  let msql_sql_query = 1
  let apache_version = "2.0"
  let enforce_freedesktop_standard = 1
  let python_highlight_all = 1

  let g:syntastic_always_populate_loc_list=1
  let g:syntastic_php_checkers=['php', 'phpcs', 'phpmd']
  let g:syntastic_javascript_checkers = ['jslint']
  let g:syntastic_enable_signs=1
  let g:syntastic_auto_jump=1
  let g:syntastic_stl_format = '[%E{Err: %fe #%e}%B{, }%W{Warn: %fw #%w}]'

  let g:gitgutter_escape_grep = 1
  let g:gitgutter_enabled = 1
  let g:gitgutter_signs = 1
  let g:gitgutter_highlight_lines = 0
  let g:gitgutter_realtime = 1
  let g:gitgutter_eager = 1

"  let g:is_bash = 1

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" NERD_tree
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

  map <F7> :execute 'NERDTreeToggle ' . getcwd()<CR>
  let NERDChristmasTree = 1
  let NERDTreeCaseSensitiveSort = 1
  let NERDTreeIgnore = ['\~$','\.[ao]$','\.swp$','\.DS_Store','\.pyc','\.pyo','\coverage']
  let NERDTreeMouseMode = 2
  let NERDTreeShowHidden = 1
  let NERDTreeChDirMode = 2
  let NERDTreeWinPos = "right"
  let NERDTreeWinSize = 40
  let g:NERDTreeDirArrowExpandable = '▸'
  let g:NERDTreeDirArrowCollapsible = '▾'

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" VIM-Tags
" https://github.com/szw/vim-tags
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

  let g:vim_tags_auto_generate = 1
  let g:vim_tags_use_vim_dispatch = 1

  " required for https://github.com/Valloric/YouCompleteMe
  let g:vim_tags_use_language_field = 1

  let g:vim_tags_ignore_files = ['.gitignore', '.svnignore', '.cvsignore']
  let g:vim_tags_ignore_file_comment_pattern = '^[#""]'
  let g:vim_tags_directories = [".git", ".hg", ".svn", ".bzr", "_darcs", "CVS"]
  let g:vim_tags_main_file = 'tags'
  let g:vim_tags_extension = '.tags'
  let g:vim_tags_cache_dir = expand($HOME)


""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Search Settings
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
set incsearch   " Search as you type
set ignorecase  " Ignore case when searching
set smartcase   " if there are caps, go case-sensitive

set tags=./tags,tags,./.git/tags;

set ofu=syntaxcomplete#Complete

" Trailing or broken whitespace.
let c_space_errors=1
highlight WhitespaceEOL ctermbg=red guibg=red
match WhitespaceEOL /\s\+$/
set backupcopy=auto,breakhardlink

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Miscellaneous
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
if has("autocmd")
  au BufRead,BufNewFile *.module set filetype=php
  au BufRead,BufNewFile *.install set filetype=php
  au BufRead,BufNewFile *.wsgi set filetype=python
  autocmd BufNewFile,BufRead *.twig set filetype=twig
  autocmd BufNewFile,BufRead *.html.twig set filetype=html.twig

  autocmd FileType javascript set omnifunc=javascriptcomplete#CompleteJS
  autocmd FileType php set omnifunc=phpcomplete#CompletePHP
  autocmd FileType php setlocal makeprg=zca\ %<.php
  autocmd FileType php setlocal errorformat=%f(line\ %l):\ %m
  autocmd FileType python set expandtab shiftwidth=4 softtabstop=4 omnifunc=pythoncomplete#Complete
  autocmd FileType html set softtabstop=4 shiftwidth=4 textwidth=0 omnifunc=htmlcomplete#CompleteTags
  autocmd FileType css set softtabstop=4 shiftwidth=4 textwidth=0 omnifunc=csscomplete#CompleteCSS
  autocmd FileType c,cpp,java,php,module,tpl.php,js,python,twig,xml,yml autocmd BufWritePre <buffer> :call setline(1,map(getline(1,"$"),'substitute(v:val,"\\s\\+$","","")'))

  autocmd bufenter * if (winnr("$") == 1 && exists("b:NERDTree") && b:NERDTree.isTabTree()) | q | endif

  au BufReadPost * if line("'\"") > 1 && line("'\"") <= line("$") | exe "normal! g'\"" | endif
  au FileType javascript call JavaScriptFold()

  au VimEnter * AirlineTheme serene
endif
