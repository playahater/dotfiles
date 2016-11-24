""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" plugs
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
call plug#begin('~/.vim/plugged')
    " utilities
    Plug 'tpope/vim-dispatch'
    Plug 'vim-scripts/L9'
    Plug 'Shougo/neocomplete.vim'
    Plug 'tpope/vim-fugitive'
    Plug 'airblade/vim-gitgutter'
    Plug 'jiangmiao/auto-pairs'
    Plug 'alvan/vim-closetag'
    Plug 'szw/vim-tags'
    Plug 'majutsushi/tagbar'
    Plug 'vim-syntastic/syntastic'
    Plug 'scrooloose/nerdtree', { 'on':  'NERDTreeToggle' }
    Plug 'SirVer/ultisnips' | Plug 'honza/vim-snippets'
    Plug 'vim-ctrlspace/vim-ctrlspace'
    Plug 'wikitopian/hardmode'
    Plug 'scrooloose/nerdcommenter'
    "Plug 'joonty/vdebug'

    " themes
    Plug 'morhetz/gruvbox'
    Plug 'vim-airline/vim-airline'
    "Plug 'vim-airline/vim-airline-themes'

    " syntax
    Plug 'StanAngeloff/php.vim'
    Plug 'cakebaker/scss-syntax.vim'
    Plug 'jwalton512/vim-blade'
    Plug 'pangloss/vim-javascript'
    Plug 'othree/html5.vim'
    Plug 'tpope/vim-markdown'
    Plug 'mustache/vim-mustache-handlebars'
call plug#end()

"automatic reloading of vimrc"
au VimEnter,BufNewFile,BufReadPost * silent! call HardMode()
au! bufwritepost .vimrc source %

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" key maps
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
nmap <silent><C-n> :tabnext<CR>
nmap <silent><C-p> :tabprevious<CR>
nmap <C-j> :bprev<CR>
nmap <C-k> :bnext<CR>

if &diff
    nnoremap <silent><C-j> :diffget LOCAL<CR>
    "nnoremap <silent><C-.> :diffget BASE<CR>
    nnoremap <silent><C-k> :diffget REMOTE<CR>
endif

map <F1> @q
map <F3> :TlistToggle<CR>
map <F4> :SyntasticToggleMode<CR>
map <F5> mzgg=G`z<CR>
map <F7> :execute 'NERDTreeToggle ' . getcwd()<CR>
map <F8> :TagbarToggle<CR>
map <F9> :set wrap!<Bar>set wrap?<CR>
map <F10> :set paste<CR>
map <C-L> :!php -l %<CR>
map <C-\> :tab split<CR>:exec("tag ".expand("<cword>"))<CR>
map <A-]> :vsp <CR>:exec("tag ".expand("<cword>"))<CR>
map <C-c> :call NERDComment(0,"toggle")<C-m>

inoremap <expr><C-g>     neocomplete#undo_completion()
inoremap <expr><C-l>     neocomplete#complete_common_string()

" <CR>: close popup and save indent.
inoremap <silent> <CR> <C-r>=<SID>my_cr_function()<CR>
function! s:my_cr_function()
    return pumvisible() ? "\<C-y>" : "\<CR>"
endfunction

" <TAB>: completion.
inoremap <expr><TAB>  pumvisible() ? "\<C-n>" : "\<TAB>"
" <C-h>, <BS>: close popup and delete backword char.
inoremap <expr><C-h> neocomplete#smart_close_popup()."\<C-h>"
inoremap <expr><BS> neocomplete#smart_close_popup()."\<C-h>"

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" global
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
syntax enable
filetype plugin indent on

set nocompatible    " Unable Vi compatibility
set ttyfast
set wildmode=longest,list:longest
set autowrite
set textauto
set winminheight=0
set showcmd     " Show incomplete cmds down the bottom
set showmode    " Show current mode down the bottom
set clipboard=autoselectplus    " save the selection into the system clipboard
set ruler   " Ruler on
set number  " Display line numbers
set timeoutlen=1    " Time to wait after ESC
set nobackup
set noswapfile
set hlsearch    " Highlight search strings
set nowritebackup
set winaltkeys=yes
set modeline
set shortmess+=filmnrxoOtT
set viewoptions=folds,options,cursor,unix,slash
set history=1000
set gdefault
set autoread
set magic   " change the way backslashes are used in search patterns
set confirm
set showtabline=0
set hidden
set ttimeout
set ttimeoutlen=50
set background=dark " enable for dark terminals
set lazyredraw
set wildmenu
set numberwidth=5
set scrolloff=10
set formatoptions=rq
set t_Co=256
set noshowmode    " get rid of the default mode indicator
set complete=.,b,u,]
set completeopt=longest,menu,preview
set showmatch   " Show matching brackets.
set matchtime=8 " Bracket blinking.
set novisualbell    " No blinking .
set noerrorbells    " No noise.
set vb t_vb="."
set laststatus=2    " Always show status line.
set tabpagemax=50   " set maximum number of tabs
set tabstop=4   " Tabs are 4 spaces
set expandtab
set shiftwidth=4    " Define the width of a shift for the<<  and>>  commands. (Tabs under smart indent)
set softtabstop=4   " Define what tabstop  is to be simulated when Tab is pressed
set autoindent      " Automatically indent eache line like previous one
set smartindent     " Automatically indent when adding a curly bracket, etc.
set backspace=indent,eol,start    " Allow backspacing over everything in insert mode
set cinwords=if,else,while,do,for,switch,case    " Define keywords that cause an extra indent
set lbr
set nowrap
set encoding=utf-8
set autochdir
set fileformats=unix,mac,dos
set iskeyword+=_,$,@,%,#
set foldmethod=indent   " fold based on indent
set foldnestmax=3       " deepest fold is 3 levels
set nofoldenable        " dont fold by default
set incsearch   " Search as you type
set ignorecase  " Ignore case when searching
set smartcase   " if there are caps, go case-sensitive
set statusline+=%#warningmsg#
set statusline+=%{SyntasticStatuslineFlag()}
set statusline+=%*

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" color
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
colorscheme gruvbox
let g:gruvbox_contrast_dark = 'hard'
let g:gruvbox_italic = 1
"let g:gruvbox_invert_signs = 1

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" system
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
let g:buftabs_only_basename=1
let g:buftabs_in_statusline=1
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
let apache_version = '2.0'
let enforce_freedesktop_standard = 1
let python_highlight_all = 1

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" search
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
set tags=./tags,tags,./.git/tags;
set ofu=syntaxcomplete#Complete
set backupcopy=auto,breakhardlink

" Trailing or broken whitespace.
let c_space_errors=1
highlight WhitespaceEOL ctermbg=red guibg=red
match WhitespaceEOL /\s\+$/

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" airline
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
if !exists('g:airline_symbols')
    let g:airline_symbols = {}
endif
let g:airline_symbols.space = "\ua0"
let g:airline#extensions#tabline#enabled = 0
set fillchars+=stl:\ ,stlnc:\
let g:airline_section_c = '%F'
let g:airline_section_y = '-%L-'
let g:bufferline_echo = 0
let g:airline_powerline_fonts = 1
let g:airline#extensions#ctrlp#color_template = 'visual'
let g:airline#extensions#tabline#show_tab_nr = 0
let g:airline#extensions#tabline#tab_nr_type = 0 " tab number
let g:airline#extensions#whitespace#enabled = 1
let g:airline#extensions#csv#enabled = 1
let g:airline#extensions#syntastic#enabled = 1
let g:airline#extensions#branch#empty_message = 'git it!!'
let g:airline_exclude_preview = 1
let g:airline#extensions#bufferline#enabled = 1
let g:airline#extensions#ctrlspace#enabled = 1
let g:airline#extensions#tabline#ctrlspace#enabled = 1
let g:airline#extensions#tabline#switch_buffers_and_tabs = 0
let g:airline#extensions#tabline#show_buffers = 1

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" syntastic
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
let g:syntastic_php_checkers=['php', 'phpmd']
let g:syntastic_python_checkers = ['pylint']
let g:syntastic_javascript_checkers = ['jshint']
let g:syntastic_enable_signs=1
let g:syntastic_auto_jump=1
let g:syntastic_auto_loc_list = 1
let g:syntastic_always_populate_loc_list=1
let g:syntastic_check_on_open = 1
let g:syntastic_check_on_wq = 1
let g:syntastic_quiet_messages = { "!level" : "errors", "type" : "style",  "regex" : "\m\[C03\d\d\]" }
let g:syntastic_error_symbol = '▸'
let g:syntastic_warning_symbol = "\u26A0"
let g:syntastic_stl_format = "[%E{Err: %fe #%e}%B{, }%W{Warn: %fw #%w}]"

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" nerdcommenter
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
let g:NERDSpaceDelims = 1
let g:NERDCompactSexyComs = 1
let g:NERDDefaultAlign = 'left'
let g:NERDAltDelims_java = 1
let g:NERDCustomDelimiters = { 'c': { 'left': '/**','right': '*/' } }
let g:NERDCommentEmptyLines = 1
let g:NERDTrimTrailingWhitespace = 1

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" markdown
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
let g:markdown_fenced_languages = ['html', 'python', 'bash=sh']

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" autopairs
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
let g:AutoPairsFlyMode = 1

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" javascript
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
let g:javascript_plugin_jsdoc           = 1
let g:javascript_plugin_ngdoc           = 1
let g:javascript_plugin_flow            = 1
let g:javascript_conceal_function       = "ƒ"
let g:javascript_conceal_null           = "ø"
let g:javascript_conceal_this           = "@"
let g:javascript_conceal_return         = "⇚"
let g:javascript_conceal_undefined      = "¿"
let g:javascript_conceal_NaN            = "ℕ"
let g:javascript_conceal_prototype      = "¶"
let g:javascript_conceal_static         = "•"
let g:javascript_conceal_super          = "Ω"
let g:javascript_conceal_arrow_function = "⇒"

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" gitgutter
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
let g:gitgutter_escape_grep = 1
let g:gitgutter_enabled = 1
let g:gitgutter_signs = 1
let g:gitgutter_highlight_lines = 0
let g:gitgutter_realtime = 1
let g:gitgutter_eager = 1

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" NERD_tree
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
let NERDChristmasTree = 1
let NERDTreeCaseSensitiveSort = 1
let NERDTreeIgnore = ['\~$','\.[ao]$','\.swp$','\.DS_Store','\.pyc','\.pyo','\coverage']
let NERDTreeMouseMode = 2
let NERDTreeShowHidden = 1
let NERDTreeChDirMode = 2
let NERDTreeWinPos = 'right'
let NERDTreeWinSize = 45
let g:NERDTreeDirArrowExpandable = '▸'
let g:NERDTreeDirArrowCollapsible = '▾'

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" vimtags
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
let g:vim_tags_auto_generate = 1
let g:vim_tags_use_vim_dispatch = 1
let g:vim_tags_ignore_files = ['.gitignore', '.svnignore', '.cvsignore']
let g:vim_tags_ignore_file_comment_pattern = '^[#""]'
let g:vim_tags_directories = [".git", ".hg", ".svn", ".bzr", "_darcs", "CVS"]
let g:vim_tags_main_file = 'tags'
let g:vim_tags_extension = '.tags'
let g:vim_tags_cache_dir = expand($HOME)

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" ultisnips
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
let g:UltiSnipsExpandTrigger="<tab>"
let g:UltiSnipsJumpForwardTrigger="<c-b>"
let g:UltiSnipsJumpBackwardTrigger="<c-z>"

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" neocomplete
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
let g:acp_enableAtStartup = 0
let g:neocomplete#enable_auto_select = 1
let g:neocomplete#enable_at_startup = 1
let g:neocomplete#enable_smart_case = 1
let g:neocomplete#sources#syntax#min_keyword_length = 3
let g:neocomplete#lock_buffer_name_pattern = '\*ku\*'
let g:neocomplete#enable_auto_select = 1
if !exists('g:neocomplete#keyword_patterns')
    let g:neocomplete#keyword_patterns = {}
endif
let g:neocomplete#keyword_patterns['default'] = '\h\w*'

if !exists('g:neocomplete#sources#omni#input_patterns')
    let g:neocomplete#sources#omni#input_patterns = {}
endif
let g:neocomplete#sources#omni#input_patterns.php = '[^. \t]->\h\w*\|\h\w*::'
let g:neocomplete#sources#omni#input_patterns.c = '[^.[:digit:] *\t]\%(\.\|->\)'
let g:neocomplete#sources#omni#input_patterns.cpp = '[^.[:digit:] *\t]\%(\.\|->\)\|\h\w*::'
let g:neocomplete#sources#omni#input_patterns.perl = '\h\w*->\h\w*\|\h\w*::'

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" ctrlspace
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
let g:CtrlSpaceLoadLastWorkspaceOnStart = 1
let g:CtrlSpaceSaveWorkspaceOnSwitch = 1
let g:CtrlSpaceSaveWorkspaceOnExit = 1
let g:CtrlSpaceSearchTiming = 200
let g:CtrlSpaceUseTabline = 1
let g:CtrlSpaceFileEngine = 'file_engine_linux_amd64'
let g:CtrlSpaceStatuslineFunction = 'airline#extensions#ctrlspace#statusline()'
let g:CtrlSpaceGlobCommand = 'ag -l --nocolor -g ""'
let g:CtrlSpaceUseTabline = 0

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" misc
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
if has('autocmd')
    au BufRead,BufNewFile *.module set filetype=php
    au BufRead,BufNewFile *.install set filetype=php
    au BufRead,BufNewFile *.wsgi set filetype=python
    au BufNewFile,BufRead *.twig set filetype=twig
    au BufNewFile,BufRead *.html.twig set filetype=html.twig
    au BufNewFile,BufReadPost *.md set filetype=markdown

    au FileType xml setlocal omnifunc=xmlcomplete#CompleteTags
    au FileType html,markdown setlocal omnifunc=htmlcomplete#CompleteTags
    au FileType javascript set omnifunc=javascriptcomplete#CompleteJS
    au FileType php set omnifunc=phpcomplete#CompletePHP
    au FileType php setlocal makeprg=zca\ %<.php
    au FileType php setlocal errorformat=%f(line\ %l):\ %m
    au FileType python set expandtab shiftwidth=4 softtabstop=4 omnifunc=pythoncomplete#Complete
    au FileType html set softtabstop=4 shiftwidth=4 textwidth=0 omnifunc=htmlcomplete#CompleteTags
    au FileType css set softtabstop=4 shiftwidth=4 textwidth=0 omnifunc=csscomplete#CompleteCSS
    au FileType c,cpp,java,php,module,tpl.php,js,python,twig,xml,yml au BufWritePre <buffer> :call setline(1,map(getline(1,"$"),'substitute(v:val,"\\s\\+$","","")'))

    au bufenter * if (winnr("$") == 1 && exists("b:NERDTree") && b:NERDTree.isTabTree()) | q | endif

    au BufReadPost * if line("'\"") > 1 && line("'\"") <= line("$") | exe "normal! g'\"" | endif

    function! PhpSyntaxOverride()
        hi! def link phpDocTags  phpDefine
        hi! def link phpDocParam phpType
    endfunction

    augroup phpSyntaxOverride
        au!
        au FileType php call PhpSyntaxOverride()
    augroup END

    au VimEnter * AirlineTheme gruvbox
endif
