""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" plugs
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
call plug#begin('~/.vim/plugged')
    " utilities
    Plug 'tpope/vim-dispatch'
    Plug 'vim-scripts/L9'
    Plug 'Shougo/neocomplete.vim'
    Plug 'w0rp/ale'
    Plug 'tpope/vim-fugitive'
    Plug 'airblade/vim-gitgutter'
    Plug 'jiangmiao/auto-pairs'
    Plug 'wikitopian/hardmode'
    Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --bin'  }
    Plug 'junegunn/fzf.vim'

    " themes
    Plug 'morhetz/gruvbox'
    Plug 'vim-airline/vim-airline'
    "Plug 'vim-airline/vim-airline-themes'

    " syntax
    Plug 'stephpy/vim-php-cs-fixer', { 'for': 'php'  }
    Plug 'prettier/vim-prettier', { 'do': 'sudo npm install -g', 'for': ['javascript', 'typescript', 'css', 'less', 'scss', 'json', 'graphql'] }
    Plug 'Quramy/tsuquyomi', { 'for': 'typescript'  }
    Plug 'leafgarland/typescript-vim', { 'for': 'typescript'  }
    Plug 'jelera/vim-javascript-syntax', { 'for': 'javascript'  }
    Plug 'Quramy/vim-js-pretty-template', { 'for': ['typescript', 'javascript']  }
    Plug 'StanAngeloff/php.vim', { 'for': 'php'  }
    Plug 'cakebaker/scss-syntax.vim', { 'for': 'scss'  }
    Plug 'jwalton512/vim-blade', { 'for': 'blade'  }
    Plug 'othree/html5.vim', { 'for': 'html'  }
    Plug 'mxw/vim-jsx', { 'for': ['typescript', 'javascript']  }
    Plug 'tpope/vim-markdown', { 'for': 'markdown'  }
    Plug 'mustache/vim-mustache-handlebars', { 'for': 'javascript'  }
call plug#end()

"automatic reloading of vimrc"
au VimEnter,BufNewFile,BufReadPost * silent! call HardMode()
au! bufwritepost .vimrc source %

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" key maps
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"let mapleader = ""

nmap <C-p> :bprev<CR>
nmap <C-n> :bnext<CR>
nmap <C-]> :exec("tag ".expand("<cword>"))<CR>
"nmap P "+p

if &diff
    nnoremap <silent><C-j> :diffget LOCAL<CR>
    nnoremap <silent><C-.> :diffget BASE<CR>
    nnoremap <silent><C-k> :diffget REMOTE<CR>
endif

map <F1> @q
map <F3> :Gblame<CR>
map <F4> :call PhpCsFixerFixDirectory()<CR>
map <F5> mzgg=G`z && retab!<CR>
map <F6> :PrettierAsync<CR>
map <F9> :set wrap!<Bar>set wrap?<CR>
map <F10> :set paste<CR>
map <F12> :call PhpCsFixerFixFile()<CR>

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
set splitbelow
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
let base16colorspace=256  " Access colors present in 256 colorspace
set noshowmode    " get rid of the default mode indicator
set complete=.,b,u,]
set completeopt=longest,menu,preview
set showmatch   " Show matching brackets.
set matchtime=8 " Bracket blinking.
set novisualbell    " No blinking .
set noerrorbells    " No noise.
set vb t_vb="."
set laststatus=2    " Always show status line.
set tabpagemax=100   " set maximum number of tabs
set tabstop=4   " Tabs are 4 spaces
set expandtab
set shiftwidth=4    " Define the width of a shift for the<<  and>>  commands. (Tabs under smart indent)
set tabstop=4   " Define what tabstop  is to be simulated when Tab is pressed
set autoindent      " Automatically indent eache line like previous one
set smartindent     " Automatically indent when adding a curly bracket, etc.
set backspace=indent,eol,start    " Allow backspacing over everything in insert mode
set cinwords=if,else,while,do,for,switch,case    " Define keywords that cause an extra indent
set lbr
set list
set list listchars=tab:▶→,trail:•,extends:»,precedes:«,nbsp:×
set nowrap
set encoding=utf-8
set autochdir
set fileformats=unix,mac,dos
set iskeyword+=_,$,@,%,#
set foldmethod=indent   " fold based on indent
set nofoldenable        " dont fold by default
set incsearch   " Search as you type
set ignorecase  " Ignore case when searching
set smartcase   " if there are caps, go case-sensitive
set spell
set spelllang=en_us
set statusline+=%#warningmsg#
set statusline+=%*

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" color
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
colorscheme gruvbox
let g:gruvbox_contrast_dark = 'soft'
let g:gruvbox_italic = 1
let g:gruvbox_termcolors = 256
let g:gruvbox_invert_signs = 1
let g:gruvbox_invert_selection = 1
let g:gruvbox_improved_strings = 1
let g:gruvbox_improved_warnings = 1

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
let g:airline#extensions#tabline#switch_buffers_and_tabs = 0
let g:airline#extensions#tabline#show_buffers = 1

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" ale
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
let g:ale_sign_error = '▸'
let g:ale_sign_warning = "\u26A0"
let g:ale_fixers = { 'javascript': ['eslint']}
let g:ale_fix_on_save = 1
let g:airline#extensions#ale#enabled = 1
let g:ale_echo_msg_error_str = 'E'
let g:ale_echo_msg_warning_str = 'W'
let g:ale_echo_msg_format = '[%linter%] %s [%severity%]'
let g:ale_set_loclist = 0
let g:ale_set_quickfix = 1
let g:ale_linters = {'jsx': ['stylelint', 'eslint']}
let g:ale_linter_aliases = {'jsx': 'css'}

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" markdown
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
let g:markdown_fenced_languages = ['html', 'python', 'bash=sh']

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" fzf
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"[Buffers] Jump to the existing window if possible
let g:fzf_buffers_jump = 1
" [[B]Commits] Customize the options used by 'git log':
let g:fzf_commits_log_options = '--graph --color=always "--format="%C(auto)%h%d %s %C(black)%C(bold)%cr"'
"[Tags] Command to generate tags file
let g:fzf_tags_command = 'ctags -R'
" [Commands] --expect expression for directly executing the command
let g:fzf_commands_expect = 'alt-enter,ctrl-x'

function! s:find_git_root()
  return system('git rev-parse --show-toplevel 2> /dev/null')[:-2]
endfunction

command! ProjectFiles execute 'Files' s:find_git_root()

" Command for git grep
" - fzf#vim#grep(command, with_column, [options], [fullscreen])
command! -bang -nargs=* GGrep
  \ call fzf#vim#grep('git grep --line-number '.shellescape(<q-args>), 0, <bang>0)

" Augmenting Ag command using fzf#vim#with_preview function
"   * fzf#vim#with_preview([[options], preview window, [toggle keys...]])
"     * For syntax-highlighting, Ruby and any of the following tools are required:
"       - Highlight: http://www.andre-simon.de/doku/highlight/en/highlight.php
"       - CodeRay: http://coderay.rubychan.de/
"       - Rouge: https://github.com/jneen/rouge
"
"   :Ag  - Start fzf with hidden preview window that can be enabled with "?" key
"   :Ag! - Start fzf in fullscreen and display the preview window above
command! -bang -nargs=* Ag
  \ call fzf#vim#ag(<q-args>,
  \                 <bang>0 ? fzf#vim#with_preview('up:60%')
  \                         : fzf#vim#with_preview('right:50%:hidden', '?'),
  \                 <bang>0)

" Likewise, Files command with preview window
command! -bang -nargs=? -complete=dir Files
  \ call fzf#vim#files(<q-args>, fzf#vim#with_preview(), <bang>0)

nmap <silent> <C-@> :Buffers<CR>
nmap <silent> / :BLines<CR>
nmap <silent> <C-f> :ProjectFiles<CR>
nmap <silent> <C-g> :GitFiles<CR>

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

let g:jsx_ext_required = 0

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" typescript
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
let g:typescript_indent_disable = 1
let g:typescript_compiler_binary = 'tsc'
let g:typescript_compiler_options = ''
autocmd QuickFixCmdPost [^l]* nested cwindow
autocmd QuickFixCmdPost    l* nested lwindow
autocmd FileType typescript :set makeprg=tsc

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
" prettier
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
let g:prettier#autoformat = 0
let g:prettier#quickfix_enabled = 0
let g:prettier#exec_cmd_async = 1

" max line length that prettier will wrap on
let g:prettier#config#print_width = 170
" number of spaces per indentation level
let g:prettier#config#tab_width = 4
" use tabs over spaces
let g:prettier#config#use_tabs = 'false'
" print semicolons
let g:prettier#config#semi = 'true'
" single quotes over double quotes
let g:prettier#config#single_quote = 'true'
" print spaces between brackets
let g:prettier#config#bracket_spacing = 'true'
" put > on the last line instead of new line
let g:prettier#config#jsx_bracket_same_line = 'true'
" none|es5|all
let g:prettier#config#trailing_comma = 'all'
" flow|babylon|typescript|postcss|json|graphql
let g:prettier#config#parser = 'typescript'
" cli-override|file-override|prefer-file
let g:prettier#config#config_precedence = 'prefer-file'

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
" misc
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
if has('autocmd')
    au BufRead,BufNewFile *.module set filetype=php
    au BufRead,BufNewFile *.install set filetype=php
    au BufRead,BufNewFile *.wsgi set filetype=python
    au BufNewFile,BufRead *.twig set filetype=twig
    au BufNewFile,BufRead *.html.twig set filetype=html.twig
    au BufNewFile,BufRead *.jsx set filetype=javascript.jsx
    au BufNewFile,BufReadPost *.md set filetype=markdown
    au BufWritePre *.js,*.jsx,*.mjs,*.ts,*.tsx,*.css,*.less,*.scss,*.json,*.graphql PrettierAsync

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

    " Don't automatically insert a comment command when entering insert mode with o
    au FileType * setl formatoptions-=o
    " " But do when hitting enter on a comment line, or when wrapping
    au FileType * setl formatoptions+=rq
    " Recognise numbered lists
    au FileType * setl formatoptions+=n
    " Don't automatically break lines when they are too long, unless they are comments
    au FileType * setl formatoptions+=lc
    " And try to remove comment leaders when joining lines
    au FileType * setl formatoptions+=j"

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
