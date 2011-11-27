""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Use Vim settings, rather then Vi settings (much better!).
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
hi StatusLine guifg=black guibg=white
set nocompatible    " Unable Vi compatibility
set ttyfast
set wildmode=longest,list
set autowrite
set textauto
set winminheight=0

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Global Settings
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
set showcmd             " Show incomplete cmds down the bottom
set showmode            " Show current mode down the bottom
set clipboard+=unnamed  " Yanks go on clipboard instead
set ruler               " Ruler on
set number      " Display line numbers
set timeoutlen=250      " Time to wait after ESC (default causes an annoying delay)
set nobackup            " 
set hlsearch    " Highlight search strings
set nowritebackup       " 
set winaltkeys=yes
syntax on           " Turn on syntax highlighting

colorscheme railscasts
set wildmenu
"set background=dark
set numberwidth=5
set scrolloff=10
set statusline=%<%F%h%m%r%h%w\ %y\ %{&ff}\ %{strftime(\"%c\",getftime(expand(\"%:p\")))}%=\ %{fugitive#statusline()}\ lin:%l\,%L\ col:%c%V\ pos:%o\ ascii:%b\ %P
set expandtab
set formatoptions=rq
"set t_Co=256 


""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Copy to X CLIPBOARD
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" map <leader>cc :w !xsel -i -b<CR>
" map <leader>cp :w !xsel -i -p<CR>
" map <leader>cs :w !xsel -i -s<CR>

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Paste from X CLIPBOARD
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" map <leader>pp :r!xsel -p<CR>
" map <leader>ps :r!xsel -s<CR>
" map <leader>pb :r!xsel -b<CR>
"
vmap <C-c> y:call system("xclip -i -selection clipboard", getreg("\""))<CR>:call system("xclip -i", getreg("\""))<CR>
nmap <C-v> :call setreg("\"",system("xclip -o -selection clipboard"))<CR>p

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Tabs and Buffers
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"map <D-S-]> gt
"map <D-S-[> gT
map <D-1> 1gt
map <D-2> 2gt
map <D-3> 3gt
map <D-4> 4gt
map <D-5> 5gt
map <D-6> 6gt
map <D-7> 7gt
map <D-8> 8gt
map <D-9> 9gt
map <D-0> :tablast<CR>

nnoremap <silent><C-n> :tabnext<CR>
nnoremap <silent><C-p> :tabprevious<CR>
nnoremap <silent><C-t> :tabnew<CR>
noremap <C-j> :bprev<CR>
noremap <C-k> :bnext<CR>

let g:buftabs_only_basename=1
let g:buftabs_in_statusline=1

map <F10> :set paste<CR>
map <F9>  :set wrap!<Bar>set wrap?<CR>

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Visual Settings
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
set showmatch       " Show matching brackets.
set matchtime=8     " Bracket blinking.
set novisualbell    " No blinking .
set noerrorbells    " No noise.
set vb t_vb="."
set laststatus=2    " Always show status line.
set tabpagemax=20   " 
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" Indentation
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
set tabstop=2                                   " Tabs are 2 spaces
set shiftwidth=2                                " Define the width of a shift for the<<  and>>  commands. (Tabs under smart indent)
set softtabstop=4                               " Define whattabstop  is to be simulated when Tab is pressed
set autoindent                                  " Automatically indent eache line like previous one
set smartindent                                 " Automatically indent when adding a curly bracket, etc.
set backspace=indent,eol,start                  " Allow backspacing over everything in insert mode
set smarttab                                    " Insert indents at the beginning os a line
set cinwords=if,else,while,do,for,switch,case   " Define keywords that cause an extra indent
set lbr
"set nowrap

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Encoding
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
set encoding=utf-8

filetype plugin indent on

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
"  let g:is_bash = 1

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" NERD_tree
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

  map <F7> :execute 'NERDTreeToggle ' . getcwd()<CR>
  "nmap <F7> :NERDTreeToggle<CR>
  let NERDChristmasTree = 1
  let NERDTreeCaseSensitiveSort = 1
  let NERDTreeIgnore = ['\~$','\.[ao]$','\.swp$','\.DS_Store','\.pyc','\.pyo','\vendor','\coverage']
  let NERDTreeMouseMode = 2
  let NERDTreeShowHidden = 1
  let NERDTreeChDirMode = 2
  let NERDTreeWinPos = "right"
  let NERDTreeWinSize = 50

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Search Settings
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
set incsearch   " Search as you type
set ignorecase  " Ignore case when searching
set smartcase   " 

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Miscellaneous
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
if has("autocmd")
  " Drupal *.module and *.install files.
  augroup module
    autocmd BufRead,BufNewFile *.module set filetype=php
    autocmd BufRead,BufNewFile *.install set filetype=php
  augroup END
endif

au BufRead,BufNewFile *.js set ft=javascript.jquery
