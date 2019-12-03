""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" plugs
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
call plug#begin('~/.vim/plugged')
  " utilities

  Plug 'prabirshrestha/async.vim'
  Plug 'prabirshrestha/vim-lsp'
  Plug 'prabirshrestha/asyncomplete.vim'
  Plug 'prabirshrestha/asyncomplete-lsp.vim'
  Plug 'prabirshrestha/asyncomplete-file.vim'
  Plug 'prabirshrestha/asyncomplete-tscompletejob.vim'
  Plug 'runoshun/tscompletejob'

  Plug 'tpope/vim-fugitive'
  Plug 'airblade/vim-gitgutter'
  Plug 'wikitopian/hardmode'
  Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --bin'  }
  Plug 'junegunn/fzf.vim'

  " themes
  Plug 'morhetz/gruvbox'
  Plug 'vim-airline/vim-airline'

  " syntax
  Plug 'prettier/vim-prettier', { 'do': 'npm install -g', 'branch': 'release/1.x', 'for': [ 'javascript', 'typescript', 'css', 'less', 'scss', 'json', 'graphql', 'markdown', 'vue', 'lua', 'php', 'python', 'ruby', 'html', 'swift' ] }
  Plug 'felixfbecker/php-language-server', {'do': 'composer install && composer run-script parse-stubs', 'for': ['php'] }

  Plug 'leafgarland/typescript-vim', { 'for': 'typescript' }
  Plug 'Quramy/vim-js-pretty-template', { 'for': ['typescript', 'javascript'] }
  Plug 'pangloss/vim-javascript', { 'for': ['typescript', 'javascript'] }
  Plug 'mxw/vim-jsx', { 'for': ['typescript', 'javascript'] }
  Plug 'mustache/vim-mustache-handlebars', { 'for': 'javascript' }
  Plug 'StanAngeloff/php.vim', { 'for': 'php' }
  Plug 'cakebaker/scss-syntax.vim', { 'for': 'scss' }
  Plug 'jwalton512/vim-blade', { 'for': 'blade' }
  Plug 'othree/html5.vim', { 'for': 'html' }
  Plug 'tpope/vim-markdown', { 'for': 'markdown' }

call plug#end()


""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" key maps
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

if &diff
  nnoremap <silent><C-j> :diffget LOCAL<CR>
  nnoremap <silent><C-.> :diffget BASE<CR>
  nnoremap <silent><C-k> :diffget REMOTE<CR>
endif

nnoremap <C-\> :noh<CR>
nmap <C-p> :bprev<CR>
nmap <C-n> :bnext<CR>
map <F2> :%s/\s\+$//e<CR>
map <F3> :Gblame<CR>
map <F5> mzgg=G`z && retab!<CR>
map <F6> :PrettierAsync<CR>
map <F9> :set wrap!<Bar>set wrap?<CR>
map <F10> :set paste<CR>

" https://github.com/prabirshrestha/vim-lsp
nnoremap <C-]> :LspDefinition<CR>
nnoremap <ESC>[ :LspReferences<CR>
nnoremap <C-'> :LspRename<CR>
nnoremap <C-;> :LspDocumentFormat<CR>

nnoremap <ESC>s :Gstatus<CR>

inoremap <expr> <Tab> pumvisible() ? "\<C-n>" : "\<Tab>"
inoremap <expr> <S-Tab> pumvisible() ? "\<C-p>" : "\<S-Tab>"
inoremap <expr> <cr> pumvisible() ? "\<C-y>" : "\<cr>"

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" global
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
syntax enable
filetype plugin indent on

set completeopt+=preview
set t_Co=256
let base16colorspace=256  " Access colors present in 256 colorspace
set number  " Display line numbers
set timeoutlen=1    " Time to wait after ESC
set nobackup
set noswapfile
set tabstop=2   " Tabs are 4 spaces
set expandtab
set shiftwidth=2    " Define the width of a shift for the<<  and>>  commands. (Tabs under smart indent)
set autoindent      " Automatically indent eache line like previous one
set smartindent     " Automatically indent when adding a curly bracket, etc.
set backspace=2
set clipboard=unnamed
set hlsearch
set history=1000
set hidden
set magic
set showtabline=0
set incsearch
set ignorecase
set smartcase
set scrolloff=10
set statusline+=%#warningmsg#
set statusline+=%*
set colorcolumn=150
highlight ColorColumn ctermbg=232

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" color
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
set background=dark
colorscheme gruvbox
let g:gruvbox_contrast_dark = 'soft'
let g:gruvbox_italic = 1
let g:gruvbox_termcolors = 256
let g:gruvbox_invert_signs = 1
let g:gruvbox_invert_selection = 1
let g:gruvbox_improved_strings = 1
let g:gruvbox_improved_warnings = 1

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" search
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
set tags=./tags;,tags;
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
" javascript
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
let g:javascript_plugin_jsdoc = 1
let g:javascript_plugin_ngdoc = 1
let g:javascript_plugin_flow = 1

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" fzf
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"[Buffers] Jump to the existing window if possible
let g:fzf_buffers_jump = 1
"[Tags] Command to generate tags file
"let g:fzf_tags_command = 'ctags --extra=+f -R'
" [Commands] --expect expression for directly executing the command
let g:fzf_commands_expect = 'alt-enter,ctrl-x'
let g:fzf_layout = { 'down': '~60%' }
let g:fzf_colors =
  \ { 'fg':      ['fg', 'Normal'],
    \ 'bg':      ['bg', 'Clear'],
    \ 'hl':      ['fg', 'String'],
    \ 'fg+':     ['fg', 'CursorLine', 'CursorColumn', 'Normal'],
    \ 'bg+':     ['bg', 'CursorLine', 'CursorColumn'],
    \ 'hl+':     ['fg', 'Statement'],
    \ 'info':    ['fg', 'PreProc'],
    \ 'prompt':  ['fg', 'Conditional'],
    \ 'pointer': ['fg', 'Exception'],
    \ 'marker':  ['fg', 'Keyword'],
    \ 'spinner': ['fg', 'Label'],
    \ 'header':  ['fg', 'Comment'] }

nmap <silent> <C-@> :Buffers<CR>
nmap <silent> / :BLines<CR>
nmap <silent> <C-f> :Rg<CR>

" meta key [ ALT ] mappings
nmap <ESC>f :GFiles<CR>
nmap <ESC>c :Commits<CR>


""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" fzf functions
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
command! -bang -nargs=* Rg
  \ call fzf#vim#grep(
  \   'rg --column --line-number --hidden --no-heading --color=always --smart-case '.shellescape(<q-args>), 1,
  \   fzf#vim#with_preview(),
  \   <bang>0)

command! -bang -nargs=? -complete=dir Files
  \ call fzf#vim#files(<q-args>, fzf#vim#with_preview(), <bang>0)

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" gitgutter
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
let g:gitgutter_escape_grep = 1
let g:gitgutter_enabled = 1
let g:gitgutter_signs = 1
let g:gitgutter_highlight_lines = 0
let g:gitgutter_realtime = 1
let g:gitgutter_eager = 1
let g:gitgutter_max_signs = 1500

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" prettier
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
let g:prettier#autoformat = 0
let g:prettier#quickfix_enabled = 0
let g:prettier#exec_cmd_async = 1
let g:prettier#exec_cmd_path = "~/.node/bin/prettier"

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" lsp
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
highlight link LspErrorText GruvboxRedSign
highlight clear LspWarningLine
let g:lsp_signs_enabled = 1
let g:lsp_diagnostics_echo_cursor = 1
let g:lsp_signs_error = {'text': '✗'}
let g:lsp_signs_warning = {'text': '‼'}

if executable('typescript-language-server')
    au User lsp_setup call lsp#register_server({
        \ 'name': 'typescript-language-server',
        \ 'cmd': {server_info->[&shell, &shellcmdflag, 'typescript-language-server --stdio']},
        \ 'root_uri':{server_info->lsp#utils#path_to_uri(lsp#utils#find_nearest_parent_file_directory(lsp#utils#get_buffer_path(), 'tsconfig.json'))},
        \ 'whitelist': ['typescript', 'javascript', 'javascript.jsx']
        \ })
endif

au User lsp_setup call lsp#register_server({
    \ 'name': 'php-language-server',
    \ 'cmd': {server_info->['php', expand('~/.vim/plugged/php-language-server/bin/php-language-server.php')]},
    \ 'whitelist': ['php'],
    \ })

if executable('css-languageserver')
    au User lsp_setup call lsp#register_server({
       \ 'name': 'css-languageserver',
       \ 'cmd': {server_info->[&shell, &shellcmdflag, 'css-languageserver --stdio']},
       \ 'whitelist': ['css', 'less', 'sass'],
       \ })
endif

if executable('pyls')
     au User lsp_setup call lsp#register_server({
        \ 'name': 'pyls',
        \ 'cmd': {server_info->['pyls']},
        \ 'whitelist': ['python'],
        \ })
endif

let g:asyncomplete_remove_duplicates = 1
let g:asyncomplete_auto_popup = 1

au User asyncomplete_setup call asyncomplete#register_source(asyncomplete#sources#file#get_source_options({
    \ 'name': 'file',
    \ 'whitelist': ['*'],
    \ 'priority': 10,
    \ 'completor': function('asyncomplete#sources#file#completor')
    \ }))

call asyncomplete#register_source(asyncomplete#sources#tscompletejob#get_source_options({
    \ 'name': 'tscompletejob',
    \ 'whitelist': ['typescript'],
    \ 'completor': function('asyncomplete#sources#tscompletejob#completor'),
    \ }))

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" misc
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
if has('autocmd')
  au VimEnter,BufNewFile,BufReadPost * silent! call HardMode()
  au VimEnter * AirlineTheme gruvbox

  au BufNewFile,BufRead *.module,*.install set filetype=php

  au BufRead,BufWritePre * if &modifiable | retab | endif

  au BufRead,BufWritePre *.js,*.jsx,*.mjs,*.ts,*.tsx,*.css,*.less,*.scss,*.json,*.graphql,*.md,*.vue,*.yaml,*.html PrettierAsync

  au BufWritePre <buffer> :call setline(1,map(getline(1,"$"),'substitute(v:val,"\\s\\+$","","")'))
  au BufReadPost * if line("'\"") > 1 && line("'\"") <= line("$") | exe "normal! g'\"" | endif

  au FileType php setlocal omnifunc=lsp#complete
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
  " auto close preview window when completion is done
  au! CompleteDone * if pumvisible() == 0 | pclose | endif

endif
