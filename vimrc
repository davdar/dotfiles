" pathogen runtime manipulation
execute pathogen#infect()

" old stuff taken out:
"
" set path&                      " Include all subdirectories in path
" set path+=**

" if not neovim, match neovim defaults
if !has('nvim')
  silent !mkdir -p ~/.local/share/vim/backup
  silent !mkdir -p ~/.local/share/vim/swap
  silent !mkdir -p ~/.local/share/vim/undo

  syntax on                      " Enable syntax highlighting
  filetype plugin indent on      " Enable filetype detection
  set autoindent                 " Do smart indenting
  set autoread                   " Automatically reload files that change outside vim
  set background=dark
  set backspace=indent,eol,start " Allow backspacing over everything in insert mode
  set backupdir=~/.local/share/vim/backup
  set belloff=all                " Never ring the bell
  set nocompatible               " Enable VIM features
  set complete-=i                " Eclude scanning included files for completions
  set cscopeverbose
  set directory=~/.local/share/vim/swap//
  "set display=lastline,msgsep
  set encoding=utf-8
  "set fillchars=vert:|,fold:·,sep:|
  set formatoptions=tcqj
  set nofsync
  set history=10000
  set hlsearch
  set incsearch
  set langnoremap
  set nolangremap
  set laststatus=2
  "set listchars=tab:>,trail:-,nbsp:+
  set nrformats=bin,hex
  set ruler                      " Show the cursor position all the time
  set sessionoptions-=options
  set shortmess+=F
  set shortmess-=S
  set showcmd                    " Show partially typed commands
  set sidescroll=1
  set smarttab
  set nostartofline
  set tabpagemax=50
  set tags=./tags;,tags
  set ttimeoutlen=50
  set ttyfast
  set undodir=~/.local/share/vim/undo
  set viminfo+=!
  set wildmenu
  "set wildoptions=pum,tagfile

  "not included: man.vim and matchit plugins
endif

" neovim doesn't support encryption
if !has('nvim')
  set cm=blowfish2               " better file encryption
endif

set background=light
set fsync
set showmatch                  " Flash matching brackets or parantheses
set ignorecase                 " / Only do case-sensitive searching when at least
set smartcase                  " \ one letter is capitalized
set tabstop=4                  " tab size = 4
set softtabstop=2              " Inserting a tab inserts 2 spaces
set shiftwidth=2               " Indent by 2 spaces
set smartindent                " Autoindent when starting a new line
set expandtab                  " Expand tabs to spaces when inserting
set nojoinspaces               " Don't insert two spaces after punctuation on a join
set hidden                     " Allow opening new buffer without first saving
set scrolloff=3                " Keep 3 visible lines above and below cursor
set title                      " Show a title
set foldmethod=marker          " use {{{ and }}} to mark folds
set clipboard=unnamed          " use the system clipboard
set mouse=a                    " allow the use of a mouse
set cmdheight=2

" suggested by coc

set updatetime=300
set shortmess+=c
" set signcolumn=yes

" bug in Vim?

set t_ZH=[3m
set t_ZR=[23m

" Make Y behave like C and D
noremap Y y$

" -----------------------
" -- EASY TEXT REPLACE --
" -----------------------

function! MySubstitute()
  exe "normal! mm"
  let replacement = input("Replace With?: ")
  exe "%s//" . replacement . "/g"
  exe "normal! `m"
endfunction

function! FilesWithPatInDir(d)
  exe "normal! mm"
  exe ":cd " . a:d
  let cmd = " grep -ls \"" . @/ . "\" *"
  " echo cmd
  let files = system(cmd)
  exe ":cd -"
  " echo files
  return files
endfunction

function! RecFilesWithPatInDir(d)
  let cmd = "grep -lr \"" . @/ . "\" " . a:d
  " echo cmd
  let files = system(cmd)
  " echo files
  return files
endfunction

function! MyGlobalSubstitute()
    exe "normal! mm"
    let dir = input("Directory?: ")
    let r = input("Recursive? (y/n): ")
    if r == "y"
      let files = RecFilesWithPatInDir(dir)
    else
      let files = FilesWithPatInDir(dir)
    endif
    echo "\n" . files
    let okay = input("Files Okay? (y/n): ")
    if okay == "y"
      let replacement = input("Replace With?: ")
      exe ":args " . substitute(files,"\\n"," ","g")
      exe ":argdo %s/" . @/ . "/" . replacement . "/g"
    endif
endfunction

map <silent> <Leader>r :call MySubstitute()<CR>
map <silent> <Leader>R :call MyGlobalSubstitute()<CR>

" -----------
" -- LATEX --
" -----------

" Make .tex files open as "latex" files
let g:tex_flavor = "latex"

" -------------
" -- HASKELL --
" -------------

let g:haskell_enable_quantification = 1   " to enable highlighting of `forall`
let g:haskell_enable_recursivedo = 1      " to enable highlighting of `mdo` and `rec`
let g:haskell_enable_arrowsyntax = 1      " to enable highlighting of `proc`
let g:haskell_enable_pattern_synonyms = 1 " to enable highlighting of `pattern`
let g:haskell_enable_typeroles = 1        " to enable highlighting of type roles
let g:haskell_enable_static_pointers = 1  " to enable highlighting of `static`
let g:haskell_backpack = 1                " to enable highlighting of backpack keywords

let g:haskell_classic_highlighting = 1    " classic highlighting colors

" ---------------
" -- NERD TREE --
" ---------------

" NERDTree
map <silent> <Leader>[ :NERDTreeToggle<CR>

" -----------------
" -- ERROR LISTS --
" -----------------

" Jumping through error lists
" map <silent> <Leader>ek       :lfirst<CR>
" map <silent> <Leader>ej       :llast<CR>
" map <silent> <Leader>eh       :lprev<CR>
" map <silent> <Leader>el       :lnext<CR>

" map <silent> <Leader>ek       :cfirst<CR>
" map <silent> <Leader>ej       :clast<CR>
" map <silent> <Leader>eh       :cprev<CR>
" map <silent> <Leader>el       :cnext<CR>

" -------------
" -- DIFFING --
" -------------

map <silent> <Leader>dj       :diffthis<CR>
map <silent> <Leader>dk       :diffoff<CR>

" ---------
" -- COC --
" ---------

highlight link CoCFloating Visual

nmap <silent> <Leader>eh <Plug>(coc-diagnostic-prev)
nmap <silent> <Leader>el <Plug>(coc-diagnostic-next)

nmap <silent> <Leader>ed <Plug>(coc-definition)
nmap <silent> <Leader>et <Plug>(coc-type-definition)
nmap <silent> <Leader>ei <Plug>(coc-implementation)
nmap <silent> <Leader>er <Plug>(coc-references)

autocmd CursorHold * silent call CocActionAsync('highlight')

" --------------------
" -- LanguageClient --
" -------------------

" let g:LanguageClient_rootMarkers = ['*.cabal', 'stack.yaml']
" let g:LanguageClient_serverCommands = {
"     \ 'rust': ['rls'],
"     \ 'haskell': ['ghcide', '--lsp'],
"     \ }

" DONT DO THESE
" let g:LanguageClient_diagnosticsList = "Location"
" let g:LanguageClient_useVirtualText = "Diagnostics"

" ---------
" -- LSP --
" ---------

" au User lsp_setup call lsp#register_server({
"     \ 'name': 'ghcide',
"     \ 'cmd': {server_info->['/Users/daviddarais/.local/bin/ghcide', '--lsp']},
"     \ 'whitelist': ['haskell'],
"     \ })
" 
" " let g:lsp_diagnostics_echo_cursor = 1
" let g:lsp_diagnostics_float_cursor = 1
" " let g:lsp_text_document_did_save_delay = 100
" 
" map <silent> <Leader>eh       :LspPreviousError<CR>
" map <silent> <Leader>el       :LspNextError<CR>
" 
" map <silent> <Leader>wh       :LspPreviousWarning<CR>
" map <silent> <Leader>wl       :LspNextWarning<CR>

" ---------
" -- ALE --
" ---------

" let g:ale_linters_explicit = 1
" let g:ale_linters = { 'haskell': ['ghcide'] }
" let g:ale_set_quickfix = 1
" let g:ale_set_highlights = 1
" let g:ale_cursor_detail = 1
" let g:ale_echo_cursor = 0

" set splitbelow

" ---------
" -- LSC --
" ---------

" let g:lsc_server_commands = 
"       \ { 'haskell': 
"       \     { 'command': 'ghcide'
"       \     , 'suppress_stderr': 'v:true'
"       \     }
"       \ }

" ----------------
" -- TABLE MODE --
" ----------------

" let g:table_mode_corner='|'

" ----------------
" -- TABULARIZE --
" ----------------

map <Leader>t :Tabularize /

" File Hooks
augroup DD_CUSTOM

  " Removes autocommands for the current group
  autocmd!

  " Remember last position in file
  autocmd BufReadPost * if line("'\"") > 0 && line("'\"") <= line("$") | exe "normal g'\"" | endif

  " Resync syntax highlighting
  autocmd BufEnter * :syntax sync fromstart

  " Source the vimrc if it gets changed
  autocmd BufWritePost *vimrc if expand("%") == expand("$MYVIMRC") | source % | endif

  " Use different fold markers for latex
  " autocmd BufReadPost *.tex set foldmarker={-{,}-}
augroup END
