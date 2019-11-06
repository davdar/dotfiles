filetype plugin indent on      " Enable filetype detection
syntax on                      " Enable syntax highlighting

set nocompatible               " Enable VIM features
set ruler                      " Show the cursor position all the time
set nobackup                   " / Do not keep a backup file; use versions
set writebackup                " \ instead; but keeps a backup while editing
set wrap                       " Wrap lines that are too long for the screen
set showmatch                  " Flash matching brackets or parantheses
set showmode                   " / Show the mode as well as 
set showcmd                    " \ partially typed commands
set wildmenu                   " Enhanced command completion
set wildmode=list:longest,full " Make tab completion complete longest part and show options, THEN cycle
set history=50                 " Save 50 lines of command-line history
set hlsearch                   " Highlight search results
set incsearch		               " Do incremental searching (best match so far)
set ignorecase                 " / Only do case-sensitive searching when at least
set smartcase                  " \ one letter is capitalized
set backspace=indent,eol,start " Allow backspacing over everything in insert mode
set tabstop=2                  " Do 2 spaces for shift-width and tab-stop
set softtabstop=2
set shiftwidth=2               
set autoindent                 " Do smart indenting
set smartindent
set expandtab                  " Expand tabs to spaces.  Pretend tabs exist when using backspace
set smarttab
set nojoinspaces               " Don't insert two spaces after punctuation on a join
set lazyredraw                 " Do not redraw screen while running macros (goes faster)
set ttyfast                    " Improves redrawing for newer computers (apparently)
set hidden                     " Allow opening a new buffer in place of existing one without first saving the existing one
set scrolloff=3                " Keep at least 3 visible lines above and below cursor at all times
set title                      " Show a title
set path&                      " Include all subdirectories in path
set path+=**
silent !mkdir -p ~/.vim/backup
silent !mkdir -p ~/.vim/tmp
set backup
set backupdir=~/.vim/backup//
set directory=~/.vim/tmp//
set textwidth=0                " don't automatically wrap text
set foldmethod=marker          " use {{{ and }}} to mark folds
set clipboard=unnamed          " use the system clipboard
set mouse=a                    " allow the use of a mouse

" Make Y behave like C and D
noremap Y y$
