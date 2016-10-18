call plug#begin('~/.config/nvim/plugged')

Plug 'scrooloose/nerdtree'
Plug 'parsonsmatt/intero-neovim'

call plug#end()

" Enable filetype detection
filetype plugin indent on

" Enable syntax highlighting
syntax on

" Show the cursor position all the time
set ruler

" Do not keep a backup file; use versions instead; but keeps a backup while
" editing
set nobackup
set writebackup

" Wrap lines that are too long for the screen
set wrap

" Flash matching brackets or parantheses
set showmatch

" Show the mode as well as partially typed commands
set showmode
set showcmd

" Make tab completion complete longest part and show options, THEN cycle
set wildmode=list:longest,full

" Only do case-sensitive searching when at least
" one letter is capitalized
set ignorecase
set smartcase

" Do 2 spaces for shift-width and tab-stop
set tabstop=2
set softtabstop=2
set shiftwidth=2

" Do smart indenting
set smartindent

" Expand tabs to spaces.  Pretend tabs exist when using backspace
set expandtab

" Don't insert two spaces after punctuation on a join
set nojoinspaces

" Do not redraw screen while running macros (goes faster)
set lazyredraw

" Allow opening a new buffer in place of existing one without first saving the
" existing one
set hidden

" Make Y behave like C and D
noremap Y y$

" Keep at least 3 visible lines above and below cursor at all times
set scrolloff=3

" Show a title
set title

" Include all subdirectories in path
set path&
set path+=**

" make vim use a global (user local) backup and temp directory
silent !mkdir -p ~/.vim/backup
silent !mkdir -p ~/.vim/tmp

set backup
set backupdir=~/.vim/backup//

set directory=~/.vim/tmp//

" don't automatically wrap text
set textwidth=0

" C indentation
setlocal cinoptions&
setlocal cinoptions+=:0,(0,j1,W2s,+0,t0

set guifont=Deja_Vu_Sans_Mono:h12

" use {{{ and }}} to mark folds
set foldmethod=marker

" use the system clipboard
set clipboard=unnamed

" allow the use of a mouse
set mouse=a

" Substitution
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

" Marking
function! MyMark()
  exe "normal! mm"
  exe "m'"
  exe "normal! `m"
endfunction

nnoremap <silent> <Leader>s :call MySubstitute()<CR>
nnoremap <silent> <Leader>S :call MyGlobalSubstitute()<CR>

" NERDTree
nnoremap <silent> <Leader>[ :NERDTreeToggle<CR>
nnoremap <silent> <Leader>] :TagbarToggle<CR>

" Jumping through (syntastic) error lists
nnoremap <silent> <Leader>k       :cfirst<CR>
nnoremap <silent> <Leader>j       :clast<CR>
nnoremap <silent> <Leader>h       :cprev<CR>
nnoremap <silent> <Leader>l       :cnext<CR>
nnoremap <silent> <Leader><Space> :cc<CR>

" Intero (Haskell)

" Process management:
au FileType haskell nnoremap <buffer> <Leader>hio :InteroOpen<CR>
au FileType haskell nnoremap <buffer> <Leader>hik :InteroKill<CR>
au FileType haskell nnoremap <buffer> <Leader>hic :InteroHide<CR>
au FileType haskell nnoremap <buffer> <Leader>hil :InteroLoadCurrentModule<CR>

" REPL commands
au FileType haskell nnoremap <buffer> <Leader>hie :InteroEval<CR>
au FileType haskell nnoremap <buffer> <Leader>hit :InteroGenericType<CR>
au FileType haskell nnoremap <buffer> <Leader>hiT :InteroType<CR>
au FileType haskell nnoremap <buffer> <Leader>hii :InteroInfo<CR>
au FileType haskell nnoremap <buffer> <Leader>hiI :InteroTypeInsert<CR>

" Go to definition:
au FileType haskell nnoremap <buffer> <Leader>hid :InteroGoToDef<CR>

" Highlight uses of identifier:
au FileType haskell nnoremap <buffer> <Leader>hiu :InteroUses<CR>

" Reload the file in Intero after saving
autocmd! BufWritePost *.hs InteroReload

function! Get_visual_selection()
  " Why is this not a built-in Vim script function?!
  let [lnum1, col1] = getpos("'<")[1:2]
  let [lnum2, col2] = getpos("'>")[1:2]
  let lines = getline(lnum1, lnum2)
  let lines[-1] = lines[-1][: col2 - (&selection == 'inclusive' ? 1 : 2)]
  let lines[0] = lines[0][col1 - 1:]
  return join(lines, "\n")
endfunction

augroup DD_CUSTOM

  " Removes autocommands for the current group
  autocmd!

  " Remember last position in file
  autocmd BufReadPost * if line("'\"") > 0 && line("'\"") <= line("$") | exe "normal g'\"" | endif

  " Source the vimrc if it gets changed
  autocmd BufWritePost *vimrc if expand("%") == expand("$MYVIMRC") | source % | endif
augroup END

