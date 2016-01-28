" pathogen runtime manipulation
call pathogen#infect()

" Enable VIM features
set nocompatible

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

" Enhanced command completion
set wildmenu

" Make tab completion complete longest part and show options, THEN cycle
set wildmode=list:longest,full

" Save 50 lines of command-line history
set history=50

" Highlight search results
set hlsearch

" Do incremental searching (best match so far)
set incsearch		

" Only do case-sensitive searching when at least
" one letter is capitalized
set ignorecase
set smartcase

" Allow backspacing over everything in insert mode
set backspace=indent,eol,start

" Do 2 spaces for shift-width and tab-stop
set ts=2
set sw=2

" Do smart indenting
set autoindent
set smartindent

" Expand tabs to spaces.  Pretend tabs exist when using backspace
set expandtab
set smarttab

" Don't insert two spaces after punctuation on a join
set nojoinspaces

" Do not redraw screen while running macros (goes faster)
set lazyredraw

" Improves redrawing for newer computers (apparently)
set ttyfast

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

set guifont=Inconsolata:h12

" use {{{ and }}} to mark folds
set foldmethod=marker

" use the system clipboard
set clipboard=unnamed

" let g:syntastic_tex_checkers=['chktex']
let g:syntastic_tex_checkers=[]
let g:syntastic_markdown_checkers=[]

" Make Syntastic always update the errors window
let g:syntastic_auto_loc_list=1
" let g:syntastic_haskell_checkers=['ghc-mod']
"
" let g:syntastic_rust_rustc_args="-Zno-trans --allow=dead_code --crate-type=lib"

" Load Custom HDevtools Options
" let g:hdevtools_options = ""
" if filereadable(".ghc_options.hdev") 
"   let g:hdevtools_options .= join(readfile(".ghc_options.hdev"), " ")
" endif
" if filereadable(".extensions.hdev")
"   let g:hdevtools_options .= join(readfile(".extensions.hdev"), " ")
" endif

" vim-slime

let g:slime_target = "tmux"
let g:slime_paste_file = "$HOME/.slime_paste"

" ctags for haskell

""" Custom Keys

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

map <silent> <Leader>s :call MySubstitute()<CR>
map <silent> <Leader>S :call MyGlobalSubstitute()<CR>

" NERDTree
map <silent> <Leader>[ :NERDTreeToggle<CR>
map <silent> <Leader>] :TagbarToggle<CR>

" Jumping through (syntastic) error lists

map <silent> <Leader>k       :lfirst<CR>
map <silent> <Leader>j       :llast<CR>
map <silent> <Leader>h       :lprev<CR>
map <silent> <Leader>l       :lnext<CR>
map <silent> <Leader><Space> :ll<CR>

" HDevTools
au FileType haskell nnoremap <buffer>          <Leader>t     :HdevtoolsType<CR>
au FileType haskell nnoremap <buffer> <silent> <Leader><ESC> :HdevtoolsClear<CR>
au FileType haskell nnoremap <buffer> <silent> <Leader>i     :call MyMark()<CR>:HdevtoolsInfo<CR>
function! Get_visual_selection()
  " Why is this not a built-in Vim script function?!
  let [lnum1, col1] = getpos("'<")[1:2]
  let [lnum2, col2] = getpos("'>")[1:2]
  let lines = getline(lnum1, lnum2)
  let lines[-1] = lines[-1][: col2 - (&selection == 'inclusive' ? 1 : 2)]
  let lines[0] = lines[0][col1 - 1:]
  return join(lines, "\n")
endfunction
au FileType haskell  noremap <buffer> <silent> <Leader>te     :call VimuxSendText(Get_visual_selection() . "\n")<CR>

au FileType haskell nnoremap <buffer> <silent> <Leader>t:     :call VimuxRunCommand("./ghci " . @%)<CR>
au FileType haskell nnoremap <buffer> <silent> <Leader>tl     :call VimuxRunCommand(":load " . @%)<CR>
au FileType haskell nnoremap <buffer> <silent> <Leader>tr     :call VimuxSendText(":re\n")<CR>
au FileType haskell nnoremap <buffer> <silent> <Leader>tp     :call VimuxSendText(":pp\n")<CR>
au FileType haskell nnoremap <buffer> <silent> <Leader>tq     :call VimuxSendText(":q\n")<CR>

" Hoogle
" au FileType haskell nnoremap <buffer>          <Leader>/      :Hoogle<Space>

" File Hooks
augroup DD_CUSTOM

  " Removes autocommands for the current group
  autocmd!

  " Remember last position in file
  autocmd BufReadPost * if line("'\"") > 0 && line("'\"") <= line("$") | exe "normal g'\"" | endif

  " Source the vimrc if it gets changed
  autocmd BufWritePost *vimrc if expand("%") == expand("$MYVIMRC") | source % | endif

  " OPL files
  autocmd BufReadPost,BufNewFile *.opl exe "set filetype=operad"

  " Idris files
  autocmd BufReadPost,BufNewFile *.idr exe "set filetype=haskell"

  " Darkdown files
  autocmd BufReadPost,BufNewFile *.dd exe "set filetype=none"

  " R Files
  autocmd BufReadPost,BufNewFile *.r exe "set filetype=r"

  " For java, use different shit stuff
  autocmd BufReadPost *.java call SetJavaOptions()
  function! SetJavaOptions()
	  setlocal sw=4
	  setlocal ts=4 
	  setlocal noexpandtab
    setlocal cinoptions&
    setlocal cinoptions+=:0,(0,j1,W2s,+0,t0
  endfunction

  " For latex files
  autocmd BufReadPost,BufNewFile *.tex exe "set foldmarker={-{,}-}"

augroup END

