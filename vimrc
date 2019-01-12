" pathogen runtime manipulation
execute pathogen#infect()

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
set cm=blowfish2               " better file encryption

" Make Y behave like C and D
noremap Y y$

" Make .tex files open as "latex" files
let g:tex_flavor = "latex"

" Make Syntastic always update the errors window
" set statusline+=%#warningmsg#
" set statusline+=%{SyntasticStatuslineFlag()}
" set statusline+=%*
    
let g:syntastic_always_populate_loc_list = 1
let g:syntastic_auto_loc_list = 1
let g:syntastic_check_on_open = 1
let g:syntastic_check_on_wq = 0

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

map <silent> <Leader>r :call MySubstitute()<CR>
map <silent> <Leader>R :call MyGlobalSubstitute()<CR>

" NERDTree
map <silent> <Leader>[ :NERDTreeToggle<CR>

" Jumping through (syntastic) error lists
map <silent> <Leader>k       :lfirst<CR>
map <silent> <Leader>j       :llast<CR>
map <silent> <Leader>h       :lprev<CR>
map <silent> <Leader>l       :lnext<CR>
map <silent> <Leader><Space> :ll<CR>

" HDevTools
au FileType haskell nnoremap <buffer> <silent> <Leader>ty      :HdevtoolsType<CR>
au FileType haskell nnoremap <buffer> <silent> <Leader>cl      :HdevtoolsClear<CR>
au FileType haskell nnoremap <buffer> <silent> <Leader>in      :HdevtoolsInfo<CR>

" Tables
let g:table_mode_corner='|'

" File Hooks
augroup DD_CUSTOM

  " Removes autocommands for the current group
  autocmd!

  " Remember last position in file
  autocmd BufReadPost * if line("'\"") > 0 && line("'\"") <= line("$") | exe "normal g'\"" | endif

  " Source the vimrc if it gets changed
  autocmd BufWritePost *vimrc if expand("%") == expand("$MYVIMRC") | source % | endif

  " Use different fold markers for latex
  autocmd BufReadPost *.tex set foldmarker={-{,}-}
augroup END

let g:syntastic_tex_checkers=[]
let g:syntastic_ocaml_checkers = ['merlin']

" ## added by OPAM user-setup for vim / base ## 93ee63e278bdfc07d1139a748ed3fff2 ## you can edit, but keep this line
let s:opam_share_dir = system("opam config var share")
let s:opam_share_dir = substitute(s:opam_share_dir, '[\r\n]*$', '', '')

let s:opam_configuration = {}

function! OpamConfOcpIndent()
  execute "set rtp^=" . s:opam_share_dir . "/ocp-indent/vim"
endfunction
let s:opam_configuration['ocp-indent'] = function('OpamConfOcpIndent')

function! OpamConfOcpIndex()
  execute "set rtp+=" . s:opam_share_dir . "/ocp-index/vim"
endfunction
let s:opam_configuration['ocp-index'] = function('OpamConfOcpIndex')

function! OpamConfMerlin()
  let l:dir = s:opam_share_dir . "/merlin/vim"
  execute "set rtp+=" . l:dir
endfunction
let s:opam_configuration['merlin'] = function('OpamConfMerlin')

let s:opam_packages = ["ocp-indent", "ocp-index", "merlin"]
let s:opam_check_cmdline = ["opam list --installed --short --safe --color=never"] + s:opam_packages
let s:opam_available_tools = split(system(join(s:opam_check_cmdline)))
for tool in s:opam_packages
  " Respect package order (merlin should be after ocp-index)
  if count(s:opam_available_tools, tool) > 0
    call s:opam_configuration[tool]()
  endif
endfor
" ## end of OPAM user-setup addition for vim / base ## keep this line
