" GENERAL

set nocompatible

source ~/.vim/projects.vim
source /usr/share/doc/fzf/examples/fzf.vim

" refresh externally changed files
autocmd FocusGained,BufEnter * :checktime

" autowrite when changing buffers etc.
set autowrite

" search
set incsearch hlsearch

" completion
set completeopt=menu,menuone,noinsert,noselect

" command line completion
set wildmenu

" no popup completion menu in command line
set wildoptions=tagfile

" open splits below and to the right
set splitbelow splitright

" do not wrap long lines
set nowrap

" indicate lines that do not fit screen
set list
autocmd BufEnter * set listchars=precedes:<,extends:>,tab:\ \ 

" keep a margin of visible lines when scrolling
set scrolloff=8

" hide buffers, do not abandon them (e.g. when openining a new buffer)
set hidden

" y copies to both X selections
set clipboard=unnamed,unnamedplus

" enable mouse everywhere
set mouse=a

" grep with ripgrep
set grepprg=rg\ -n\ $*

" command line abbreviation for current buffers working dir
cabbr <expr> %% expand('%:h')

" FZF

autocmd FileType fzf set laststatus=0 | autocmd WinLeave <buffer> set laststatus=2

" MARKDOWN

" enable folding
let g:markdown_folding=1

" change how the heading of the fold looks
function! MarkdownFoldText()
  return getline(v:foldstart) . "..."
endfunction
autocmd BufEnter *.md setlocal foldtext=MarkdownFoldText() fillchars=fold:\ 

" start normal markdown with open folds
autocmd BufRead *.md setlocal foldlevel=100

" formatting
autocmd BufNewFile,BufRead *.md setlocal textwidth=80

" start notes.md with closed folds
autocmd BufRead ~/txt/notes.md setlocal foldlevel=1

" APPEARANCE

colorscheme colors

function! StatusProjectName()
  if ProInProject()
    return ProProjectName() . " "
  else
    return ""
  endif
endfunction

function! StatusFileName()
  if ProInProject()
    let l:path = ProProjectFilePath()
  else
    let l:path = expand("%")
  endif

  if len(l:path) > 0
    return l:path . (&modified ? "*" : "") . (&readonly ? "#" : "") . " "
  else
    return "- "
  endif
endfunction

function! StatusBranch()
  return fugitive#head()
endfunction

" status line
set statusline=\ 
set statusline+=%1*%{StatusProjectName()}%0*
set statusline+=%2*%{StatusFileName()}%0*
set statusline+=%3*%{StatusBranch()}%0*
set statusline+=\ 
set statusline+=%< " when cutting, start here
set statusline+=%=
set statusline+=0x%B
set statusline+=\ 
set statusline+=%l " line number
set statusline+=,
set statusline+=%c " column number
set statusline+=\ 
set statusline+=%p " percentage through the file
set statusline+=%% " literal percent
set statusline+=\ %{''.(&fenc!=''?&fenc:&enc).''}
set statusline+=\ %{&ff}
set statusline+=\ 

" show line numbers
set number

" buffers need some left padding
set foldcolumn=1

" always show the status line
set laststatus=2

" show the pending command in bottom right corner of the screen
set showcmd

" set the tmux title to current file name
set t_ts=]2;
set t_fs=\\
set title

function Title()
  let l:path = expand("%:t")
  if len(l:path) > 0
    return l:path
  else
    return fnamemodify(getcwd(), ":t")
  endif
endfunction
autocmd VimEnter,WinEnter,BufEnter * let &titlestring = Title()

" highlight current line in active window
augroup CursorLineOnlyInActiveWindow
  autocmd!
  autocmd VimEnter,WinEnter,BufWinEnter * setlocal cursorline
  autocmd WinLeave * setlocal nocursorline
augroup END

" change cursor depending on mode
let &t_SI = "\<Esc>[6 q"
let &t_SR = "\<Esc>[4 q"
let &t_EI = "\<Esc>[2 q"

" PROGRAMMING

" syntax highlighting
syntax enable

" auto indent
filetype plugin indent on
set autoindent

" default indentation settings
set shiftwidth=4 expandtab

" filetype-specific default indentation settings
autocmd FileType vim setlocal shiftwidth=2
autocmd FileType yaml setlocal shiftwidth=2

" vim-dispatch compiler settings
let g:dispatch_compilers = { 'python3': 'python' }

" KEY BINDINGS

" general
nnoremap <silent> <C-l> :nohlsearch<CR>

" buffers
nnoremap <M-;> :bprevious<CR>
nnoremap <M-'> :bnext<CR>

" quickfix
nnoremap <M-,> :cprevious!<CR>
nnoremap <M-.> :cnext!<CR>
nnoremap <M-/> :copen<CR>:cc<CR>

" leader - general
inoremap jj <Esc>
let mapleader = " "
nnoremap <leader>x :qall<CR>
nnoremap <leader>w :w<CR>
nnoremap <leader>q :wq<CR>

" leader - config
nnoremap <leader>ce :edit ~/.vimrc<CR>
nnoremap <leader>cr :source ~/.vimrc<CR>

" leader - make
nnoremap <leader>mm :make<CR>
nnoremap <leader>md :Dispatch<CR>

" leader - projects
nnoremap <leader>pp :ProFzfProjects<CR>
nnoremap <leader>pf :ProFzfFilesInProject<CR>
nnoremap <leader>df :ProFzfFilesInWorkingDir<CR>
nnoremap <leader>pr :ProOpenRoot<CR>
nnoremap <leader>pg :ProGrepInProject
nnoremap <leader>dg :ProGrepInWorkingDir
nnoremap <leader>pm :ProMake<CR>
nnoremap <leader>pc :ProEditConfigFile<CR>

" leader - git
nnoremap <leader>gb :Gblame<CR>
nnoremap <leader>gl :Glog<CR>
nnoremap <leader>gs :Gstatus<CR>
nnoremap <leader>gc :Gcommit<CR>

" python - jedi
" jedi will conflict with leader mappings if this is not set
let g:jedi#goto_command = "<leader>jg"
let g:jedi#documentation_command = "<leader>jd"
let g:jedi#usages_command = "<leader>ju"
let g:jedi#completions_command = "<C-Space>"
let g:jedi#goto_assignments_command = ""
let g:jedi#goto_stubs_command = ""
let g:jedi#goto_definitions_command = ""
let g:jedi#rename_command = ""

