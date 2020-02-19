" GENERAL

set nocompatible

source ~/.config/nvim/projects.vim
source /usr/share/doc/fzf/examples/fzf.vim

" autowrite when changing buffers etc.
set autowrite

" search
set incsearch hlsearch

" completion
set completeopt=menu,menuone,noinsert,noselect

" command line completion
set wildmenu

" open splits below and to the right
set splitbelow splitright

" do not wrap long lines
set nowrap

" keep a margin of visible lines when scrolling
set scrolloff=8

" hide buffers, do not abandon them (e.g. when openining a new buffer)
set hidden

" y copies to both X selections
set clipboard=unnamed,unnamedplus

" enable mouse everywhere
set mouse=a

" grep with ripgrep
set grepprg=rg\ --no-heading\ -n\ -H\ -F\ --\ '$*'

" MARKDOWN

" enable folding
let g:markdown_folding=1

" change how the heading of the fold looks
function! MarkdownFoldText()
  return getline(v:foldstart) . "..."
endfunction
autocmd BufEnter *.md setlocal foldtext=MarkdownFoldText()
autocmd BufEnter *.md setlocal fillchars=fold:\ 

" fold starting from ## header
autocmd BufRead ~/txt/notes.md setlocal foldlevel=1

" APPEARANCE

colorscheme colors

function! StatusProProjectName()
  if ProInProject()
    return " " . ProProjectName() . " "
  else
    return ""
  endif
endfunction

function! StatusProProjectFilePath()
  if ProInProject()
    let l:path = ProProjectFilePath()
    if len(l:path) > 0
      return " " . ProProjectFilePath() . " "
    endif
  endif
  return ""
endfunction

function! StatusFileName()
  if ProInProject()
    return ""
  else
    return " " . expand("%") . " "
  endif
endfunction

" status line
set statusline=\ 
set statusline+=%1*%{StatusProProjectName()}
set statusline+=%2*%{StatusProProjectFilePath()}
set statusline+=%2*%{StatusFileName()}
set statusline+=%*%r " readonly flag
set statusline+=%m " modified flag
set statusline+=\ 
set statusline+=%3*%l " line number
set statusline+=,
set statusline+=%c " column number
set statusline+=\ 
set statusline+=%p " percentage through the file
set statusline+=%% " literal percent

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
autocmd BufEnter * let &titlestring = expand("%:t") . ' '

" highlight current line in active window
augroup CursorLineOnlyInActiveWindow
  autocmd!
  autocmd VimEnter,WinEnter,BufWinEnter * setlocal cursorline
  autocmd WinLeave * setlocal nocursorline
augroup END

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
inoremap jj <Esc>
nnoremap <silent> <C-l> :nohlsearch<CR>
let mapleader = " "
nnoremap <leader>, :bprevious<CR>
nnoremap <leader>. :bnext<CR>
nnoremap <leader>r :source ~/.config/nvim/init.vim<CR>
nnoremap <leader>x :qall<CR>
nnoremap <leader>w :w<CR>
nnoremap <leader>q :wq<CR>

" projects
nnoremap <leader>pp :ProFzfProjects<CR>
nnoremap <leader>pf :ProFzfFilesInProject<CR>
nnoremap <leader>df :ProFzfFilesInWorkingDir<CR>
nnoremap <leader>pr :ProOpenRoot<CR>
nnoremap <leader>pg :ProGrepInProject
nnoremap <leader>dg :ProGrepInWorkingDir
nnoremap <leader>pm :ProMake<CR>
nnoremap <leader>pc :ProEditConfigFile<CR>

" quickfix
nnoremap <leader>cq :cc!<CR>
nnoremap <leader>cp :cprevious!<CR>
nnoremap <leader>cn :cnext!<CR>
nnoremap <leader>co :copen<CR>
nnoremap <leader>cc :cclose<CR>

" git
nnoremap <leader>gb :Gblame<CR>
nnoremap <leader>gl :Glog<CR>
nnoremap <leader>gs :Gstatus<CR>
nnoremap <leader>gc :Gcommit<CR>
