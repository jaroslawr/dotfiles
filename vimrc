" GENERAL

set nocompatible

source ~/.vim/projects.vim
source /usr/share/doc/fzf/examples/fzf.vim

" theme
packadd! vim-noctu
colorscheme noctu

" keep the working directory clean
set backupdir=~/.vim/.backup
set undodir=~/.vim/.undo
set directory=~/.vim/.swap

" search
set incsearch hlsearch

" completion
set completeopt=menu,menuone,noinsert,noselect

" command line completion
set wildmenu

" keep a margin of visible lines when scrolling
set scrolloff=8

" hide buffers, do not abandon them (e.g. when openining a new buffer)
set hidden

" working directory tracks the opened file
set autochdir
let g:netrw_keepdir=0

" grep with ripgrep
set grepprg=rg\ --no-heading\ -n\ -H\ -F\ --\ '$*'

" fix pasting in tmux
if &term =~ '^screen'
  let &t_BE="\<Esc>[?2004h"
  let &t_BD="\<Esc>[?2004l"
  let &t_PS="\<Esc>[200~"
  let &t_PE="\<Esc>[201~"
endif

" APPEARANCE

" buffers need some left padding
set foldcolumn=1

" always show the status line
set laststatus=2

" show the pending command in bottom right corner of the screen
set showcmd

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

" KEY BINDINGS

" general
let mapleader = ","
nnoremap <leader>x :qall!<CR>
nnoremap <leader>w :w<CR>

" projects
nnoremap <leader>pp :ProFzfProjects<CR>
nnoremap <leader>pf :ProFzfFilesInProject<CR>
nnoremap <leader>df :ProFzfFilesInWorkingDir<CR>
nnoremap <leader>pg :ProGrepInProject 
nnoremap <leader>dg :ProGrepInWorkingDir 
nnoremap <leader>pm :ProMake<CR>
nnoremap <leader>pc :ProEditConfigFile<CR>

" quickfix
nnoremap <leader>qp :cprevious!<CR>
nnoremap <leader>qn :cnext!<CR>
nnoremap <leader>qf :cfirst<CR>
nnoremap <leader>ql :clast!<CR>
nnoremap <leader>qo :copen<CR>
nnoremap <leader>qc :cclose<CR>

" git
nnoremap <leader>gb :Gblame<CR>
nnoremap <leader>gl :Glog<CR>
nnoremap <leader>gs :Gstatus<CR>
nnoremap <leader>gc :Gcommit<CR>
