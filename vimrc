" GENERAL

set nocompatible

source ~/.vim/projects.vim
source /usr/share/doc/fzf/examples/fzf.vim

" theme
packadd! vim-noctu
colorscheme noctu

" search
set incsearch hlsearch

" completion
set completeopt=menu,menuone,noinsert,noselect

" command line completion
set wildmenu

" working directory tracks the opened file
set autochdir
let g:netrw_keepdir=0

" grep with ripgrep
set grepprg=rg\ --no-heading\ -n\ -H\ -F\ --\ '$*'

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

" KEY BINDINGS

" general
let mapleader = ","
noremap <leader>x :qall!<CR>
noremap <leader>w :w<CR>

" projects
noremap <leader>pf :call ProFzf()<CR>
noremap <leader>df :call ProDirFzf()<CR>
noremap <leader>pg :ProGrep 
noremap <leader>dg :ProDirGrep 
noremap <leader>pm :call ProMake()<CR>
noremap <leader>pc :call ProEditConfigFile()<CR>

" quickfix
noremap <leader>qp :cprevious!<CR>
noremap <leader>qn :cnext!<CR>
noremap <leader>qf :cfirst<CR>
noremap <leader>ql :clast!<CR>
noremap <leader>qo :copen<CR>
noremap <leader>qc :cclose<CR>

" git
noremap <leader>gb :Gblame<CR>
noremap <leader>gl :Glog<CR>
noremap <leader>gs :Gstatus<CR>
noremap <leader>gc :Gcommit<CR>
