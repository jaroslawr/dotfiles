" GENERAL

set nocompatible

source ~/.vim/projects.vim
source /usr/share/doc/fzf/examples/fzf.vim

packadd! onedark.vim
colorscheme onedark

" search
set incsearch hlsearch

" completion
set wildmenu

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
noremap <leader>q :qall!<CR>
noremap <leader>w :w<CR>

" projects
noremap <leader>pf :call ProFzf()<CR>
noremap <leader>df :call ProDirFzf()<CR>

" git
noremap <leader>gb :Gblame<CR>
noremap <leader>gl :Glog<CR>
noremap <leader>gs :Gstatus<CR>
noremap <leader>gc :Gcommit<CR>
