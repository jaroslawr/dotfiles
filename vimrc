" INCLUDES

source ~/.vim/projects.vim
source /usr/share/doc/fzf/examples/fzf.vim

" GENERAL

set incsearch

packadd! onedark.vim
colorscheme onedark

" PROGRAMMING

" syntax highlighting
syntax enable

" auto indent
filetype plugin indent on
set autoindent

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
