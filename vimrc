" INCLUDES

source ~/.vim/projects.vim
source /usr/share/doc/fzf/examples/fzf.vim

" GENERAL

set incsearch

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

" projects
noremap <leader>pf :call ProFzf()<CR>
noremap <leader>df :call ProDirFzf()<CR>
