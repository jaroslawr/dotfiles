" GENERAL

set nocompatible

source ~/.config/nvim/projects.vim
source /usr/share/doc/fzf/examples/fzf.vim

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

" y copies to both X selections
set clipboard=unnamed,unnamedplus

" working directory tracks the opened file
set autochdir
let g:netrw_keepdir=0

" grep with ripgrep
set grepprg=rg\ --no-heading\ -n\ -H\ -F\ --\ '$*'

" APPEARANCE

" theme
packadd! vim-noctu
colorscheme noctu

" theme overrides
augroup vimrc
  autocmd!
  autocmd ColorScheme * hi! VertSplit ctermfg=8
  autocmd ColorScheme * hi! StatusLine ctermbg=236 ctermfg=15
  autocmd ColorScheme * hi! StatusLineNC ctermbg=238 ctermfg=252
  autocmd ColorScheme * hi! User1 ctermfg=003 ctermbg=235 " status line - project
  autocmd ColorScheme * hi! User2 ctermfg=006 ctermbg=235 " status line - file name
augroup END

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
set statusline+=%l " line number
set statusline+=,
set statusline+=%c " column number
set statusline+=\ 
set statusline+=%p " percentage through the file
set statusline+=%% " literal percent

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
autocmd FileType yaml setlocal shiftwidth=2

" KEY BINDINGS

" general
let mapleader = ","
nnoremap <leader>, :bprevious<CR>
nnoremap <leader>. :bnext<CR>
nnoremap <leader>x :qall!<CR>
nnoremap <leader>w :w<CR>

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
