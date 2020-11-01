" GENERAL

set nocompatible

source ~/.vim/projects.vim
source /usr/share/doc/fzf/examples/fzf.vim

" automatically reload externally changed files
set autoread
augroup autoread
autocmd!
autocmd FocusGained,BufEnter * :checktime
augroup end

" automatically save when compiling, switching buffers etc.
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

" character to use for window border
set fillchars+=vert:│

" do not wrap long lines
set nowrap

" indicate lines that do not fit screen
set list
augroup longlines
autocmd!
autocmd BufEnter * set listchars=precedes:<,extends:>,tab:\ \ 
augroup end

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

augroup fzf
autocmd!
autocmd FileType fzf set laststatus=0
autocmd WinLeave <buffer> set laststatus=2
augroup end

" MARKDOWN

augroup markdown
autocmd!
" formatting
autocmd BufNewFile,BufRead *.md setlocal textwidth=80 shiftwidth=4 tabstop=4 expandtab
" push notes after write
autocmd BufWrite ~/Notes/notes.md Dispatch ntpush
augroup end

" APPEARANCE

colorscheme colors

function! StatusProjectName()
  if ProInProject()
    return "  " . ProProjectName() . " "
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
    return "  " . l:path . (&modified ? "*" : "") . (&readonly ? "#" : "") . " "
  else
    return "  - "
  endif
endfunction

function! StatusBranch()
  let l:branch = fugitive#head()
  if len(l:branch) > 0
    return l:branch . " "
  else
    return ""
  endif
endfunction

" status line
set statusline=%1*%{StatusProjectName()}
set statusline+=%{StatusBranch()}%0*
set statusline+=%{StatusFileName()}
set statusline+=\ 
set statusline+=%< " when cutting, start here
set statusline+=%=
set statusline+=%1*\ 
set statusline+=0x%02B\ 
set statusline+=%03l " line number
set statusline+=,
set statusline+=%03c " column number
set statusline+=\ 
set statusline+=%02p " percentage through the file
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

augroup title
autocmd!
autocmd VimEnter,WinEnter,BufEnter * let &titlestring = Title()
augroup end

" highlight current line only in active window
augroup currentline
autocmd!
autocmd VimEnter,WinEnter,BufWinEnter * setlocal cursorline
autocmd WinLeave * setlocal nocursorline
augroup end

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
augroup formatting
autocmd!
autocmd FileType go setlocal shiftwidth=8 noexpandtab
autocmd FileType vim setlocal shiftwidth=2
autocmd FileType yaml setlocal shiftwidth=2
augroup end

" vim-dispatch compiler settings
let g:dispatch_compilers = { 'python3': 'python' }

" KEY BINDINGS

" general
nnoremap <silent> <C-l> :nohlsearch<CR>

" buffers
nnoremap ; :bprevious<CR>
nnoremap ' :bnext<CR>

" quickfix
nnoremap , :cprevious!<CR>
nnoremap . :cnext!<CR>
nnoremap / :copen<CR>:cc<CR>

" leader - general
inoremap jj <Esc>
let mapleader = " "
nnoremap <leader>b :bdelete!<CR>
nnoremap <leader>x :qall!<CR>
nnoremap <leader>w :w<CR>

" leader - clipboard
xnoremap <leader>y y:call system("wl-copy", @")<cr>
nnoremap <leader>p :let @"=substitute(system("wl-paste --no-newline"), '<C-v><C-m>', '', 'g')<cr>p

" leader - folding
nnoremap <leader><leader> za

" leader - config
nnoremap <leader>ce :edit ~/.vimrc<CR>
nnoremap <leader>cr :source ~/.vimrc<CR>

" leader - fuzzy find
nnoremap <leader>fp :ProFzfProjects<CR>
nnoremap <leader>ff :Files<CR>
nnoremap <leader>fb :Buffers<CR>

" leader - make
nnoremap <leader>m :ProMake<CR>
