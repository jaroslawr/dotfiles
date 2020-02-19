set background=dark
hi clear
syntax reset
set t_Co=256
let g:colors_name = "colors"

" text
hi! Type ctermfg=250
hi! Statement ctermfg=12
hi! Number ctermfg=1
hi! String ctermfg=3
hi! Keyword ctermfg=6
hi! Function ctermfg=8
hi! PreProc ctermfg=11
hi! Identifier ctermfg=10
hi! Constant ctermfg=14
hi! Comment ctermfg=246 cterm=NONE

" buffer
hi! Search ctermbg=6
hi! Visual ctermbg=3 ctermfg=0
hi! LineNr ctermfg=246
hi! EndOfBuffer ctermfg=240 cterm=NONE
hi! CursorLine ctermbg=238 cterm=NONE
hi! CursorLineNr ctermbg=238

" ui
hi! ErrorMsg ctermbg=NONE ctermfg=1 cterm=none
hi! WarningMsg ctermbg=NONE ctermfg=1 cterm=none
hi! VertSplit ctermbg=NONE ctermfg=236 cterm=NONE

" ui - status line
hi! StatusLine ctermbg=236 ctermfg=15 cterm=NONE
hi! StatusLineNC ctermbg=236 ctermfg=252 cterm=NONE
hi! User1 ctermbg=236 ctermfg=3 " status line - project
hi! User2 ctermbg=236 ctermfg=4 " status line - file name
hi! User3 ctermbg=236 ctermfg=246 " status line - file name

" ui - tabs
hi! TabLineFill ctermbg=NONE ctermfg=246 cterm=NONE
hi! TabLine ctermbg=NONE ctermfg=246 cterm=NONE
hi! TabLineSel ctermbg=NONE ctermfg=246 cterm=NONE

" ui - popup menu
hi! Pmenu ctermbg=236 ctermfg=15
hi! PmenuSel ctermbg=240 ctermfg=3

" quickfix
hi! QuickFixLine ctermbg=238 ctermfg=NONE
hi! qfError ctermfg=9

" folds
hi! FoldColumn ctermbg=0 ctermfg=0
hi! Folded ctermbg=NONE ctermfg=3

" markdown
hi! markdownHeadingDelimiter ctermfg=3
hi! markdownH1 ctermfg=3
hi! markdownH2 ctermfg=3
hi! markdownH3 ctermfg=3
hi! markdownH4 ctermfg=3
hi! markdownH5 ctermfg=3
hi! markdownH6 ctermfg=3
hi! markdownUrl ctermfg=6
hi! markdownError ctermfg=1
