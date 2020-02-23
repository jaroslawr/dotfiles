set background=dark
hi clear
syntax reset
set t_Co=256
let g:colors_name = "colors"

" text
hi! Constant ctermfg=5
hi! String ctermfg=2
hi! Number ctermfg=3
hi! Identifier ctermfg=1
hi! Function ctermfg=3
hi! Statement ctermfg=6
hi! Keyword ctermfg=1
hi! PreProc ctermfg=4
hi! Include ctermfg=1
hi! Type ctermfg=8
hi! Special ctermfg=1
hi! SpecialChar ctermfg=1
hi! Delimiter ctermfg=7
hi! Comment ctermfg=7 cterm=NONE
hi! Whitespace ctermbg=NONE ctermfg=NONE cterm=NONE

" buffer
hi! IncSearch ctermbg=3 ctermfg=0 cterm=NONE
hi! Search ctermbg=2 ctermfg=0
hi! Visual ctermbg=3 ctermfg=0
hi! MatchParen ctermbg=NONE ctermfg=9 cterm=NONE
hi! EndOfBuffer ctermfg=240 cterm=NONE
hi! Directory ctermfg=6

" ui
hi! LineNr ctermfg=246
hi! CursorLine ctermbg=235 cterm=NONE
hi! CursorLineNr ctermbg=235
hi! Msg ctermbg=NONE ctermfg=7 cterm=NONE
hi! ErrorMsg ctermbg=NONE ctermfg=1 cterm=none
hi! WarningMsg ctermbg=NONE ctermfg=1 cterm=none
hi! VertSplit ctermbg=NONE ctermfg=236 cterm=NONE

" ui - status line
hi! StatusLine ctermbg=NONE ctermfg=7 cterm=NONE
hi! StatusLineNC ctermbg=NONE ctermfg=7 cterm=NONE
hi! User1 ctermbg=NONE ctermfg=11 " status line - project
hi! User2 ctermbg=NONE ctermfg=10 " status line - file name
hi! User3 ctermbg=NONE ctermfg=9 " status line - branch

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
hi! FoldColumn ctermbg=NONE ctermfg=0
hi! Folded ctermbg=NONE ctermfg=3

" markdown
hi! markdownHeadingDelimiter ctermfg=3
hi! markdownH1 ctermfg=3
hi! markdownH2 ctermfg=3
hi! markdownH3 ctermfg=3
hi! markdownH4 ctermfg=3
hi! markdownH5 ctermfg=3
hi! markdownH6 ctermfg=3
hi! markdownUrl ctermfg=12
hi! markdownError ctermfg=1

" python
hi! def link pythonBuiltin Keyword

" javascript
hi! def link javaScriptBraces Normal

" sql
hi! def link sqlKeyword sqlStatement

" diff
hi! DiffAdd ctermbg=NONE ctermfg=10
hi! DiffDelete ctermbg=NONE ctermfg=9
hi! DiffChange ctermbg=NONE ctermfg=11 " changed line
hi! DiffText ctermbg=8 ctermfg=11 " changed text in changed line
hi! DiffDelete ctermbg=NONE ctermfg=8

" vimscript
hi! def link vimFuncName Keyword

" git
hi! gitHash ctermfg=13

" git - commit
hi! gitcommitComment ctermfg=7
hi! gitcommitHeader ctermfg=7
hi! gitcommitSelected ctermfg=7
hi! gitcommitDiscarded ctermfg=7
hi! gitcommitUntracked ctermfg=7
hi! gitcommitSelectedType ctermfg=2
hi! gitcommitSelectedFile ctermfg=2
hi! gitcommitDiscardedType ctermfg=6
hi! gitcommitDiscardedFile ctermfg=6
hi! gitcommitUntrackedType ctermfg=7
hi! gitcommitUntrackedFile ctermfg=7

" git - fugitive
hi! fugitiveSymbolicRef ctermfg=9
