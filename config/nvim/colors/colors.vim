set background=dark
hi clear
syntax reset
set t_Co=256
let g:colors_name = "colors"

" text
hi! Constant ctermfg=5
hi! String ctermfg=2
hi! Number ctermfg=3
hi! Identifier ctermfg=15
hi! Function ctermfg=3
hi! Statement ctermfg=4
hi! Keyword ctermfg=1
hi! PreProc ctermfg=6
hi! Include ctermfg=1
hi! Type ctermfg=8
hi! Special ctermfg=1
hi! SpecialChar ctermfg=1
hi! Delimiter ctermfg=7
hi! Comment ctermfg=7 cterm=NONE
hi! Whitespace ctermbg=NONE ctermfg=NONE cterm=NONE

" buffer
hi! Visual ctermbg=11 ctermfg=0
hi! IncSearch ctermbg=9 ctermfg=0 cterm=NONE
hi! Search ctermbg=NONE ctermfg=9
hi! MatchParen ctermbg=NONE ctermfg=9 cterm=NONE
hi! EndOfBuffer ctermfg=240 cterm=NONE

" ui
hi! LineNr ctermfg=246
hi! CursorLine ctermbg=236 cterm=NONE
hi! CursorLineNr ctermbg=236
hi! Msg ctermbg=NONE ctermfg=7 cterm=NONE
hi! ErrorMsg ctermbg=NONE ctermfg=1 cterm=none
hi! WarningMsg ctermbg=NONE ctermfg=1 cterm=none
hi! VertSplit ctermbg=NONE ctermfg=236 cterm=NONE

" ui - status line
hi! StatusLine ctermbg=234 ctermfg=7 cterm=NONE
hi! StatusLineNC ctermbg=234 ctermfg=7 cterm=NONE
hi! User1 ctermbg=234 ctermfg=11 " status line - project
hi! User2 ctermbg=234 ctermfg=12 " status line - file name
hi! User3 ctermbg=234 ctermfg=13 " status line - branch

" ui - tabs
hi! TabLineFill ctermbg=NONE ctermfg=246 cterm=NONE
hi! TabLine ctermbg=NONE ctermfg=246 cterm=NONE
hi! TabLineSel ctermbg=NONE ctermfg=246 cterm=NONE

" ui - popup menu
hi! Pmenu ctermbg=236 ctermfg=15
hi! PmenuSel ctermbg=240 ctermfg=3

" netrw
hi! Directory ctermfg=4
hi! netrwExe ctermfg=1

" quickfix
hi! QuickFixLine ctermbg=236 ctermfg=NONE
hi! qfFileName ctermfg=3
hi! qfError ctermfg=3

" folds
hi! FoldColumn ctermbg=NONE ctermfg=0
hi! Folded ctermbg=NONE ctermfg=11

" markdown
hi! markdownHeadingDelimiter ctermfg=11
hi! markdownH1 ctermfg=11
hi! markdownH2 ctermfg=11
hi! markdownH3 ctermfg=11
hi! markdownH4 ctermfg=11
hi! markdownH5 ctermfg=11
hi! markdownH6 ctermfg=11
hi! markdownUrl ctermfg=4
hi! markdownError ctermfg=1

" python
hi! def link pythonBuiltin Keyword

" javascript
hi! def link javaScriptBraces Normal

" sql
hi! def link sqlKeyword sqlStatement

" xml
hi! def link xmlEndTag xmlTag

" vimscript
hi! def link vimFuncName Keyword

" git
hi! gitKeyword ctermfg=8
hi! gitIdentityKeyword ctermfg=8
hi! gitHash ctermfg=5
hi! gitIdentity ctermfg=15
hi! gitEmail ctermfg=4
hi! gitDate ctermfg=15

" git - commit message header
hi! gitcommitSummary ctermfg=15
hi! gitcommitOverflow ctermfg=15
hi! gitcommitBlank ctermfg=9

" git - commit message body
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

" git - diff
hi! diffSubname ctermbg=NONE ctermfg=7
hi! diffAdded ctermbg=NONE ctermfg=2
hi! diffRemoved ctermbg=NONE ctermfg=1

" git - diff mode
hi! DiffAdd ctermbg=NONE ctermfg=2
hi! DiffDelete ctermbg=NONE ctermfg=1
hi! DiffChange ctermbg=NONE ctermfg=NONE " changed line
hi! DiffText ctermbg=NONE ctermfg=4 " changed text in changed line
