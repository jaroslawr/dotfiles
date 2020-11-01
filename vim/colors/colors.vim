set background=dark
hi clear
syntax reset
set t_Co=256
let g:colors_name = "colors"

" text
hi! Constant ctermfg=6 cterm=NONE
hi! String ctermfg=5 cterm=NONE
hi! Number ctermfg=3 cterm=NONE
hi! Identifier ctermfg=15 cterm=NONE
hi! Function ctermfg=3 cterm=NONE
hi! Statement ctermfg=4 cterm=NONE
hi! Keyword ctermfg=1 cterm=NONE
hi! PreProc ctermfg=1 cterm=NONE
hi! Include ctermfg=1 cterm=NONE
hi! Type ctermfg=6 cterm=NONE
hi! Special ctermfg=1 cterm=NONE
hi! SpecialChar ctermfg=1 cterm=NONE
hi! Delimiter ctermfg=7 cterm=NONE
hi! Comment ctermfg=7 cterm=NONE
hi! Whitespace ctermbg=NONE ctermfg=NONE cterm=NONE
hi! Todo ctermbg=NONE ctermfg=3 cterm=NONE

" buffer
hi! Visual ctermbg=11 ctermfg=0 cterm=NONE
hi! IncSearch ctermbg=9 ctermfg=0 cterm=NONE
hi! Search ctermbg=NONE ctermfg=9 cterm=NONE
hi! MatchParen ctermbg=NONE ctermfg=9 cterm=NONE
hi! EndOfBuffer ctermfg=240 cterm=NONE

" ui
hi! LineNr ctermfg=240 cterm=NONE
hi! CursorLine ctermbg=236 cterm=NONE
hi! CursorLineNr ctermbg=236 cterm=NONE
hi! Msg ctermbg=NONE ctermfg=7 cterm=NONE
hi! ErrorMsg ctermbg=NONE ctermfg=1 cterm=NONE
hi! WarningMsg ctermbg=NONE ctermfg=1 cterm=NONE
hi! VertSplit ctermbg=NONE ctermfg=236 cterm=NONE

" ui - status line
hi! StatusLine ctermbg=235 ctermfg=3 cterm=NONE
hi! StatusLineNC ctermbg=235 ctermfg=7 cterm=NONE
hi! User1 ctermbg=236 ctermfg=5 cterm=NONE " status line - project

" ui - tabs
hi! TabLineFill ctermbg=NONE ctermfg=246 cterm=NONE
hi! TabLine ctermbg=NONE ctermfg=246 cterm=NONE
hi! TabLineSel ctermbg=NONE ctermfg=246 cterm=NONE

" ui - popup menu
hi! Pmenu ctermbg=236 ctermfg=15 cterm=NONE
hi! PmenuSel ctermbg=240 ctermfg=3 cterm=NONE

" netrw
hi! Directory ctermfg=4 cterm=NONE
hi! netrwExe ctermfg=1 cterm=NONE

" quickfix
hi! QuickFixLine ctermbg=236 ctermfg=NONE cterm=NONE
hi! qfFileName ctermfg=3 cterm=NONE
hi! qfError ctermfg=3 cterm=NONE

" folds
hi! FoldColumn ctermbg=NONE ctermfg=0 cterm=NONE
hi! Folded ctermbg=NONE ctermfg=11 cterm=NONE

" markdown
hi! markdownH1 ctermfg=11 cterm=NONE
hi! markdownH1Delimiter ctermfg=11 cterm=NONE
hi! markdownH2 ctermfg=13 cterm=NONE
hi! markdownH2Delimiter ctermfg=13 cterm=NONE
hi! markdownH3 ctermfg=14 cterm=NONE
hi! markdownH3Delimiter ctermfg=14 cterm=NONE
hi! markdownH4 ctermfg=12 cterm=NONE
hi! markdownH4Delimiter ctermfg=12 cterm=NONE
hi! markdownH5 ctermfg=8 cterm=NONE
hi! markdownH5Delimiter ctermfg=8 cterm=NONE
hi! markdownH6 ctermfg=8 cterm=NONE
hi! markdownH6Delimiter ctermfg=8 cterm=NONE
hi! markdownItalic ctermfg=8 cterm=NONE
hi! markdownUrl ctermfg=4 cterm=NONE
hi! markdownError ctermfg=1 cterm=NONE

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
hi! gitKeyword ctermfg=8 cterm=NONE
hi! gitIdentityKeyword ctermfg=8 cterm=NONE
hi! gitHash ctermfg=5 cterm=NONE
hi! gitIdentity ctermfg=15 cterm=NONE
hi! gitEmail ctermfg=4 cterm=NONE
hi! gitDate ctermfg=15 cterm=NONE

" git - commit message header
hi! gitcommitSummary ctermfg=15 cterm=NONE
hi! gitcommitOverflow ctermfg=15 cterm=NONE
hi! gitcommitBlank ctermfg=9 cterm=NONE

" git - commit message body
hi! gitcommitComment ctermfg=7 cterm=NONE
hi! gitcommitHeader ctermfg=7 cterm=NONE
hi! gitcommitSelected ctermfg=7 cterm=NONE
hi! gitcommitDiscarded ctermfg=7 cterm=NONE
hi! gitcommitUntracked ctermfg=7 cterm=NONE
hi! gitcommitSelectedType ctermfg=2 cterm=NONE
hi! gitcommitSelectedFile ctermfg=2 cterm=NONE
hi! gitcommitDiscardedType ctermfg=6 cterm=NONE
hi! gitcommitDiscardedFile ctermfg=6 cterm=NONE
hi! gitcommitUntrackedType ctermfg=7 cterm=NONE
hi! gitcommitUntrackedFile ctermfg=7 cterm=NONE

" git - fugitive
hi! fugitiveSymbolicRef ctermfg=9 cterm=NONE

" git - diff
hi! diffSubname ctermbg=NONE ctermfg=7 cterm=NONE
hi! diffAdded ctermbg=NONE ctermfg=2 cterm=NONE
hi! diffRemoved ctermbg=NONE ctermfg=1 cterm=NONE

" git - diff mode
hi! DiffAdd ctermbg=NONE ctermfg=2 cterm=NONE
hi! DiffDelete ctermbg=NONE ctermfg=1 cterm=NONE
hi! DiffChange ctermbg=NONE ctermfg=NONE cterm=NONE " changed line
hi! DiffText ctermbg=NONE ctermfg=4 cterm=NONE " changed text in changed line
