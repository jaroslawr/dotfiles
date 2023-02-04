-- TEXT
--

vim.api.nvim_set_hl(0, "Constant", { ctermfg=6 })
vim.api.nvim_set_hl(0, "String", { ctermfg=5 })
vim.api.nvim_set_hl(0, "Number", { ctermfg=3 })
vim.api.nvim_set_hl(0, "Identifier", { ctermfg=15 })
vim.api.nvim_set_hl(0, "Function", { ctermfg=3 })
vim.api.nvim_set_hl(0, "Statement", { ctermfg=4 })
vim.api.nvim_set_hl(0, "Keyword", { ctermfg=1 })
vim.api.nvim_set_hl(0, "PreProc", { ctermfg=1 })
vim.api.nvim_set_hl(0, "Include", { ctermfg=1 })
vim.api.nvim_set_hl(0, "Type", { ctermfg=6 })
vim.api.nvim_set_hl(0, "Special", { ctermfg=1 })
vim.api.nvim_set_hl(0, "SpecialChar", { ctermfg=1 })
vim.api.nvim_set_hl(0, "Delimiter", { ctermfg=7 })
vim.api.nvim_set_hl(0, "Comment", { ctermfg=7 })
vim.api.nvim_set_hl(0, "Whitespace", {})
vim.api.nvim_set_hl(0, "Todo", { ctermfg=3 })

-- BUFFER
--

vim.api.nvim_set_hl(0, "Visual", { ctermbg=11, ctermfg=0 })
vim.api.nvim_set_hl(0, "IncSearch", { ctermbg=9, ctermfg=0 })
vim.api.nvim_set_hl(0, "Search", { ctermfg=9 })
vim.api.nvim_set_hl(0, "MatchParen", { ctermfg=9 })
vim.api.nvim_set_hl(0, "EndOfBuffer", { ctermfg=240 })

-- BUFFER - folds
--
vim.api.nvim_set_hl(0, "FoldColumn", { ctermfg=0 })
vim.api.nvim_set_hl(0, "Folded", { ctermfg=11 })

-- UI
--

vim.api.nvim_set_hl(0, "LineNr", { ctermfg=240 })
vim.api.nvim_set_hl(0, "CursorLine", { ctermbg=236 })
vim.api.nvim_set_hl(0, "CursorLineNr", { ctermbg=236 })
vim.api.nvim_set_hl(0, "Msg", { ctermfg=7 })
vim.api.nvim_set_hl(0, "ErrorMsg", { ctermfg=1 })
vim.api.nvim_set_hl(0, "WarningMsg", { ctermfg=1 })
vim.api.nvim_set_hl(0, "VertSplit", { ctermfg=236 })
vim.api.nvim_set_hl(0, "SignColumn", {})
 
-- UI - tabs
--

vim.api.nvim_set_hl(0, "TabLineFill", { ctermfg=246 })
vim.api.nvim_set_hl(0, "TabLine", { ctermfg=246 })
vim.api.nvim_set_hl(0, "TabLineSel", { ctermfg=246 })

-- UI - status line
--

vim.api.nvim_set_hl(0, "StatusLine", { ctermbg=235, ctermfg=3 })
vim.api.nvim_set_hl(0, "StatusLineNC", { ctermbg=235, ctermfg=7 })

-- project name
vim.api.nvim_set_hl(0, "User1", { ctermbg=236, ctermfg=5 })

-- UI - popup menu
--

vim.api.nvim_set_hl(0, "Pmenu", { ctermbg=236, ctermfg=15 })
vim.api.nvim_set_hl(0, "PmenuSel", { ctermbg=240, ctermfg=3 })

-- UI - quickfix
--

vim.api.nvim_set_hl(0, "QuickFixLine", { ctermbg=236 })
vim.api.nvim_set_hl(0, "qfFileName", { ctermfg=3 })
vim.api.nvim_set_hl(0, "qfError", { ctermfg=3 })

-- SYNTAX - markdown
--

vim.api.nvim_set_hl(0, "markdownH1", { ctermfg=11 })
vim.api.nvim_set_hl(0, "markdownH1Delimiter", { ctermfg=11 })
vim.api.nvim_set_hl(0, "markdownH2", { ctermfg=3 })
vim.api.nvim_set_hl(0, "markdownH2Delimiter", { ctermfg=3 })
vim.api.nvim_set_hl(0, "markdownH3", { ctermfg=13 })
vim.api.nvim_set_hl(0, "markdownH3Delimiter", { ctermfg=13 })
vim.api.nvim_set_hl(0, "markdownH4", { ctermfg=5 })
vim.api.nvim_set_hl(0, "markdownH4Delimiter", { ctermfg=5 })
vim.api.nvim_set_hl(0, "markdownH5", { ctermfg=8 })
vim.api.nvim_set_hl(0, "markdownH5Delimiter", { ctermfg=8 })
vim.api.nvim_set_hl(0, "markdownH6", { ctermfg=8 })
vim.api.nvim_set_hl(0, "markdownH6Delimiter", { ctermfg=8 })
vim.api.nvim_set_hl(0, "markdownItalic", { ctermfg=8 })
vim.api.nvim_set_hl(0, "markdownUrl", { ctermfg=4 })
vim.api.nvim_set_hl(0, "markdownError", { ctermfg=1 })
 
-- SYNTAX - python
--

vim.api.nvim_set_hl(0, "pythonBuiltin", { link="Keyword" })

-- SYNTAX - javascript
--

vim.api.nvim_set_hl(0, "javaScriptBraces", { link="Normal" })

-- SYNTAX - sql
--

vim.api.nvim_set_hl(0, "sqlStatement", { link="sqlStatement" })

-- SYNTAX - xml
--

vim.api.nvim_set_hl(0, "xmlEndTag", { link="xmlTag" })

-- SYNTAX - vimscript
--

vim.api.nvim_set_hl(0, "vimFuncName", { link="Keyword" })

-- SYNTAX - git commit message header
--

vim.api.nvim_set_hl(0, "gitcommitSummary", { ctermfg=15 })
vim.api.nvim_set_hl(0, "gitcommitOverflow", { ctermfg=15 })
vim.api.nvim_set_hl(0, "gitcommitBlank", { ctermfg=9 })

-- SYNTAX - git commit message body
--

vim.api.nvim_set_hl(0, "gitcommitComment", { ctermfg=7 })
vim.api.nvim_set_hl(0, "gitcommitHeader", { ctermfg=7 })
vim.api.nvim_set_hl(0, "gitcommitSelected", { ctermfg=7 })
vim.api.nvim_set_hl(0, "gitcommitDiscarded", { ctermfg=7 })
vim.api.nvim_set_hl(0, "gitcommitUntracked", { ctermfg=7 })
vim.api.nvim_set_hl(0, "gitcommitSelectedType", { ctermfg=2 })
vim.api.nvim_set_hl(0, "gitcommitSelectedFile", { ctermfg=2 })
vim.api.nvim_set_hl(0, "gitcommitDiscardedType", { ctermfg=6 })
vim.api.nvim_set_hl(0, "gitcommitDiscardedFile", { ctermfg=6 })
vim.api.nvim_set_hl(0, "gitcommitUntrackedType", { ctermfg=7 })
vim.api.nvim_set_hl(0, "gitcommitUntrackedFile", { ctermfg=7 })

-- SYNTAX - diff
--

vim.api.nvim_set_hl(0, "diffSubname", { ctermfg=7 })
vim.api.nvim_set_hl(0, "diffAdded", { ctermfg=2 })
vim.api.nvim_set_hl(0, "diffRemoved", { ctermfg=1 })

-- SYNTAX - git diff
--

vim.api.nvim_set_hl(0, "gitKeyword", { ctermfg=8 })
vim.api.nvim_set_hl(0, "gitIdentityKeyword", { ctermfg=8 })
vim.api.nvim_set_hl(0, "gitHash", { ctermfg=5 })
vim.api.nvim_set_hl(0, "gitIdentity", { ctermfg=15 })
vim.api.nvim_set_hl(0, "gitEmail", { ctermfg=4 })
vim.api.nvim_set_hl(0, "gitDate", { ctermfg=15 })

-- 
-- SYNTAX - diff mode

vim.api.nvim_set_hl(0, "DiffAdd", { ctermfg=2 })
vim.api.nvim_set_hl(0, "DiffDelete", { ctermfg=1 })
-- changed line
vim.api.nvim_set_hl(0, "DiffChange", {})
-- changed text in changed line
vim.api.nvim_set_hl(0, "DiffText", { ctermfg=4 })

-- PLUGINS - netrw
--

vim.api.nvim_set_hl(0, "Directory", { ctermfg=4 })
vim.api.nvim_set_hl(0, "netrwExe", { ctermfg=1 })

-- PLUGINS - fzf-lua
--
vim.api.nvim_set_hl(0, "FzfLuaTitle", { ctermfg=11 })

