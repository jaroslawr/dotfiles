-- COLORS
--

local colors = {}

local function color(name)
  if colors[name] == nil then
    colors[name] = os.getenv("COLOR_" .. name:upper() .. "_RGB")
  end
  return colors[name]
end

-- TEXT
--

vim.api.nvim_set_hl(0, "Constant", { fg=color("cyan") })
vim.api.nvim_set_hl(0, "String", { fg=color("magenta") })
vim.api.nvim_set_hl(0, "Number", { fg=color("red") })
vim.api.nvim_set_hl(0, "Identifier", { fg=color("bright_white") })
vim.api.nvim_set_hl(0, "Function", { fg=color("yellow") })
vim.api.nvim_set_hl(0, "Statement", { fg=color("blue") })
vim.api.nvim_set_hl(0, "Keyword", { fg=color("blue") })
vim.api.nvim_set_hl(0, "PreProc", { fg=color("cyan") })
vim.api.nvim_set_hl(0, "Include", { fg=color("yellow") })
vim.api.nvim_set_hl(0, "Type", { fg=color("cyan") })
vim.api.nvim_set_hl(0, "Special", { fg=color("cyan") })
vim.api.nvim_set_hl(0, "SpecialChar", { fg=color("cyan") })
vim.api.nvim_set_hl(0, "Delimiter", { fg=color("white") })
vim.api.nvim_set_hl(0, "Comment", { fg=color("bright_gray") })
vim.api.nvim_set_hl(0, "Whitespace", {})
vim.api.nvim_set_hl(0, "Todo", { fg=color("yellow") })

-- BUFFER
--

vim.api.nvim_set_hl(0, "Visual", { bg=color("yellow"), fg=color("black") })
vim.api.nvim_set_hl(0, "IncSearch", { bg=color("bright_red"), fg=color("black") })
vim.api.nvim_set_hl(0, "Search", { fg=color("bright_red") })
vim.api.nvim_set_hl(0, "MatchParen", { fg=color("yellow") })
vim.api.nvim_set_hl(0, "EndOfBuffer", { fg=color("gray_10") })
vim.api.nvim_set_hl(0, "NonText", { fg=color("yellow") })

-- UI
--

vim.api.nvim_set_hl(0, "LineNr", { fg=color("gray_14") })
vim.api.nvim_set_hl(0, "CursorLine", { bg=color("ui_2_10") })
vim.api.nvim_set_hl(0, "CursorLineNr", { bg=color("ui_2_10"), fg=color("gray") })
vim.api.nvim_set_hl(0, "CursorLineSign", { bg=color("red") })
vim.api.nvim_set_hl(0, "Msg", { fg=color("bright_white") })
vim.api.nvim_set_hl(0, "ErrorMsg", { fg=color("red") })
vim.api.nvim_set_hl(0, "WarningMsg", { fg=color("red") })
vim.api.nvim_set_hl(0, "VertSplit", { fg=color("gray_10") })
vim.api.nvim_set_hl(0, "SignColumn", {})

-- UI - status line
--

-- focused
vim.api.nvim_set_hl(0, "StatusLine", { bg=color("ui_1_10"), fg=color("gray") })
vim.api.nvim_set_hl(0, "User1", { bg=color("ui_1_10"), fg=color("bright_yellow") })
-- not focused
vim.api.nvim_set_hl(0, "StatusLineNC", { bg=color("ui_1_8"), fg=color("bright_gray") })
vim.api.nvim_set_hl(0, "User2", { bg=color("ui_1_8"), fg=color("bright_gray") })

-- UI - quickfix
--

vim.api.nvim_set_hl(0, "QuickFixLine", { fg=color("yellow") })
vim.api.nvim_set_hl(0, "qfFileName", { fg=color("red") })
vim.api.nvim_set_hl(0, "qfLineNr", { fg=color("red") })
vim.api.nvim_set_hl(0, "qfError", { fg=color("red") })

-- UI - diagnostics
--

vim.api.nvim_set_hl(0, "DiagnosticError", { fg=color("red") })
vim.api.nvim_set_hl(0, "DiagnosticWarn", { fg=color("bright_black") })

-- SYNTAX - markdown
--

vim.api.nvim_set_hl(0, "markdownH1", { fg=color("bright_yellow") })
vim.api.nvim_set_hl(0, "markdownH1Delimiter", { fg=color("bright_yellow") })
vim.api.nvim_set_hl(0, "markdownH2", { fg=color("yellow") })
vim.api.nvim_set_hl(0, "markdownH2Delimiter", { fg=color("yellow") })
vim.api.nvim_set_hl(0, "markdownH3", { fg=color("bright_magenta") })
vim.api.nvim_set_hl(0, "markdownH3Delimiter", { fg=color("bright_magenta") })
vim.api.nvim_set_hl(0, "markdownH4", { fg=color("magenta") })
vim.api.nvim_set_hl(0, "markdownH4Delimiter", { fg=color("magenta") })
vim.api.nvim_set_hl(0, "markdownH5", { fg=color("bright_black") })
vim.api.nvim_set_hl(0, "markdownH5Delimiter", { fg=color("bright_black") })
vim.api.nvim_set_hl(0, "markdownH6", { fg=color("bright_black") })
vim.api.nvim_set_hl(0, "markdownH6Delimiter", { fg=color("bright_black") })
vim.api.nvim_set_hl(0, "markdownItalic", { fg=color("bright_black") })
vim.api.nvim_set_hl(0, "markdownUrl", { fg=color("blue") })
vim.api.nvim_set_hl(0, "markdownError", { fg=color("red") })
 
-- SYNTAX - python
--

vim.api.nvim_set_hl(0, "pythonInclude", { link="Keyword" })
vim.api.nvim_set_hl(0, "pythonStatement", { link="Keyword" })
vim.api.nvim_set_hl(0, "pythonFunction", { link="Function" })
vim.api.nvim_set_hl(0, "pythonBuiltin", { link="Special" })

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

-- SYNTAX - git commit (keep in sync with gitconfig)
--

-- header line
vim.api.nvim_set_hl(0, "gitcommitSummary", { fg=color("bright_white") })
vim.api.nvim_set_hl(0, "gitcommitOverflow", { fg=color("bright_white") })
vim.api.nvim_set_hl(0, "gitcommitBlank", { fg=color("red") })

-- message body
vim.api.nvim_set_hl(0, "gitcommitBranch", { fg=color("magenta") })
vim.api.nvim_set_hl(0, "gitcommitComment", { fg=color("gray") })
vim.api.nvim_set_hl(0, "gitcommitHeader", { fg=color("gray") })
vim.api.nvim_set_hl(0, "gitcommitSelected", { fg=color("green") })
vim.api.nvim_set_hl(0, "gitcommitSelectedType", { fg=color("green") })
vim.api.nvim_set_hl(0, "gitcommitSelectedFile", { fg=color("green") })
vim.api.nvim_set_hl(0, "gitcommitDiscarded", { fg=color("cyan") })
vim.api.nvim_set_hl(0, "gitcommitDiscardedType", { fg=color("cyan") })
vim.api.nvim_set_hl(0, "gitcommitDiscardedFile", { fg=color("cyan") })
vim.api.nvim_set_hl(0, "gitcommitUntracked", { fg=color("bright_gray") })
vim.api.nvim_set_hl(0, "gitcommitUntrackedType", { fg=color("bright_gray") })
vim.api.nvim_set_hl(0, "gitcommitUntrackedFile", { fg=color("bright_gray") })

-- SYNTAX - diff
--

vim.api.nvim_set_hl(0, "diffSubname", { fg=color("bright_white") })
vim.api.nvim_set_hl(0, "diffAdded", { fg=color("green") })
vim.api.nvim_set_hl(0, "diffRemoved", { fg=color("red") })

-- SYNTAX - git diff
--

vim.api.nvim_set_hl(0, "gitKeyword", { fg=color("bright_black") })
vim.api.nvim_set_hl(0, "gitIdentityKeyword", { fg=color("bright_black") })
vim.api.nvim_set_hl(0, "gitHash", { fg=color("magenta") })
vim.api.nvim_set_hl(0, "gitIdentity", { fg=color("bright_white") })
vim.api.nvim_set_hl(0, "gitEmail", { fg=color("blue") })
vim.api.nvim_set_hl(0, "gitDate", { fg=color("bright_white") })

-- SYNTAX - diff mode
--

vim.api.nvim_set_hl(0, "DiffAdd", { fg=color("green") })
vim.api.nvim_set_hl(0, "DiffDelete", { fg=color("red") })
-- changed line
vim.api.nvim_set_hl(0, "DiffChange", {})
-- changed text in changed line
vim.api.nvim_set_hl(0, "DiffText", { fg=color("blue") })

-- PLUGINS - netrw
--

vim.api.nvim_set_hl(0, "Directory", { fg=color("blue") })
vim.api.nvim_set_hl(0, "netrwExe", { fg=color("red") })

-- PLUGINS - fzf-lua
--

vim.api.nvim_set_hl(0, "FzfLuaTitle", { fg=color("bright_yellow") })
