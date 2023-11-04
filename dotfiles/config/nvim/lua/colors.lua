-- COLORS
--

local color_black          = os.getenv("COLOR_BLACK_RGB")          -- 0
local color_red            = os.getenv("COLOR_RED_RGB")            -- 1
local color_green          = os.getenv("COLOR_GREEN_RGB")          -- 2
local color_yellow         = os.getenv("COLOR_YELLOW_RGB")         -- 3
local color_blue           = os.getenv("COLOR_BLUE_RGB")           -- 4
local color_magenta        = os.getenv("COLOR_MAGENTA_RGB")        -- 5
local color_cyan           = os.getenv("COLOR_CYAN_RGB")           -- 6
local color_gray           = os.getenv("COLOR_GRAY_RGB")           -- 7
local color_bright_gray    = os.getenv("COLOR_BRIGHT_GRAY_RGB")    -- 8
local color_bright_red     = os.getenv("COLOR_BRIGHT_RED_RGB")     -- 9
local color_bright_green   = os.getenv("COLOR_BRIGHT_GREEN_RGB")   -- 10
local color_bright_yellow  = os.getenv("COLOR_BRIGHT_YELLOW_RGB")  -- 11
local color_bright_blue    = os.getenv("COLOR_BRIGHT_BLUE_RGB")    -- 12
local color_bright_magenta = os.getenv("COLOR_BRIGHT_MAGENTA_RGB") -- 13
local color_bright_cyan    = os.getenv("COLOR_BRIGHT_CYAN_RGB")    -- 14
local color_bright_white   = os.getenv("COLOR_BRIGHT_WHITE_RGB")   -- 15

local color_gray_15 = os.getenv("COLOR_GRAY_1_15_RGB")
local color_gray_20 = os.getenv("COLOR_GRAY_1_20_RGB")
local color_gray_25 = os.getenv("COLOR_GRAY_1_25_RGB")
local color_gray_30 = os.getenv("COLOR_GRAY_1_30_RGB")
local color_gray_35 = os.getenv("COLOR_GRAY_1_35_RGB")
local color_gray_40 = os.getenv("COLOR_GRAY_1_40_RGB")
local color_gray_45 = os.getenv("COLOR_GRAY_1_45_RGB")
local color_gray_50 = os.getenv("COLOR_GRAY_1_50_RGB")
local color_gray_55 = os.getenv("COLOR_GRAY_1_55_RGB")
local color_gray_60 = os.getenv("COLOR_GRAY_1_60_RGB")

local color_gray_2_15 = os.getenv("COLOR_GRAY_2_15_RGB")
local color_gray_2_20 = os.getenv("COLOR_GRAY_2_20_RGB")
local color_gray_2_25 = os.getenv("COLOR_GRAY_2_25_RGB")
local color_gray_2_30 = os.getenv("COLOR_GRAY_2_30_RGB")
local color_gray_2_35 = os.getenv("COLOR_GRAY_2_35_RGB")
local color_gray_2_40 = os.getenv("COLOR_GRAY_2_40_RGB")
local color_gray_2_45 = os.getenv("COLOR_GRAY_2_45_RGB")
local color_gray_2_50 = os.getenv("COLOR_GRAY_2_50_RGB")
local color_gray_2_55 = os.getenv("COLOR_GRAY_2_55_RGB")
local color_gray_2_60 = os.getenv("COLOR_GRAY_2_60_RGB")

local color_gray_3_15 = os.getenv("COLOR_GRAY_3_15_RGB")
local color_gray_3_20 = os.getenv("COLOR_GRAY_3_20_RGB")
local color_gray_3_25 = os.getenv("COLOR_GRAY_3_25_RGB")
local color_gray_3_30 = os.getenv("COLOR_GRAY_3_30_RGB")
local color_gray_3_35 = os.getenv("COLOR_GRAY_3_35_RGB")
local color_gray_3_40 = os.getenv("COLOR_GRAY_3_40_RGB")
local color_gray_3_45 = os.getenv("COLOR_GRAY_3_45_RGB")
local color_gray_3_50 = os.getenv("COLOR_GRAY_3_50_RGB")
local color_gray_3_55 = os.getenv("COLOR_GRAY_3_55_RGB")
local color_gray_3_60 = os.getenv("COLOR_GRAY_3_60_RGB")

-- TEXT
--

vim.api.nvim_set_hl(0, "Constant", { fg=color_cyan })
vim.api.nvim_set_hl(0, "String", { fg=color_magenta })
vim.api.nvim_set_hl(0, "Number", { fg=color_green })
vim.api.nvim_set_hl(0, "Identifier", { fg=color_bright_white })
vim.api.nvim_set_hl(0, "Function", { fg=color_yellow })
vim.api.nvim_set_hl(0, "Statement", { fg=color_blue })
vim.api.nvim_set_hl(0, "Keyword", { fg=color_blue })
vim.api.nvim_set_hl(0, "PreProc", { fg=color_cyan })
vim.api.nvim_set_hl(0, "Include", { fg=color_yellow })
vim.api.nvim_set_hl(0, "Type", { fg=color_cyan })
vim.api.nvim_set_hl(0, "Special", { fg=color_cyan })
vim.api.nvim_set_hl(0, "SpecialChar", { fg=color_cyan })
vim.api.nvim_set_hl(0, "Delimiter", { fg=color_white })
vim.api.nvim_set_hl(0, "Comment", { fg=color_gray })
vim.api.nvim_set_hl(0, "Whitespace", {})
vim.api.nvim_set_hl(0, "Todo", { fg=color_yellow })

-- BUFFER
--

vim.api.nvim_set_hl(0, "Visual", { bg=color_yellow, fg=color_black })
vim.api.nvim_set_hl(0, "IncSearch", { bg=color_bright_red, fg=color_black })
vim.api.nvim_set_hl(0, "Search", { fg=color_bright_red })
vim.api.nvim_set_hl(0, "MatchParen", { fg=color_yellow })
vim.api.nvim_set_hl(0, "EndOfBuffer", { fg=color_gray_2_60 })
vim.api.nvim_set_hl(0, "NonText", { fg=color_yellow })

-- UI
--

vim.api.nvim_set_hl(0, "LineNr", { fg=color_gray_2_60 })
vim.api.nvim_set_hl(0, "CursorLine", { bg=color_gray_2_35 })
vim.api.nvim_set_hl(0, "CursorLineNr", { bg=color_gray_2_35, fg=color_gray })
vim.api.nvim_set_hl(0, "CursorLineSign", { bg=color_red })
vim.api.nvim_set_hl(0, "Msg", { fg=color_bright_white })
vim.api.nvim_set_hl(0, "ErrorMsg", { fg=color_red })
vim.api.nvim_set_hl(0, "WarningMsg", { fg=color_red })
vim.api.nvim_set_hl(0, "VertSplit", { fg=color_gray_35 })
vim.api.nvim_set_hl(0, "SignColumn", {})

-- UI - status line
--

-- focused
vim.api.nvim_set_hl(0, "StatusLine", { bg=color_gray_3_50, fg=color_gray })
vim.api.nvim_set_hl(0, "User1", { bg=color_gray_3_50, fg=color_bright_yellow })
-- not focused
vim.api.nvim_set_hl(0, "StatusLineNC", { bg=color_gray_35, fg=color_gray })
vim.api.nvim_set_hl(0, "User2", { bg=color_gray_35, fg=color_gray })

-- UI - quickfix
--

vim.api.nvim_set_hl(0, "QuickFixLine", { fg=color_yellow })
vim.api.nvim_set_hl(0, "qfFileName", { fg=color_red })
vim.api.nvim_set_hl(0, "qfLineNr", { fg=color_red })
vim.api.nvim_set_hl(0, "qfError", { fg=color_red })

-- UI - diagnostics
--

vim.api.nvim_set_hl(0, "DiagnosticError", { fg=color_red })
vim.api.nvim_set_hl(0, "DiagnosticWarn", { fg=color_bright_black })

-- SYNTAX - markdown
--

vim.api.nvim_set_hl(0, "markdownH1", { fg=color_bright_yellow })
vim.api.nvim_set_hl(0, "markdownH1Delimiter", { fg=color_bright_yellow })
vim.api.nvim_set_hl(0, "markdownH2", { fg=color_yellow })
vim.api.nvim_set_hl(0, "markdownH2Delimiter", { fg=color_yellow })
vim.api.nvim_set_hl(0, "markdownH3", { fg=color_bright_magenta })
vim.api.nvim_set_hl(0, "markdownH3Delimiter", { fg=color_bright_magenta })
vim.api.nvim_set_hl(0, "markdownH4", { fg=color_magenta })
vim.api.nvim_set_hl(0, "markdownH4Delimiter", { fg=color_magenta })
vim.api.nvim_set_hl(0, "markdownH5", { fg=color_bright_black })
vim.api.nvim_set_hl(0, "markdownH5Delimiter", { fg=color_bright_black })
vim.api.nvim_set_hl(0, "markdownH6", { fg=color_bright_black })
vim.api.nvim_set_hl(0, "markdownH6Delimiter", { fg=color_bright_black })
vim.api.nvim_set_hl(0, "markdownItalic", { fg=color_bright_black })
vim.api.nvim_set_hl(0, "markdownUrl", { fg=color_blue })
vim.api.nvim_set_hl(0, "markdownError", { fg=color_red })
 
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
vim.api.nvim_set_hl(0, "gitcommitSummary", { fg=color_bright_white })
vim.api.nvim_set_hl(0, "gitcommitOverflow", { fg=color_bright_white })
vim.api.nvim_set_hl(0, "gitcommitBlank", { fg=color_red })

-- message body
vim.api.nvim_set_hl(0, "gitcommitBranch", { fg=color_magenta })
vim.api.nvim_set_hl(0, "gitcommitComment", { fg=color_gray })
vim.api.nvim_set_hl(0, "gitcommitHeader", { fg=color_gray })
vim.api.nvim_set_hl(0, "gitcommitSelected", { fg=color_green })
vim.api.nvim_set_hl(0, "gitcommitSelectedType", { fg=color_green })
vim.api.nvim_set_hl(0, "gitcommitSelectedFile", { fg=color_green })
vim.api.nvim_set_hl(0, "gitcommitDiscarded", { fg=color_cyan })
vim.api.nvim_set_hl(0, "gitcommitDiscardedType", { fg=color_cyan })
vim.api.nvim_set_hl(0, "gitcommitDiscardedFile", { fg=color_cyan })
vim.api.nvim_set_hl(0, "gitcommitUntracked", { fg=color_bright_gray })
vim.api.nvim_set_hl(0, "gitcommitUntrackedType", { fg=color_bright_gray })
vim.api.nvim_set_hl(0, "gitcommitUntrackedFile", { fg=color_bright_gray })

-- SYNTAX - diff
--

vim.api.nvim_set_hl(0, "diffSubname", { fg=color_bright_white })
vim.api.nvim_set_hl(0, "diffAdded", { fg=color_green })
vim.api.nvim_set_hl(0, "diffRemoved", { fg=color_red })

-- SYNTAX - git diff
--

vim.api.nvim_set_hl(0, "gitKeyword", { fg=color_bright_black })
vim.api.nvim_set_hl(0, "gitIdentityKeyword", { fg=color_bright_black })
vim.api.nvim_set_hl(0, "gitHash", { fg=color_magenta })
vim.api.nvim_set_hl(0, "gitIdentity", { fg=color_bright_white })
vim.api.nvim_set_hl(0, "gitEmail", { fg=color_blue })
vim.api.nvim_set_hl(0, "gitDate", { fg=color_bright_white })

-- SYNTAX - diff mode
--

vim.api.nvim_set_hl(0, "DiffAdd", { fg=color_green })
vim.api.nvim_set_hl(0, "DiffDelete", { fg=color_red })
-- changed line
vim.api.nvim_set_hl(0, "DiffChange", {})
-- changed text in changed line
vim.api.nvim_set_hl(0, "DiffText", { fg=color_blue })

-- PLUGINS - netrw
--

vim.api.nvim_set_hl(0, "Directory", { fg=color_blue })
vim.api.nvim_set_hl(0, "netrwExe", { fg=color_red })

-- PLUGINS - fzf-lua
--

vim.api.nvim_set_hl(0, "FzfLuaTitle", { fg=color_bright_yellow })
