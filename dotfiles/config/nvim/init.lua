-- ALIASES
--

local opt = vim.opt

-- GENERAL OPTIONS
--

-- Enable mouse support
opt.mouse = 'a'

-- Use system clipboard
opt.clipboard = 'unnamedplus'

-- Grep with ripgrep
opt.grepprg = 'rg -n $*'

-- APPEARANCE
--

-- Show line numbers
opt.number = true
