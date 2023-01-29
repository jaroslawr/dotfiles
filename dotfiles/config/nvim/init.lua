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

-- PROGRAMMING
--

local python_autocmds = vim.api.nvim_create_augroup('vimrc', { clear = true })
vim.api.nvim_create_autocmd('FileType', {
  pattern = 'python',
  group = python_autocmds,
  command = 'compiler python'
})

-- APPEARANCE
--

-- Show line numbers
opt.number = true

-- KEY BINDINGS
--

local keymap = vim.api.nvim_set_keymap
local keymap_options = { noremap = true }

-- leader
vim.g.mapleader = ' '

-- leader - general
keymap('n', '<leader>x', ':qall!<cr>', keymap_options)
keymap('n', '<leader>w', ':w<cr>', keymap_options)

-- leader - fzf
keymap('n', '<leader>fb', ':FzfLua buffers<cr>', keymap_options)
keymap('n', '<leader>ff', ':FzfLua files<cr>', keymap_options)
