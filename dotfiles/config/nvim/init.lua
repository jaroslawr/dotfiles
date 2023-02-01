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

local python_autocmds = vim.api.nvim_create_augroup('init_python', { clear = true })
vim.api.nvim_create_autocmd('FileType', {
  pattern = 'python',
  group = python_autocmds,
  command = 'compiler python'
})

-- APPEARANCE
--

-- Load color theme
require('colors')

-- Show line numbers
opt.number = true

-- TMUX INTEGRATION
--

-- Set tmux window title
opt.title = true

-- Function for computing the title
function tmux_title()
	local path = vim.fn.expand("%:t")
	if string.len(path) > 0 then
		return path
	else
		return vim.fn.fnamemodify(vim.fn.getcwd(), ":t")
	end
end

-- Update the titlestring using tmux_title()
local tmux_autocmds = vim.api.nvim_create_augroup('init_tmux', { clear = true })
vim.api.nvim_create_autocmd({ 'VimEnter', 'WinEnter', 'BufEnter' }, {
  pattern = '*',
  group = tmux_autocmds,
  command = 'let &titlestring = v:lua.tmux_title()'
})

-- PLUGINS - fzf-lua
--

require('fzf-lua').setup({
	winopts = {
		border = false,
		fullscreen = true,
		preview = {
			layout = 'horizontal'
		}
	}
})

-- KEY BINDINGS
--

local keymap = vim.api.nvim_set_keymap
local keymap_options = { noremap = true }

-- back to normal mode with jj when in insert mode
keymap('i', 'jj', '<Esc>', keymap_options)

-- quickfix
keymap('n', '<A-,>', ':cprev<cr>', keymap_options)
keymap('n', '<A-.>', ':cnext<cr>', keymap_options)
keymap('n', '<A-/>', ':copen<cr>', keymap_options)
keymap('n', '<A-?>', ':cclose<cr>', keymap_options)

-- leader
vim.g.mapleader = ' '

-- leader - general
keymap('n', '<leader>x', ':qall!<cr>', keymap_options)
keymap('n', '<leader>w', ':w<cr>', keymap_options)

-- leader - fzf
keymap('n', '<leader>ff', ':FzfLua files<cr>', keymap_options)
keymap('n', '<leader>fb', ':FzfLua buffers<cr>', keymap_options)
keymap('n', '<leader>fa', ':FzfLua args<cr>', keymap_options)

-- leader - make
keymap('n', '<leader>m', ':make<cr>', keymap_options)
