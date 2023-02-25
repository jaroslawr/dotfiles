-- GENERAL OPTIONS
--

-- Enable mouse support
vim.opt.mouse = 'a'

-- Use system clipboard
vim.opt.clipboard = 'unnamedplus'

-- Disable backups
vim.opt.backup = false
vim.opt.writebackup = false

-- Disable swapfiles
vim.opt.swapfile = false

-- Open splits below and right
vim.opt.splitbelow = true
vim.opt.splitright = true

-- Grep with ripgrep
vim.opt.grepprg = 'rg -n $*'

-- APPEARANCE
--

-- Show line numbers
vim.opt.number = true

-- Show signs in number column
vim.opt.signcolumn = 'number'

-- Color theme
require('colors')

-- Status line
require('statusline')

-- Cursor line
local cursorline_autocmds = vim.api.nvim_create_augroup('init_cursorline', { clear = true })
vim.api.nvim_create_autocmd({ 'VimEnter', 'WinEnter' }, {
  pattern = '*',
  group = cursorline_autocmds,
  command = 'set cursorline'
})
vim.api.nvim_create_autocmd({ 'WinLeave' }, {
  pattern = '*',
  group = cursorline_autocmds,
  command = 'set nocursorline'
})

-- TMUX INTEGRATION
--

-- Set tmux window title
vim.opt.title = true

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

-- MARKDOWN
--

-- Word wrap markdown at 80 characters
local markdown_autocmds = vim.api.nvim_create_augroup('init_markdown', { clear = true })
vim.api.nvim_create_autocmd('FileType', {
  pattern = 'markdown',
  group = markdown_autocmds,
  command = 'setlocal textwidth=80 shiftwidth=4 tabstop=4 expandtab'
})

-- PROGRAMMING - common
--

local on_attach_common = function(client, bufnr)
  local keymap_opts = { noremap=true, silent=true, buffer=bufnr }
  vim.keymap.set('n', 'gD', vim.lsp.buf.declaration, keymap_opts)
  vim.keymap.set('n', 'gd', vim.lsp.buf.definition, keymap_opts)
end

-- PROGRAMMING - go
--

local on_attach_gopls = on_attach_common
require'lspconfig'.gopls.setup{ on_attach = on_attach_gopls }

-- PROGRAMMING - python
--

-- Set the python interpreter as compiler for python files
local python_autocmds = vim.api.nvim_create_augroup('init_python', { clear = true })
vim.api.nvim_create_autocmd('FileType', {
  pattern = 'python',
  group = python_autocmds,
  command = 'compiler python'
})

local on_attach_pylsp = on_attach_common
require'lspconfig'.pylsp.setup{ on_attach = on_attach_pylsp }

-- PLUGINS - fzf-lua
--

require('fzf-lua').setup({
  winopts = {
    border = false,
    fullscreen = true,
    preview = {
      layout = 'horizontal',
      horizontal = 'down',
      winopts = {
        cursorline = false
      }
    },
    preview_pos = 'down'
  },
  fzf_opts = {
    ['--tiebreak'] = 'index'
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
keymap('n', '<leader>fl', ":lua require'filelist'.fzf()<cr>", keymap_options)

-- leader - make
keymap('n', '<leader>m', ':make<cr>', keymap_options)
