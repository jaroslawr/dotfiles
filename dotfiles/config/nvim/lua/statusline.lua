local M={}

local function filename()
  local buffer = vim.api.nvim_win_get_buf(vim.g.statusline_winid)
  local filename = vim.fn.fnamemodify(vim.api.nvim_buf_get_name(buffer), ":.")
  if filename == "" then
    filename = "-"
  end

  local focus = vim.g.statusline_winid == vim.fn.win_getid()
  local filename_color = focus and "%1*" or "%2*"

  local modifiers = ""
  modifiers = modifiers .. (vim.bo.modified and "*" or "")
  modifiers = modifiers .. (vim.bo.readonly and "#" or "")

  return " " .. filename_color .. filename .. modifiers .. "%0*" .. " "
end

function statusline()
  return table.concat({
    filename(), -- filename
    "%<", -- avoid shortening everything until now
    "%03l:%03c ", -- line and column number
    "%02p%% ", -- percentage through the file
    "%=", -- right align
    "%{&fileformat} ", -- file format
    "%{&fileencoding==''?'':&fileencoding.' '}", -- file encoding
    "%{(&ft==''?'none':&ft)} ", -- file type
    "sw=%{&shiftwidth} ", -- shift width
    "et=%{&expandtab} ", -- expand tabs
  })
end

vim.opt.statusline = "%!luaeval('statusline()')"
