local M={}

local filelist={}

function M.filelist_load ()
  filelist = vim.api.nvim_buf_get_lines(0, 0, -1, true)
end

function M.filelist_fzf ()
  require'fzf-lua'.fzf_exec(filelist, {
    actions = {
      ['default'] = require'fzf-lua'.actions.file_edit
    }
  })
end

return M
