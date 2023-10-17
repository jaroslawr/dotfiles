function status_filename()
  local result = vim.fn.expand("%:.")
  if result == "" then
    return "-"
  end
  if vim.bo.modified then
    result=result.."*"
  end
  if vim.bo.readonly then
    result=result.."#"
  end
  return result
end

vim.opt.statusline = table.concat({
  " %0*%{luaeval('status_filename()')}%* ", -- filename
  "%< ", -- avoid shortening everything until now
  "%=", -- right align
  "0x%02B ", -- byte under cursor as hex
  "%03l,%03c ", -- line and column number
  "%02p%% ", -- percentage through the file
  "%{(&ft==''?'none':&ft)} ", -- file type
  "sw=%{&shiftwidth} ", -- shift width
  "et=%{&expandtab} ", -- expand tabs
  "%{&fileformat} ", -- file format
  "%{&fileencoding==''?'':&fileencoding.' '}" -- file encoding
})
