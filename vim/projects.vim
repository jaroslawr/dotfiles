let s:home = expand('~')
let s:projects_parent_dir_re = '\(' . s:home . '/Projects\)/\([^/]\+\)[/]\?\(.*\)'

function! s:ProInProject(path)
  return match(a:path, s:projects_parent_dir_re) != -1
endfunction!

function! s:ProParsePath(path)
  return matchlist(a:path, s:projects_parent_dir_re)
endfunction!

function! s:ProCurPath()
  return expand('%:p')
endfunction!

function! ProPath()
  let l:path = s:ProCurPath()
  if s:ProInProject(path)
    let l:pro_dir_name_path = s:ProParsePath(l:path)
    return pro_dir_name_path[1] . '/' . pro_dir_name_path[2]
  endif
endfunction!

function! ProName()
  let l:path = s:ProCurPath()
  if s:ProInProject(path)
    let l:pro_dir_name_path = s:ProParsePath(l:path)
    return pro_dir_name_path[2]
  endif
endfunction!

function! ProFilePath()
  let l:path = s:ProCurPath()
  if s:ProInProject(path)
    let l:pro_dir_name_path = s:ProParsePath(l:path)
    return pro_dir_name_path[3]
  endif
endfunction

function! ProNotInProject()
  echo "Not in a project"
endfunction

function! ProFzf()
  let l:path = s:ProCurPath()
  if s:ProInProject(l:path)
    let l:store_cwd = getcwd()
    exec "cd " . l:path
    call fzf#run({'sink': 'e'})
    exec "cd " . l:store_cwd
  else
    call ProNotInProject()
  endif
endfunction

function! ProDirFzf()
  let l:store_cwd = getcwd()
  exec "cd " . expand('%:p:h')
  call fzf#run({'sink': 'e'})
  exec "cd " . l:store_cwd
endfunction