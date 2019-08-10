let s:home = expand('~')
let s:projects_dir = s:home . '/Projects'
let s:projects_parent_dir_re = '\(' . s:projects_dir . '\)/\([^/]\+\)[/]\?\(.*\)'

function! s:ProInProject(path)
  return match(a:path, s:projects_parent_dir_re) != -1
endfunction!

function! s:ProParsePath(path)
  return matchlist(a:path, s:projects_parent_dir_re)
endfunction!

function! s:ProCurPath()
  let l:path = expand('%:p')
  if len(l:path) == 0
    return getcwd()
  else
    return l:path
  endif
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

function! s:ProConfigFilePath()
  if s:ProInProject(s:ProCurPath())
    return expand("~/.vim/projects/" . ProName() . ".vim")
  else
    return ""
  endif
endfunction

function! s:ProSourceConfigFile()
  let l:config_file_path = s:ProConfigFilePath()
  if len(l:config_file_path) > 0 && filereadable(l:config_file_path)
    exec "source " . l:config_file_path
  endif
endfunction!

autocmd BufRead,BufNewFile * :call s:ProSourceConfigFile()

function! ProEditConfigFile()
  if s:ProInProject(s:ProCurPath())
    let l:config_file_path = s:ProConfigFilePath()
    exec "edit " . l:config_file_path
  else
    call s:ProNotInProject()
  endif
endfunction

function! s:ProNotInProject()
  echo "Not in a project"
endfunction

function! ProFzfProjects()
  " -mindepth 1 excludes the projects-holding directory itself
  call fzf#run({'sink': function('ProOpenProject'), 'source': 'find ' . s:projects_dir . ' -mindepth 1 -maxdepth 1 -type d -printf "%P\n"'})
endfunction

command! ProFzfProjects call ProFzfProjects()

function! ProOpenProject(project)
  let l:project_path = s:projects_dir . '/' . a:project
  exec "edit " . fnameescape(l:project_path)
endfunction

command! ProOpenProject -nargs=1 call ProOpenProject(<q-args>)

function! ProFzfFilesInProject()
  let l:path = s:ProCurPath()
  if s:ProInProject(l:path)
    let l:store_cwd = getcwd()
    exec "cd " . ProPath()
    call fzf#run({'sink': 'e', 'source': 'fdfind . -tf'})
    exec "cd " . l:store_cwd
  else
    call s:ProNotInProject()
  endif
endfunction

command! ProFzfFilesInProject call ProFzfFilesInProject()

function! ProFzfFilesInWorkingDir()
  let l:store_cwd = getcwd()
  exec "cd " . expand('%:p:h')
  call fzf#run({'sink': 'e', 'source': 'fdfind . -tf'})
  exec "cd " . l:store_cwd
endfunction

command! ProFzfFilesInWorkingDir call ProFzfFilesInWorkingDir()

function! ProGrepInProject(query)
  echo "ProGrep"
  let l:path = s:ProCurPath()
  if s:ProInProject(l:path)
    let l:store_cwd = getcwd()
    exec "cd " . ProPath()
    exec "grep " . a:query
    exec "cd " . l:store_cwd
  endif
endfunction

command! -nargs=1 ProGrepInProject call ProGrepInProject(<q-args>)

function! ProGrepInWorkingDir(query)
  let l:store_cwd = getcwd()
  exec "cd " . expand('%:p:h')
  exec "grep ". a:query
  exec "cd " . l:store_cwd
endfunction

command! -nargs=1 ProGrepInWorkingDir call ProGrepInWorkingDir(<q-args>)

let g:ProMakePrg=""

function! ProMake()
  let l:path = s:ProCurPath()
  if s:ProInProject(l:path)
    let l:store_cwd = getcwd()
    let l:store_makeprg = &makeprg
    exec "cd " . ProPath()
    let &makeprg = b:ProMakePrg
    make
    let &makeprg = l:store_makeprg
    exec "cd " . l:store_cwd
  endif
endfunction

command! ProMake call ProMake()
