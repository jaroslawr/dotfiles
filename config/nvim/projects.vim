let s:home = expand('~')
let s:projects_dir = s:home . '/Projects'
let s:projects_parent_dir_re = '\(' . s:projects_dir . '\)/\([^/]\+\)[/]\?\(.*\)'
let s:projects_config_dir = expand('~/.config/nvim/projects')
let s:project_config_file_glob = resolve(s:projects_config_dir) . '/' . '*.vim'

" BASIC UTILITY FUNCTIONS

function! s:ProInProject(path)
  return match(a:path, s:projects_parent_dir_re) != -1
endfunction

function! s:ProParsePath(path)
  return matchlist(a:path, s:projects_parent_dir_re)
endfunction

function! s:ProCurPath()
  let l:path = expand('%:p')
  if len(l:path) == 0
    return getcwd()
  else
    return l:path
  endif
endfunction

function! ProPath()
  let l:path = s:ProCurPath()
  if s:ProInProject(path)
    let l:pro_dir_name_path = s:ProParsePath(l:path)
    return pro_dir_name_path[1] . '/' . pro_dir_name_path[2]
  endif
endfunction

function! ProName()
  let l:path = s:ProCurPath()
  if s:ProInProject(path)
    let l:pro_dir_name_path = s:ProParsePath(l:path)
    return pro_dir_name_path[2]
  endif
endfunction

function! ProFilePath()
  let l:path = s:ProCurPath()
  if s:ProInProject(path)
    let l:pro_dir_name_path = s:ProParsePath(l:path)
    return pro_dir_name_path[3]
  endif
endfunction

function! ProSetupProject()
  if exists('b:pro_project')
    return
  endif

  let l:path = s:ProCurPath()
  let b:pro_project = ProName()
  call ProSourceConfigFile()
endfunction

function! s:ProRequireProject()
  let l:path = s:ProCurPath()
  if s:ProInProject(path)
    return 1
  else
    call s:ProNotInProject()
    return 0
  endif
endfunction

function! s:ProNotInProject()
  echo "Not in a project"
endfunction

function! ProBuffersInProject(project)
  return filter(range(1, bufnr('$')), "getbufvar(v:val, 'pro_project') == '" . a:project . "'")
endfunction

" PROJECT CONFIG FILES

function! s:ProConfigFilePath()
  return expand(s:projects_config_dir . "/" . ProName() . ".vim")
endfunction

function! ProSourceConfigFile()
  let l:config_file_path =  s:ProConfigFilePath()
  if len(l:config_file_path) > 0 && filereadable(l:config_file_path)
    exec "source " . l:config_file_path
  endif
endfunction

function! ProConfigFileChangeAutoCmd()
  call s:ProReloadConfigOf(expand('%:r'))
endfunction

function! s:ProReloadConfigOf(project)
  let l:store_bufnr = bufnr('%')
  call map(ProBuffersInProject(a:project), function('s:ProReloadConfigInBuffer'))
  exec "buffer " . l:store_bufnr
endfunction

function! s:ProReloadConfigInBuffer(idx, bufno)
  exec a:bufno . "bufdo call ProSourceConfigFile()"
endfunction

" GLOBAL COMMANDS

function! ProFzfProjects()
  " -mindepth 1 excludes the projects-holding directory itself
  call fzf#run({'sink': function('ProOpenProject'), 'source': 'find ' . s:projects_dir . ' -mindepth 1 -maxdepth 1 -type d -printf "%P\n"'})
endfunction

command! ProFzfProjects call ProFzfProjects()

function! ProOpenProject(project)
  let l:project_path = s:projects_dir . '/' . a:project
  exec "edit! " . fnameescape(l:project_path)
endfunction

command! ProOpenProject -nargs=1 call ProOpenProject(<q-args>)

function! ProFzfFilesInWorkingDir()
  let l:store_cwd = getcwd()
  exec "cd " . expand('%:p:h')
  call fzf#run({'sink': 'edit!', 'source': 'fdfind . -tf'})
  exec "cd " . l:store_cwd
endfunction

command! ProFzfFilesInWorkingDir call ProFzfFilesInWorkingDir()

function! ProGrepInWorkingDir(query)
  let l:store_cwd = getcwd()
  exec "cd " . expand('%:p:h')
  exec "grep ". a:query
  exec "cd " . l:store_cwd
endfunction

command! -nargs=1 ProGrepInWorkingDir call ProGrepInWorkingDir(<q-args>)

" PROJECT-LOCAL COMMANDS

function! ProEditConfigFile()
  if s:ProRequireProject()
    let l:config_file_path = s:ProConfigFilePath()
    exec "edit! " . l:config_file_path
  endif
endfunction

command! ProEditConfigFile call ProEditConfigFile()

function! ProOpenRoot()
  if s:ProRequireProject()
    exec "edit! " . ProPath()
  endif
endfunction

command! ProOpenRoot call ProOpenRoot()

function! ProFzfFilesInProject()
  if s:ProRequireProject()
    let l:store_cwd = getcwd()
    exec "cd " . ProPath()
    call fzf#run({'sink': 'edit!', 'source': 'fdfind . -tf'})
    exec "cd " . l:store_cwd
  endif
endfunction

command! ProFzfFilesInProject call ProFzfFilesInProject()

function! ProGrepInProject(query)
  if s:ProRequireProject()
    let l:store_cwd = getcwd()
    exec "cd " . ProPath()
    exec "grep " . a:query
    exec "cd " . l:store_cwd
  endif
endfunction

command! -nargs=1 ProGrepInProject call ProGrepInProject(<q-args>)

function! s:ProMakeCmdRequire()
  if exists("b:ProMakeCmd") && len(b:ProMakeCmd) == 2
    return 1
  else
    echo "ProMake(): command not configured"
    return 0
  endif
endfunction

function! ProMake()
  if s:ProRequireProject()
    if !s:ProMakeCmdRequire()
      return
    endif

    let l:store_cwd = getcwd()
    let l:store_makeprg = &makeprg
    exec "cd " . ProPath()
    exec "compiler " . b:ProMakeCmd[0]
    let &makeprg = b:ProMakeCmd[1]
    make
    let &makeprg = l:store_makeprg
    exec "cd " . l:store_cwd
  endif
endfunction

command! ProMake call ProMake()

" AUTOCMDS

augroup pro
  autocmd!
  autocmd! VimEnter,BufEnter * nested :call ProSetupProject()
  exec "autocmd! BufWritePost " . s:project_config_file_glob . " :call ProConfigFileChangeAutoCmd()"
augroup END
