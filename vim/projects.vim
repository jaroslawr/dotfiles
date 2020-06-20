let s:home = expand('~')
let s:projects_dir = s:home . '/Projects'
let s:projects_parent_dir_re = '\(' . s:projects_dir . '\)/\([^/]\+\)[/]\?\(.*\)'
let s:projects_config_dir = expand('~/.vim/projects')
let s:project_config_file_glob = resolve(s:projects_config_dir) . '/' . '*.vim'

" BASIC UTILITY FUNCTIONS

function! ProInProject()
  return match(s:ProCurPath(), s:projects_parent_dir_re) != -1
endfunction

function! s:ProCurPath()
  let l:path = expand('%:p')
  if &ft == "netrw"
    return b:netrw_curdir
  elseif len(l:path) > 0
    return l:path
  else
    return getcwd()
  endif
endfunction

function! ProProjectRoot()
  if ProInProject()
    let l:pro_dir_name_path = s:ProParsePath(s:ProCurPath())
    return pro_dir_name_path[1] . '/' . pro_dir_name_path[2]
  endif
endfunction

function! ProProjectName()
  if ProInProject()
    let l:pro_dir_name_path = s:ProParsePath(s:ProCurPath())
    return pro_dir_name_path[2]
  endif
endfunction

function! ProProjectFilePath()
  if ProInProject()
    let l:pro_dir_name_path = s:ProParsePath(s:ProCurPath())
    return pro_dir_name_path[3]
  endif
endfunction

function! s:ProParsePath(path)
  return matchlist(a:path, s:projects_parent_dir_re)
endfunction

function! ProSetupProject()
  if exists('b:pro_project')
    return
  endif

  let l:path = s:ProCurPath()
  call ProSourceConfigFile()
  let b:pro_project = ProProjectName()
endfunction

function! s:ProRequireProject()
  if ProInProject()
    " Netrw somehow looses the setup done in the autocmd
    " This does the set up a second and final time in this case
    call ProSetupProject()
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
  return expand(s:projects_config_dir . "/" . ProProjectName() . ".vim")
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
  exec "cd " . fnameescape(l:project_path)
  exec "Explore " . fnameescape(l:project_path)
endfunction

command! ProOpenProject -nargs=1 call ProOpenProject(<q-args>)

" PROJECT-LOCAL COMMANDS

function! ProEditConfigFile()
  if s:ProRequireProject()
    let l:config_file_path = s:ProConfigFilePath()
    exec "edit! " . l:config_file_path
  endif
endfunction

command! ProEditConfigFile call ProEditConfigFile()

function! s:ProMakeCmdRequire()
  if exists("b:ProMakeCmd") && len(b:ProMakeCmd) == 2
    return 1
  else
    return 0
  endif
endfunction

function! s:ProMakePrg()
  if exists("g:ProMakePrg")
    return g:ProMakePrg
  elseif exists("b:ProMakePrg")
    return b:ProMakePrg
  else
    echoerr "Set ProMakePrg to use ProMake()"
    return ""
  endif
endfunction

function! ProMake()
  if s:ProRequireProject()
      let l:makeprg = s:ProMakePrg()
      if len(l:makeprg) > 0
        let l:store_makeprg = &makeprg
        let &makeprg = l:makeprg
        make
        let &makeprg = l:store_makeprg
      endif
  endif
endfunction

command! ProMake call ProMake()

" AUTOCMDS

augroup pro
  autocmd!
  autocmd! BufEnter * :call ProSetupProject()
  exec "autocmd! BufWritePost " . s:project_config_file_glob . " :call ProConfigFileChangeAutoCmd()"
augroup END
