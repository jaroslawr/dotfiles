BEGIN {
  changes_in_index=0
  changes_in_worktree=0
  untracked_files=0
}

# current branch name
$2 == "branch.head" {
  branch=$3
}

# ordinary
$1 == "1" {
  status_in_index=substr($2,1,1)
  status_in_worktree=substr($2,2,1)
  if (status_in_index != ".") {
    changes_in_index++
  }
  if (status_in_worktree != ".") {
     changes_in_worktree++
  }
}

# copied or renamed
$1 == "2" {
}

# unmerged
$1 == "u" {
}

# untracked
$1 == "?" {
  untracked_files++
}

# ignored
$1 == "!" {
}

END {
  print branch "\t" changes_in_index "\t" changes_in_worktree "\t" untracked_files
}
