BEGIN {
  staged_changes=0
  nonstaged_changes=0
  untracked_files=0
}

# current branch name
$2 == "branch.head" {
  branch=$3
}

# ordinary
$1 == "1" {
  index_change=substr($2,1,1)
  worktree_change=substr($2,2,1)
  if (index_change == ".") {
    if (worktree_change != ".") {
      nonstaged_changes++
    }
  } else {
    staged_changes++
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
  print branch "\t" staged_changes "\t" nonstaged_changes "\t" untracked_files
}
