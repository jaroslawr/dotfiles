# PATHS

# cd-s
cdpath=(~ ~/projects)

# HISTORY

# Set history file
HISTFILE=~/.history

# Huge history size
SAVEHIST=100000
HISTSIZE=100000

# Share history between sessions
setopt sharehistory

# Do not save duplicates in history
setopt hist_save_no_dups hist_ignore_all_dups

# EDITING

# In M-b, M-f, C-w, consider everything except " " and "/" as part of the word
autoload -U select-word-style
WORDCHARS="()[]{}<>\"'|*?_-.~=&:;"
select-word-style normal

# KEYBINDINGS

# Prevent Ctrl-S from freezing the terminal, make it accessible as a key binding
stty -ixon

# Make pattern search the default
bindkey '' history-incremental-pattern-search-backward
bindkey '' history-incremental-pattern-search-forward

# CD

# Go to directory by typing just the directory name
setopt auto_cd

# Push each new directory onto the stack after directory change
setopt auto_pushd

# COMPLETION

# Enable rich completion
autoload -U compinit
compinit

# On ambiguous completion, expand to the unambiguous part and show
# list of matches On second tab, present menu
unsetopt list_ambiguous

# Case insensitive completion
zstyle ':completion:*' matcher-list 'm:{a-zA-Z}={A-Za-z}'

# Highlight active completion in the menu
zstyle ':completion:*' menu select

# Complete with cdpath subdirectories only if no local subdirectories match
zstyle ':completion:*:complete:(cd|pushd):*' tag-order 'local-directories named-directories'

# PROMPT

# Support parameter expansion, command substitution and arithmetic
# expansion in prompt
setopt prompt_subst

# Load VCS module
autoload -Uz vcs_info

# Select supported VCS
zstyle ':vcs_info:*' enable git svn

# VCS info format
zstyle ':vcs_info:*' check-for-changes true
zstyle ':vcs_info:*' stagedstr "+"
zstyle ':vcs_info:*' unstagedstr "*"
zstyle ':vcs_info:*' formats "%b%c%u"

# Update VCS info before every command execution
precmd() {
    vcs_info
}

# Set prompt
PROMPT='%F{yellow}%~%E %F{blue}${vcs_info_msg_0_}
%F{green}> %f'

# ALIASES

# Usable ls
alias ls='ls -h -l --color --group-directories-first'

# Usable calendar
alias cal='ncal -M -b'

# Output installed packages matching a query, in format suitable for xargs pipelines
alias findpkgs="dpkg-query -f '\${binary:Package}\n' -W | grep"

# MISC

# More readable output of "time [command]"
TIMEFMT=$'\nreal\t%E\nuser\t%U\nsys\t%S'
