# -*- mode: shell-script -*-

# ENVIRONMENT

export EDITOR=vim
export FZF_DEFAULT_OPTS="--layout=reverse --no-bold --color 16,fg+:11,hl+:9,hl:9,pointer:9"
export FZF_TMUX=1
export PATH="${HOME}/bin:${PATH}"
export PROJECTS_ROOT="${HOME}/Projects"
export RIPGREP_CONFIG_PATH=~/.ripgreprc

# set LS_COLORS
eval $(dircolors ~/.dircolors)

# HISTORY

# Set history file
HISTFILE=~/.history

# Huge history size
SAVEHIST=100000
HISTSIZE=100000

# Share history between sessions
setopt sharehistory

# Use extended history format
setopt extendedhistory

# Do not find duplicates in history
setopt hist_save_no_dups

# Do not find duplicates in history
setopt hist_find_no_dups

# EDITING

# In M-b, M-f, C-w, consider everything except " " and "/" as part of the word
autoload -U select-word-style
WORDCHARS="()[]{}<>\"'|*?_-.~=&:;"
select-word-style normal

# KEYBINDINGS

# Emacs-style keybindings
bindkey -e

# Up and Down keys do history search when input is present
autoload -U up-line-or-beginning-search
zle -N up-line-or-beginning-search
bindkey 'OA' up-line-or-beginning-search
autoload -U down-line-or-beginning-search
zle -N down-line-or-beginning-search
bindkey 'OB' down-line-or-beginning-search

# Prevent Ctrl-S from freezing the terminal, make it accessible as a key binding
stty -ixon

# Load FZF key bindings
source /usr/share/doc/fzf/examples/key-bindings.zsh

# CD

# Go to directory by typing just the directory name
setopt auto_cd

# Push each new directory onto the stack after directory change
setopt auto_pushd

# COMPLETION

# Enable rich completion
autoload -U compinit
compinit

# Automatically list choices on an ambiguous completion
setopt menu_complete

# Show as many suggestions as fit on the screen
LISTMAX=0

# Use colors
zstyle ':completion:*' list-colors "${(@s.:.)LS_COLORS}"

# Highlight active suggestion in the menu
zstyle ':completion:*' menu select

# Case insensitive completion
zstyle ':completion:*' matcher-list 'm:{a-zA-Z}={A-Za-z}'

# Suggest cdpath subdirectories only if no local subdirectories match
zstyle ':completion:*:complete:(cd|pushd):*' tag-order 'local-directories named-directories'

# Suggest short options unless input is start of a long option
zstyle ':completion:*:*:*:*:options' ignored-patterns '???*'

# Load FZF completions
source /usr/share/doc/fzf/examples/completion.zsh

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

precmd() {
    # Update VCS info before every command execution
    vcs_info
    # Set a blank terminal title
    print -Pn "\e]0;\a"
}

# Set prompt
PROMPT='%F{11}%~%E %F{13}${vcs_info_msg_0_}
%F{12}> %f'

# OVERRIDES

# Usable calendar
alias cal='ncal -M -b'

# Usable ls
alias ls='ls --color=auto --group-directories-first --literal --time-style=long-iso --human-readable'

# ALIASES

# Output installed packages matching a query, in format suitable for xargs pipelines
alias findpkgs="dpkg-query -f '\${binary:Package}\n' -W | grep"

# Notes
alias n="vim ~/txt/notes.md"

function rg() {
    if [[ -t 1 ]]; then
        /usr/bin/rg --color=always --line-number $@ | less -FRS
    else
        /usr/bin/rg $@
    fi
}

# PROJECTS

projects_root=~/Projects
projects_root_re="${projects_root}/.*/.*"
project_dirs_glob="${projects_root}/*/"

function cdp() {
    if [[ -n "$1" ]]; then
        cd "${projects_root}/$1"
    else
        project=$(pwd | sed -E -n "s|${projects_root}/([^/]*)[/]?.*|\1|p")
        if [[ -n "${project}" ]]; then
            cd "${projects_root}/${project}"
        else
            echo "Not in a project dir"
        fi
    fi
}

function _projects() {
    # ${~x} enables glob expansion in variable x
    for proj in ${~project_dirs_glob}; do
        compadd -M 'm:{a-z}={A-Z}' -M 'r:|[._-]=* r:|=*' -M 'l:|=* r:|=*' "${proj:t}"
    done
}

compdef _projects cdp

# MISC

# More readable output of "time [command]"
TIMEFMT=$'\nreal\t%E\nuser\t%U\nsys\t%S'
