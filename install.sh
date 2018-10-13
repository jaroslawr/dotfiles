#!/bin/bash

# Fail on errors
set -euxo pipefail

# Dotfiles list
DOTFILES=(
    emacs
    git
    mysql
    readline
    tmux
    x
    zsh
)

# Change the shell
SHELL="/bin/zsh"
CURRENT_SHELL=$(getent passwd $LOGNAME | cut -d: -f7)
if [[ ! $CURRENT_SHELL == $SHELL ]]; then
    chsh -s $SHELL $USER
fi

# Init the history file
touch ~/.history

# Disable ssh component of gnome keyring
# (annoying modal window for ssh pass phrases)
mkdir -p ~/.config/autostart
cp config/autostart/gnome-keyring-ssh.desktop ~/.config/autostart

# Install dotfiles
stow -v -v -v -d . -t ~ ${DOTFILES[@]}
