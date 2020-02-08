#!/bin/bash

# Fail on errors
set -euxo pipefail

# Change the shell
SHELL="/bin/zsh"
CURRENT_SHELL=$(getent passwd $LOGNAME | cut -d: -f7)
if [[ ! $CURRENT_SHELL == $SHELL ]]; then
    chsh -s $SHELL $USER
fi

# Init the history file
touch ~/.history

# Make sure submodules are checked out and up to date
git submodule init
git submodule update --recursive

# Install dotfiles
# -x excludes
# -S symlinks whole directory
rcup -d . \
     -x install.sh \
     -x scripts \
     -S emacs.d \
     -S config/nvim \
     -S local/share/nvim

# Make scripts available
ln -s "${PWD}/bin" ~/bin

source scripts/setup_gnome_terminal.sh
source scripts/set_default_browser.sh
