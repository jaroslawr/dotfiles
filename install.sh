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
rcup -d dotfiles \
     -S vim \
     -S config/nvim \
     -S local/share/nvim/site/pack/plugins/start

# Make scripts available
if [[ ! -h "${HOME}/bin" ]]; then
    ln -Ts "${PWD}/bin" "${HOME}/bin"
fi

source setup/setup_gnome_terminal.sh
source setup/setup_default_browser.sh
source setup/setup_vim_helptags.sh
source setup/setup_systemd_user_services.sh
