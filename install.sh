#!/bin/bash

# Fail on errors
set -euxo pipefail

# Change the shell
SHELL=$(which bash)
CURRENT_SHELL=$(getent passwd $LOGNAME | cut -d: -f7)
if [[ ! $CURRENT_SHELL == $SHELL ]]; then
    chsh -s $SHELL $USER
fi

# Make sure submodules are checked out and up to date
git submodule init
git submodule update --recursive

# Install dotfiles
# -x excludes
# -S symlinks whole directory
rcup -d dotfiles \
     -S config/nvim \
     -S local/share/nvim/site

# Make scripts available
if [[ ! -h "${HOME}/bin" ]]; then
    ln -Ts "${PWD}/bin" "${HOME}/bin"
fi

./setup/setup_gnome_terminal.sh
./setup/setup_default_browser.sh
./setup/setup_neovim_helptags.sh
./setup/setup_systemd_user_services.sh
./setup/setup_default_venv.sh
