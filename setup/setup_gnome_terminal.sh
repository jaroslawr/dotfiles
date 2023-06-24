#!/bin/bash
# Configure Gnome Terminal
set -euxo pipefail

PROFILE_ID=$(dconf read /org/gnome/terminal/legacy/profiles:/default | sed s/\'//g)
PROFILE_KEY="org.gnome.Terminal.Legacy.Profile:/org/gnome/terminal/legacy/profiles:/:${PROFILE_ID}/"

function setting() {
  gsettings set "${PROFILE_KEY}" "${1}" "${2}"
}

setting "use-system-font" "false"
setting "font" "monospace 10.5"
setting "cell-height-scale" "1.00"
setting "use-theme-colors" "false"
setting "background-color" "#232323"
setting "foreground-color" "#ffffff"
setting "palette" "['#252218', '#ff8080', '#80ffbf', '#ffea80', '#80d4ff', '#ffb580', '#80ffff', '#ad8585', '#d6c2c2', '#ffb3b3', '#b3ffd9', '#fff2b3', '#b3e5ff', '#ffd2b3', '#b3ffff', '#ffffff']"
