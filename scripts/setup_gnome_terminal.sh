#!/bin/bash
# Configure Gnome Terminal
set -euxo pipefail

PROFILE_ID=$(dconf read /org/gnome/terminal/legacy/profiles:/default | sed s/\'//g)
PROFILE_KEY="org.gnome.Terminal.Legacy.Profile:/org/gnome/terminal/legacy/profiles:/:${PROFILE_ID}/"

function setting() {
  gsettings set "${PROFILE_KEY}" "${1}" "${2}"
}

setting "allow-bold" "false"
setting "cell-height-scale" "1.1"
setting "use-system-font" "false"
setting "font" "Fira Code 11"
setting "use-theme-colors" "false"
setting "background-color" "#141010"
setting "foreground-color" "#fff2f2"
setting "palette" "['#141010', '#d88282', '#c7d882', '#d8c082', '#82a4d8', '#d882b6', '#82c3d8', '#999999', '#ccadad', '#f28585', '#dcf285', '#f2d385', '#85b0f2', '#f285c6', '#85d8f2', '#fff2f2']"
