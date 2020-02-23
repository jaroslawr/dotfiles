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
setting "palette" "['#141010', '#d88282', '#c7d882', '#d8c082', '#82b9d8', '#d882a1', '#82d8ce', '#999999', '#ccadad', '#f28585', '#dcf285', '#f2d385', '#85cbf2', '#f285ac', '#85f2e5', '#fff2f2']"
