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
setting "font" "Fira Code weight=453 11" # Fira Code Retina
setting "use-theme-colors" "false"
setting "background-color" "#191818"
setting "foreground-color" "#fff2f2"
setting "palette" "['#191818', '#ffb2b2', '#b2ffd8', '#fff2b2', '#b2ebff', '#ffc5b2', '#b2ffeb', '#999999', '#ccadad', '#ff7f7f', '#7fffbf', '#ffe97f', '#7fdfff', '#ff9f7f', '#7fffdf', '#fff2f2']"
