#!/bin/bash
# Configure Gnome Terminal
set -euxo pipefail

PROFILE_ID=$(dconf read /org/gnome/terminal/legacy/profiles:/default | sed s/\'//g)
PROFILE_KEY="org.gnome.Terminal.Legacy.Profile:/org/gnome/terminal/legacy/profiles:/:${PROFILE_ID}/"

function setting() {
  gsettings set "${PROFILE_KEY}" "${1}" "${2}"
}

setting "use-system-font" "false"
setting "font" "monospace 12"
setting "cell-height-scale" "1.00"
setting "use-theme-colors" "false"
setting "background-color" "#232323"
setting "foreground-color" "#ffffff"
setting "palette" "['#1a1a1a', '#f98686', '#86f9bf', '#f9e586', '#86d2f9', '#f9b686', '#86f9f9', '#999999', '#cccccc', '#fa9e9e', '#9efacc', '#faeb9e', '#9edbfa', '#fac49e', '#9efafa', '#ffffff']"
