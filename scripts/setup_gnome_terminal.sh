#!/bin/bash
# Configure Gnome Terminal
set -euxo pipefail

PROFILE_ID=$(dconf read /org/gnome/terminal/legacy/profiles:/default | sed s/\'//g)
PROFILE_KEY="org.gnome.Terminal.Legacy.Profile:/org/gnome/terminal/legacy/profiles:/:${PROFILE_ID}/"

function setting() {
  gsettings set "${PROFILE_KEY}" "${1}" "${2}"
}

setting "cell-height-scale" "1.1"
setting "use-system-font" "false"
setting "font" "Ubuntu Mono 12"
setting "use-theme-colors" "false"
setting "background-color" "#232323"
setting "foreground-color" "#ffffff"
setting "palette" "['#232323', '#efa8a8', '#a8efd7', '#efe3a8', '#a8d7ef', '#efc6a8', '#a8efef', '#a38e8e', '#e0b7b7', '#ff6565', '#65ffcc', '#ffe565', '#65cbff', '#ffa565', '#65feff', '#ffffff']"
