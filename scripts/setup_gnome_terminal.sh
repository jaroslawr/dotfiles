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
setting "font" "JetBrains Mono 10"
setting "use-theme-colors" "false"
setting "background-color" "#232323"
setting "foreground-color" "#ffffff"
setting "palette" "['#232323', '#f99e9e', '#9ef9db', '#f9ea9e', '#9edbf9', '#f9c49e', '#9ef9f9', '#a38e8e', '#e0b7b7', '#ff6565', '#65ffcc', '#ffe565', '#65cbff', '#ffa565', '#65feff', '#ffffff']"
