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
setting "font" "Ubuntu Mono 12"
setting "use-theme-colors" "false"
setting "background-color" "#181818"
setting "foreground-color" "#f5f5f5"
# derp theme from https://terminal.sexy/ (xcolors.net collection)
setting "palette" "['#111111', '#d36265', '#aece91', '#e7e18c', '#5297cf', '#963c59', '#5e7175', '#bebebe', '#666666', '#ef8171', '#cfefb3', '#fff796', '#74b8ef', '#b85e7b', '#a3babf', '#ffffff']"
