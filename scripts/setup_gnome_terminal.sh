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
setting "background-color" "#151515"
setting "foreground-color" "#ffffff"
setting "palette" "['#151515', '#ff5c57', '#5af78e', '#f3f99d', '#57c7ff', '#ff6ac1', '#9aedfe', '#e2e4e5', '#78787e', '#ff5c57', '#5af78e', '#f3f99d', '#57c7ff', '#ff6ac1', '#9aedfe', '#f1f1f0']"
