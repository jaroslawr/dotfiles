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
setting "font" "Inconsolata 14"
setting "use-theme-colors" "false"
setting "background-color" "#202020"
setting "foreground-color" "#f5f5f5"
setting "palette" "['#202020', '#ff5c57', '#5af78e', '#f3f99d', '#57c7ff', '#ff6ac1', '#9aedfe', '#e2e4e5', '#78787e', '#ff5c57', '#5af78e', '#f3f99d', '#57c7ff', '#ff6ac1', '#9aedfe', '#f5f5f5']"
