#!/bin/bash
# Configure Gnome Terminal
set -eux

PROFILE_ID=$(dconf list /org/gnome/terminal/legacy/profiles:/)
PROFILE_KEY="org.gnome.Terminal.Legacy.Profile:/org/gnome/terminal/legacy/profiles:/${PROFILE_ID}"

function setting() {
  gsettings set "${PROFILE_KEY}" "${1}" "${2}"
}

setting "allow-bold" "false"
setting "cell-height-scale" "1.05"
setting "use-system-font" "false"
setting "font" "Ubuntu Mono 12"
setting "background-color" "#282a36"
setting "foreground-color" "#e2e4e5"
setting "palette" "['#282a36', '#ff5c57', '#5af78e', '#f3f99d', '#57c7ff', '#ff6ac1', '#9aedfe', '#e2e4e5', '#78787e', '#ff5c57', '#5af78e', '#f3f99d', '#57c7ff', '#ff6ac1', '#9aedfe', '#f1f1f0']"
