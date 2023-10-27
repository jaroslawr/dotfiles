#!/bin/bash
# Configure Gnome Terminal
set -euxo pipefail

PROFILE_ID=$(dconf read /org/gnome/terminal/legacy/profiles:/default | sed s/\'//g)
PROFILE_KEY="org.gnome.Terminal.Legacy.Profile:/org/gnome/terminal/legacy/profiles:/:${PROFILE_ID}/"

function setting() {
  gsettings set "${PROFILE_KEY}" "${1}" "${2}"
}


source dotfiles/colors

setting "use-system-font" "false"
setting "font" "monospace 10"
setting "cell-height-scale" "1.00"
setting "use-theme-colors" "false"
setting "background-color" "${COLOR_BLACK_RGB}"
setting "foreground-color" "${COLOR_BRIGHT_WHITE_RGB}"
setting "palette" "['${COLOR_BLACK_RGB}', '${COLOR_RED_RGB}', '${COLOR_GREEN_RGB}', '${COLOR_YELLOW_RGB}', '${COLOR_BLUE_RGB}', '${COLOR_MAGENTA_RGB}', '${COLOR_CYAN_RGB}', '${COLOR_GRAY_RGB}', '${COLOR_BRIGHT_GRAY_RGB}', '${COLOR_BRIGHT_RED_RGB}', '${COLOR_BRIGHT_GREEN_RGB}', '${COLOR_BRIGHT_YELLOW_RGB}', '${COLOR_BRIGHT_BLUE_RGB}', '${COLOR_BRIGHT_MAGENTA_RGB}', '${COLOR_BRIGHT_CYAN_RGB}', '${COLOR_BRIGHT_WHITE_RGB}']"
