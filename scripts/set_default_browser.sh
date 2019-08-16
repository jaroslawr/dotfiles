#!/bin/bash
set -euxo pipefail

# xdg-settings is part of xdg-utils package
xdg-settings set default-web-browser google-chrome.desktop
