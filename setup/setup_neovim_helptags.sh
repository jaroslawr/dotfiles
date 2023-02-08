#!/bin/bash
find ~/.local/share/nvim/site/pack/plugins/start/ -type d -name "doc" -print0 | \
    xargs -0 -I{} vim -e -c ":helptags {}" -c "q" || true
