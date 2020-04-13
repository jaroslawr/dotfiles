#!/bin/bash
find ~/.vim/pack/plugins/start/ -type d -name "doc" -print0 | \
    xargs -0 -I{} vim -T dumb -c ":helptags {}" -c "q" 2&>1 > /dev/null
