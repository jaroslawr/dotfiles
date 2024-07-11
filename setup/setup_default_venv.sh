#!/bin/bash

# set -u and set -e break virtualenvwrapper
set -x

source /usr/share/virtualenvwrapper/virtualenvwrapper.sh

mkvirtualenv home
workon home
pip install pip-tools
pip-compile venv/requirements.in
pip install -r venv/requirements.txt
