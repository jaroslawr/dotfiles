#!/bin/bash
set -euxo pipefail

while IFS="" read -r service
do
  systemctl --user enable "${service}"
  systemctl --user start "${service}"
done < <(find . -path './dotfiles/config/systemd/user/*.service' -printf '%f\n' | cut -d. -f1)
