#!/bin/sh
# [[file:~/.dotfiles/Bash.org::*FCITX][FCITX:1]]
fcitx5 --verbose default=5 -D > $HOME/.cache/log/fcitx5.$(date +%s).log 2>&1
# FCITX:1 ends here
