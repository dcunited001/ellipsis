#!/bin/sh
# [[file:../../../Bash.org::*Guix][Guix:1]]
alias guix-all-profiles='find /gnu/store -maxdepth 1 -type d -name "*profile" -exec ls -al \{\} +'
alias guix-main="$HOME/.config/guix/current/bin/guix"

GUIX_PROFILE="$HOME/.guix-profile"
. "$GUIX_PROFILE/etc/profile"

export GUIX_LOCPATH=$HOME/.guix-profile/lib/locale
# Guix:1 ends here
