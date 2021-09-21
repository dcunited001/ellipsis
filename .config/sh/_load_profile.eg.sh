#!/bin/sh
# This is the default _load_profile.d.sh script

__scripts=(
"guix.sh"
"apps.sh"
"xdg.sh"
"gtk.sh"
"qt.sh"
"emacs.sh"
"java.sh"
"julia.sh"
    )

for s in ${__scripts[@]}; do
    [[ -f $DOTS_PROFILE_D/$s ]] && . $DOTS_PROFILE_D/$s
done
