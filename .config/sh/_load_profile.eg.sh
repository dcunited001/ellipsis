#!/bin/sh
# [[file:../../Bash.org::*Default Scripts][Default Scripts:2]]
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
"graphviz.sh"
"flatpak.sh"
    )

for __script in ${__scripts[@]}; do
    [[ -f $DOTS_PROFILE_D/$__script ]] && . $DOTS_PROFILE_D/$__script
done

unset $__script
# Default Scripts:2 ends here
