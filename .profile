#!/bin/sh
# [[file:Bash.org::*XDG Shim][XDG Shim:1]]
# If XDG variables need to change from default, set them here.
[[ -f ~/.xdg_shim ]] && . ~/.xdg_shim.sh

[[ -z $XDG_CONFIG_HOME ]] && export XDG_CONFIG_HOME=$HOME/.config
[[ -z $XDG_CONFIG_DIRS ]] && export XDG_CONFIG_DIRS=/usr/local/share/:/usr/share/
[[ -z $XDG_DATA_HOME ]] && export XDG_DATA_HOME=$HOME/.local/share
[[ -z $XDG_DATA_DIRS ]] && export XDG_DATA_DIRS=/etc/xdg
[[ -z $XDG_STATE_HOME ]] && export XDG_STATE_HOME=$HOME/.local/state
[[ -z $XDG_CACHE_HOME ]] && export XDG_CACHE_HOME=$HOME/.cache
# XDG Shim:1 ends here

# [[file:Bash.org::*Before Profile.d][Before Profile.d:1]]
export DOTS_CFG_SHELL=$XDG_CONFIG_HOME/sh
export DOTS_PROFILE_D=$DOTS_CFG_SHELL/profile.d

[[ -f $DOTS_CFG_SHELL/_before_profile.d.sh ]] && source $DOTS_CFG_SHELL/_before_profile.d.sh

[[ -f $DOTS_CFG_SHELL/_load_profile.d.sh ]] && source $DOTS_CFG_SHELL/_load_profile.d.sh
# Before Profile.d:1 ends here

# [[file:Bash.org::*Process][Process:1]]
# Browser
export MOZ_ENABLE_WAYLAND=1             # only start firefox in wayland mode and no other GTK apps
export MOZ_DBUS_REMOTE=1                # fixes firefox is already running, but is not responding

# clutter
#export CLUTTER_BACKEND=wayland          # this can prevent programs from starting. therefore, this should be set per app instead of globally.


# elementary
export ECORE_EVAS_ENGINE=wayland-egl
export ELM_ENGINE=wayland_egl
#export ELM_DISPLAY=wl
#export ELM_ACCEL=gl

# TODO: Accessibility
# http://library.gnome.org/devel/accessibility-devel-guide/stable/gad-how-it-works.html.en

# disables accessibility
export NO_AT_BRIDGE=1

# Bemenu (not configured in sway)
export BEMENU_BACKEND=wayland

# sdl
#export SDL_VIDEODRIVER=wayland        # this can prevent programs from starting old sdl games. therefore, this should be set per app instead of globally.

# Graphviz
export GRAPHVIZ_DOT=/usr/bin/dot
# Process:1 ends here

# [[file:Bash.org::*After Profile.d][After Profile.d:1]]
[[ -f $DOTS_CFG_SHELL/_after_profile.d.sh ]] && source $DOTS_CFG_SHELL/_after_profile.d.sh
# After Profile.d:1 ends here

# [[file:Bash.org::*=.profile=][=.profile=:1]]
# TODO: decide on sourcing .bashrc
# if [ -f ~/.bashrc ]; then . ~/.bashrc; fi
# =.profile=:1 ends here
