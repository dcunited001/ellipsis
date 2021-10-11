#!/bin/sh
# [[file:../../../Bash.org::*Julia][Julia:1]]
export JULIA_SHELL=/bin/sh
export JULIA_EDITOR=vim
# Julia:1 ends here

# [[file:../../../Bash.org::*Julia][Julia:2]]
export JULIA_LOAD_PATH="$JULIA_LOAD_PATH"
export JULIA_DEPOT_PATH="$_DATA/lang/.julia:$JULIA_DEPOT_PATH"
# Julia:2 ends here
