##
# Hyprland Host Deploy
#
# @file
# @version 0.1

MKPATH       := $(abspath $(lastword $(MAKEFILE_LIST)))
MKPATHREAL   := $(realpath $(lastword $(MAKEFILE_LIST)))
MKDIR        := $(abspath $(dir $(MKPATH)))
MKPARENT     := $(abspath $(dir $(MKDIR)))
SHELL=/bin/sh

HOST=$(shell hostname)

# This will run where the directory is linked from:
#
# ~/.config/hypr -> ~/.dotfiles/.config/hypr
#
# Since it's checking links as targets, we need `make -L omarchylink`

.PHONY: omarchylink
omarchylink: hyprland.conf hypridle.conf hyprlock.conf hyprsunset.conf

# this test isn't working (and it's dumb anyways)
# 	test -f hyprland.conf && $(error hyprland.conf exists, but is not a symlink)

# condense the target names using make syntax later
hyprland.conf:
	ln -s "./$(HOST).hyprland.conf" hyprland.conf
hypridle.conf:
	ln -s "./$(HOST)/hypridle.conf" hypridle.conf
hyprlock.conf:
	ln -s "./$(HOST)/hyprlock.conf" hyprlock.conf
hyprsunset.conf:
	ln -s "./$(HOST)/hyprsunset.conf" hyprsunset.conf

# end
