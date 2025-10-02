##
# Hyprland Host Deploy
#
# @file
# @version 0.1

MKPATH       := $(abspath $(lastword $(MAKEFILE_LIST)))
MKPATHREAL  := $(realpath $(lastword $(MAKEFILE_LIST)))
MKDIR        := $(abspath $(dir $(MKPATH)))
MKPARENT     := $(abspath $(dir $(MKDIR)))
SHELL=/bin/sh

# this will run where the directory is linked from:
#
# ~/.config/hypr -> ~/.dotfiles/.config/hypr

HOST=$(shell hostname)

# condense the target names using make syntax later
hyprland.conf:
	test -f hyprland.conf && $(error hyprland.conf exists, but is not a symlink)
	ln -s "./$(HOST)/hyprland.conf" hyprland.conf
hypridle.conf:
	test -f hypridle.conf && $(error hypridle.conf exists, but is not a symlink)
	ln -s "./$(HOST)/hypridle.conf" hypridle.conf
hyprlock.conf:
	test -f hyprlock.conf && $(error hyprlock.conf exists, but is not a symlink)
	ln -s "./$(HOST)/hyprlock.conf" hyprlock.conf
hyprsunset.conf:
	test -f hyprsunset.conf && $(error hyprsunset.conf exists, but is not a symlink)
	ln -s "./$(HOST)/hyprsunset.conf" hyprsunset.conf

.PHONY: omarchylink
omarchylink: hyprland.conf hypridle.conf hyprlock.conf hyprsunset.conf

# end
