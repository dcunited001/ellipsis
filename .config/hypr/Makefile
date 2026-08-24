##
# Hyprland Host Deploy
#
# @file
# @version 0.1

MKDIR      ?= $(patsubst %/,%,$(dir $(abspath $(lastword $(MAKEFILE_LIST)))))

SHELL=/bin/sh
HOST=$(shell hostname)

# This will run where the directory is linked from:
#
# ~/.config/hypr -> ~/.dotfiles/.config/hypr
#
# Since it's checking links as targets, we need `make -L omarchylink`

.PHONY: lualink
link: hyprland.lua hyprsunset.conf xdph.conf

hyprland.lua:
	ln -s "./$(HOST).hyprland.lua" hyprland.lua

.PHONY: conflink
link: hypridle.conf hyprlock.conf hyprsunset.conf xdph.conf

# these can't be used simply with linked file targets
# 	test -f hyprland.conf && $(error hyprland.conf exists, but is not a symlink)
# 	test -s hyprland.lua && $(error fdsa) || ln -s "./$(HOST).hyprland.lua" hyprland.lua

# condense the target names using make syntax later
hypridle.conf:
	ln -s "./$(HOST)/hypridle.conf" hypridle.conf
hyprlock.conf:
	ln -s "./$(HOST)/hyprlock.conf" hyprlock.conf
hyprsunset.conf:
	ln -s "./$(HOST)/hyprsunset.conf" hyprsunset.conf
xdph.conf:
	ln -s "./$(HOST)/xdph.conf" xdph.conf

hyprlua="$(HOME)/.config/hypr/$(HOST).hyprland.lua"

.PHONY: luaverify
luaverify:
	hyprland --verify-config -c "$(HOST).hyprland.lua"

watchdebounce=5s
watchlua="*.lua"

.PHONY: luawatch
luawatch:
	watchexec -d "$(watchdebounce)" --workdir . --filter="$(watchlua)" hyprland --verify-config -c "$(HOST).hyprland.lua"

# end
