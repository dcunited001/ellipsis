##
# dcunited001/ellipsis nixos
#
# @file
# @version 0.1
MKPATH       := $(abspath $(lastword $(MAKEFILE_LIST)))
MKPATHREAL   := $(realpath $(lastword $(MAKEFILE_LIST)))
MKDIR        := $(abspath $(dir $(MKPATH)))
MKPARENT     := $(abspath $(dir $(MKDIR)))
SHELL=/bin/sh

NIXOS_HOST=$(shell hostname)
NIXOS_HOST_CONFIG=./hosts/$(NIXOS_HOST)/configuration.nix

# make NIXOS_HOST=helius build

#-----------------------
# nixos-rebuild
.PHONY: build switch repl
build:
	nixos-rebuild build --flake ".#$(NIXOS_HOST)" --show-trace

switch:
	sudo nixos-rebuild switch --flake ".#$(NIXOS_HOST)" --show-trace

repl:
	nix repl ".#nixosConfigurations.$(NIXOS_HOST)"

#-----------------------
# nh os build
.PHONY: nhbuild nhswitch
nhbuild:
	nh os build ".#nixosConfigurations.$(NIXOS_HOST)" -- --show-trace

nhswitch:
	nh os switch ".#nixosConfigurations.$(NIXOS_HOST)" -- --show-trace

#-----------------------
# nixos without flakes
.PHONY: buildhost switchhost liverepl
buildhost:
	nixos-rebuild build -I nixos-config=$(NIXOS_HOST_CONFIG)

switchhost:
	sudo nixos-rebuild switch -I nixos-config=$(NIXOS_HOST_CONFIG)

liverepl:
	nix repl --file '<nixpkgs/nixos>' -I nixos-config=$(NIXOS_HOST_CONFIG)

#-----------------------
# build iso
NIXOS_ISO="anywhere"
NIXOS_ISO_SYSTEM="x86_64-linux"

.PHONY: buildiso
buildiso:
	nix build ".#nixosConfigurations.$(NIXOS_ISO).$(NIXOS_ISO_SYSTEM).config.system.build.isoImage"

#-----------------------
# fetch iso
ISO_VARIANT=minimal            # or graphical
ISO_PLATFORM=x86_64-linux      # or aarch64-linux
ISO_VERSION=nixos-25.05        # or nixos-unstable

.PHONY: downloadNixOS
downloadNixOS:
	isoName=latest-nixos-$(ISO_VARIANT)-$(ISO_PLATFORM)
	isoUrl="https://channels.nixos.org/$(ISO_VERSION)/$(isoName).iso"
	mkdir -p iso
	wget -O iso/$isoName.iso.sha256 $isoUrl.sha256
	wget -O iso/$isoName.iso $isoUrl $isoUrl

#-----------------------
APKG=/run/current-system
BPKG=nixpkgs#libsoup
.PHONY=whydep
whydep:
	nix why-depends "$(APKG)" "$(BPKG)"

#-----------------------
# compare systems
#
# currentSys=$(readlink /run/current-system)
# currentSysDrv=$(nix path-info --derivation /run/current-system)
# nixos-rebuild build -I nixos-config=$(NIXOS_HOST_CONFIG)
# newSys=$(readlink -f ./result)
# newSysDrv=$(nix path-info --derivation $newSys)
#
# newDrv=${newSysDrv/\/nix\/store\//}
# curDrv=${currentSysDrv/\/nix\/store\//}
#
# nix-diff --word-oriented $currentSysDrv $newSysDrv > .diff/$curDrv__to__$newDrv.diff
# nix-diff --word-oriented $currentSysDrv $newSysDrv --json > .diff/$curDrv__to__$newDrv.diff.json


# run `nixpr=config.hardware make nixpr``
# nixpr:
# 	nix repl --file '<nixpkgs/nixos>' -I nixos-config=$(NIXOS_HOST_CONFIG) --expr :p ${nixpr}

# TERM=dumb nix repl --file '<nixpkgs/nixos>' -I "nixos-config=./hosts/kratos/configuration.nix" <<EOF
#   :p config.system.stateVersion
# EOF

# nixos-rebuild build -I \
#   nixos-config=$HOME/.dotfiles/nixos/hosts/kratos/configuration.nix
#
# nh -v os repl -E 'import <nixpkgs/nixos/lib/eval-config.nix> { modules = [ ./hosts/kratos/configuration.nix ]; } '
# nh -v os repl -f '<nixpkgs/nixos' -E 'with import <nixpkgs/nixos/lib/eval-config.nix>; { modules = [ ./hosts/kratos/configuration.nix ]; } '

# update guix channels from here?
# .PHONY:
# update-user-guix-on-nix:
# 	source "bash -c 'source .config/guix/current/etc/profile; '""

#
