##
# dcunited001/ellipsis nixos
#
# @file
# @version 0.1

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
.PHONY: nhbuild
nhbuild:
	nh os build ".#nixosConfigurations.$(NIXOS_HOST)" -- --show-trace

nhswitch: nhswitch
	nh os switch ".#nixosConfigurations.$(NIXOS_HOST)" -- --show-trace

#-----------------------
# nixos without flakes
.PHONY: buildhost switchhost liverepl
buildhost:
	nixos-rebuild build -I nixos-config=$(NIXOS_HOST_CONFIG)

switchhost:
	sudo nixos-rebuild switch -I nixos-config=$(NIXOS_HOST_CONFIG)

liverepl:
	nix repl --file '<nixpkgs/nixos>' -I nixos-config=${NIXOS_HOST_CONFIG}

#-----------------------
# build iso
NIXOS_ISO="anywhere"
NXISO_ISO_SYSTEM="x86_64-linux"

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

#
