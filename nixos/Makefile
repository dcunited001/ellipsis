NIXOS_HOST=$(shell hostname)
NIXOS_HOST_CONFIG=./hosts/$(NIXOS_HOST)/configuration.nix

buildhost:
	nixos-rebuild build -I nixos-config=$(NIXOS_HOST_CONFIG)

switchhost:
	sudo nixos-rebuild switch -I nixos-config=$(NIXOS_HOST_CONFIG)

liverepl:
	nix repl --file '<nixpkgs/nixos>' -I nixos-config=${NIXOS_HOST_CONFIG}

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
# 	nix repl --file '<nixpkgs/nixos>' -I nixos-config=${NIXOS_HOST_CONFIG} --expr :p ${nixpr}

# nixos-rebuild build -I \
#   nixos-config=$HOME/.dotfiles/nixos/hosts/kratos/configuration.nix
#
# nh -v os repl -E 'import <nixpkgs/nixos/lib/eval-config.nix> { modules = [ ./hosts/kratos/configuration.nix ]; } '
# nh -v os repl -f '<nixpkgs/nixos' -E 'with import <nixpkgs/nixos/lib/eval-config.nix>; { modules = [ ./hosts/kratos/configuration.nix ]; } '
