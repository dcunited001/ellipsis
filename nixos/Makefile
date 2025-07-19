NIXOS_HOST=$(shell hostname)
NIXOS_HOST_CONFIG=./hosts/$(NIXOS_HOST)/configuration.nix

buildhost:
	nixos-rebuild build -I nixos-config=$(NIXOS_HOST_CONFIG)

switchhost:
	sudo nixos-rebuild switch -I nixos-config=$(NIXOS_HOST_CONFIG)

liverepl:
	nix repl --file '<nixpkgs/nixos>' -I nixos-config=${NIXOS_HOST_CONFIG}

# nixos-rebuild build -I \
#   nixos-config=$HOME/.dotfiles/nixos/hosts/kratos/configuration.nix
