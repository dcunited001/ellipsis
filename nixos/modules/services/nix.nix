{ pkgs, ... }: {
  # nix-store --gc --print-roots
  nix.gc = {
    automatic = true;
    options = "--delete-older-than 30d";
    persistent = true;
    randomizedDelaySec = "60min";
  };

  # generate documentation for modules in configuration.nix
  # documentation.nixos.includeAllModules = true;

  nix.settings = {
    auto-optimise-store = true;
    experimental-features = [ "nix-command" "flakes" ];
    allowed-users = [ "root" "@wheel" "@builders" ];
    trusted-users = [ "@wheel" ]; # root was implicit here
    substituters = [ "https://hyprland.cachix.org" ];
    trusted-substituters = [ "https://nix-community.cachix.org" ];
    trusted-public-keys = [
      "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
    ];
  };
  nixpkgs.config.allowUnfree = true;

  environment.systemPackages = [ pkgs.cachix pkgs.nix-tree pkgs.nix-du ];
}
