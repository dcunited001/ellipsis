{ config, lib, pkgs, ... }:

{
  # Database for aiding terminal-based programs
  environment.enableAllTerminfo = true;

  # -----------------------
  # nixpkgs (pre-overlay)
  nixpkgs.config = {
    allowBroken = true;
    allowUnfree = true;
  };

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
    optimise = {
      automatic = true;
      dates = [ "03:45" ]; # Periodically optimize the store
    };

    connect-timeout = 5;
    log-lines = 25;
    # min-free = 128000000; # 128MB
    # max-free = 1000000000; # 1GB

    experimental-features = lib.mkDefault "nix-command flakes";

    builders-use-substitutes = true;
    fallback = true;

    allowed-users = [ "root" "@wheel" "@builders" ];
    trusted-users = [ "@wheel" ]; # root was implicit here
    substituters = [ "https://hyprland.cachix.org" ];
    trusted-substituters =
      [ "https://cache.nixos.org" "https://nix-community.cachix.org" ];
    trusted-public-keys = [
      "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
    ];
  };

}
