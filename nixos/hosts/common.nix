{
  lib,
  ...
}:

{
  # Database for aiding terminal-based programs
  #
  # NOTE until #465358 is resolved, it builds without OpenSSL
  #
  # - https://github.com/NixOS/nixpkgs/issues/465358
  # - https://github.com/contour-terminal/termbench-pro
  #
  # environment.enableAllTerminfo = true;

  # -----------------------
  # nixpkgs (pre-overlay)
  nixpkgs.config = {
    allowBroken = true;
    allowUnfree = true;
  };

  # nix-store --gc --print-roots
  nix.gc = {
    automatic = false; # use nh clean
    options = "--delete-older-than 30d";
    persistent = true;
    randomizedDelaySec = "60min";
  };

  # generate documentation for modules in configuration.nix
  # documentation.nixos.includeAllModules = true;

  nix.settings = {
    auto-optimise-store = true;

    connect-timeout = 5;
    log-lines = 25;
    # min-free = 128000000; # 128MB
    # max-free = 1000000000; # 1GB

    experimental-features = lib.mkDefault "nix-command flakes";

    builders-use-substitutes = true;
    fallback = true;

    allowed-users = [
      "root"
      "@wheel"
      "@builders"
    ];
    trusted-users = [ "@wheel" ]; # root was implicit here
    substituters = [
      "https://hyprland.cachix.org"
      "https://cache.nixos.org"
      "https://nix-community.cachix.org"
    ];
    trusted-substituters = [
      "https://hyprland.cachix.org"
      "https://cache.nixos.org"
      "https://nix-community.cachix.org"
    ];
    trusted-public-keys = [
      "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
      "hyprland.cachix.org-1:a7pgxzMz7+chwVL3/pzj6jIBMioiJM7ypFP8PwtkuGc="
    ];
  };

}
