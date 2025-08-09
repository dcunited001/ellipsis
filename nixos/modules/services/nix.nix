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
    trusted-substituters = [ "https://hyprland.cachix.org" ];
    trusted-public-keys =
      [ "hyprland.cachix.org-1:a7pgxzMz7+chwVL3/pzj6jIBMioiJM7ypFP8PwtkuGc=" ];
  };
  nixpkgs.config.allowUnfree = true;

  environment.systemPackages = [ pkgs.cachix pkgs.nix-tree ];
}
