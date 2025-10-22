{ pkgs, ... }:
{
  programs.nh = {
    enable = true;
    clean.enable = true;
    clean.extraArgs = "--keep-since 20d --keep 20";
    # flake = "/home/user/${config.hostSpec.home}/nix-config";
  };

  environment.systemPackages = [
    pkgs.cachix
    pkgs.nix-tree
    pkgs.nix-du
  ];
}
