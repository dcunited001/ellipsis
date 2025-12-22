{
  config,
  lib,
  pkgs,
  ...
}:
{
  environment.systemPackages = with pkgs; [
    uv
  ];
  programs.nix-ld.enable = true; # magic?
}
