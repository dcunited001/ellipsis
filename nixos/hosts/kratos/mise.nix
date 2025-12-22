{
  config,
  lib,
  pkgs,
  ...
}:
{
  environment.systemPackages = with pkgs; [
    mise
  ];
  programs.nix-ld.enable = true;
}
