{
  config,
  lib,
  inputs,
  pkgs,
  ...
}:
let
  ai-jail = inputs.ai-jail.packages.${pkgs.stdenv.system}.default;
in
{
  users.users.dc.packages = [
    pkgs.opencode-desktop
    ai-jail
  ];
}
