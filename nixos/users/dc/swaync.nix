{
  config,
  lib,
  pkgs,
  ...
}:
let
  dc = "dc";
in
{
  users.users.${dc}.packages = [
    pkgs.swaynotificationcenter
    pkgs.mako
    pkgs.libnotify
  ];
}
