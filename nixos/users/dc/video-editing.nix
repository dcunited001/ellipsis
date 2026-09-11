{

  lib,
  pkgs,
  ...
}:
{
  users.users.dc.packages = [
    pkgs.kdePackages.kdenlive
  ];
}
