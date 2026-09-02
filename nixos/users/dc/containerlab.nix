{
  config,
  lib,
  pkgs,
  ...
}:
{
  programs.containerlab.enable = true;
  users.users.dc.extraGroups = [ "clab_admins" ];
}
