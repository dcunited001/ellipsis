{
  config,
  pkgs,
  lib,
  ...
}:
let
  cfg = config.programs.containerlab;
in
{
  options.programs.containerlab = with lib; {
    enable = mkEnableOption "Enable containerlab";
  };

  config = lib.mkIf cfg.enable {
    environment.systemPackages = [ pkgs.containerlab ];
    users.groups.clab_admins = { };
  };
}
