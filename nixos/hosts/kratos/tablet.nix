{
  lib,
  config,
  pkgs,
  ...
}:
{
  # https://opentabletdriver.net/Plugins
  hardware.opentabletdriver.enable = true;
  # hardware.opentabletdriver.blacklistedKernelModules = [ "hid-uclogic" "wacom" ]; # defaults

  # from JManch/nixos
  #
  # - https://github.com/JManch/nixos/blob/d8290387dba747f0ee5871e34222179d286bfde6/modules/nixos/hardware/tablet.nix#L1
  # - https://github.com/JManch/nixos/blob/d8290387dba747f0ee5871e34222179d286bfde6/lib/default.nix#L356-L361
  systemd.user.services.opentabletdriver = {
    after = [ "graphical-session.target" ];
    requisite = [ "graphical-session.target" ];
    # serviceConfig.Slice = "background${lib.${lib.ns}.sliceSuffix config}.slice";
    serviceConfig.Slice = "background-graphical.slice";
    serviceConfig.SuccessExitStatus = 143;
  };
}
