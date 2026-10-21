{
  config,
  lib,
  pkgs,
  ...
}:

{
  imports = [
    ./i2c.nix
  ];

  # control moniters (requires i2c group)
  environment.systemPackages = [
    pkgs.ddccontrol-db
    pkgs.ddcutil
  ]
  ++ [ pkgs.ddccontrol ]; # NOTE: remove after issue is resolved -------

  # services.ddccontrol.enable = true;
  boot.kernelModules = [
    "ddcci_backlight"
  ];
  boot.extraModulePackages = [
    config.boot.kernelPackages.ddcci-driver
  ];
  hardware.i2c = {
    enable = true;
  };
  services.dbus.packages = [
    pkgs.ddccontrol
  ];
  systemd.packages = [
    pkgs.ddccontrol
  ];
  # ---------------------------------------------
}
