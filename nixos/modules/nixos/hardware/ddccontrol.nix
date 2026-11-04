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
  services.ddccontrol.enable = true;
  environment.systemPackages = [
    pkgs.ddccontrol-db
    pkgs.ddcutil
  ];
}
