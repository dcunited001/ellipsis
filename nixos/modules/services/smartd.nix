{ config, lib, pkgs, ... }:

{
  services.smartd.enable = true;
  services.smartd.autodetect = true;
  services.smartd.notifications.wall.enable = true; # default
}

  # services.smartd.devices = { device = "/dev/sda23"; options = "?"; };
  #
  # services.smartd.defaults differ for {.monitored,.autodetected}
  # .monitored passes "-a", which enables most defaults (disk-heavy?)
