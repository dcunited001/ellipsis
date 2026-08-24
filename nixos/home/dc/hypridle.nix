{
  inputs,
  config,
  lib,
  pkgs,
  ...
}:
{
  hjem.users.dc.systemd.targets.hypridle = {
    unitConfig = {
      Description = "Hypridle target satisfied by either hypridle.service or hypridle-smartcard.service";
      ConditionEnvironment = "WAYLAND_DISPLAY";
    };
    wantedBy = [ "graphical-session.target" ];
    after = [ "graphical-session.target" ];
  };

  hjem.users.dc.systemd.services.hypridle = {
    unitConfig = {
      Description = "Basic hypridle service";
      Documentation = "https://github.com/hyprwm/hypridle";
      ConditionEnvironment = "WAYLAND_DISPLAY";
    };
    serviceConfig = {
      ExecStart = "${lib.getExe pkgs.hypridle} -c  \"\${XDG_CONFIG_HOME}\"/hypr/hypridle.conf";
      Restart = "on-failure";
      RestartSec = 5;
    };
    environment.PATH = lib.mkForce null;
    environment.XDG_CONFIG_HOME = lib.mkForce null;
    wantedBy = [ "hypridle.target" ];
    partOf = [ "hypridle.target" ];
    after = [ "graphical-session.target" ];
  };
}
