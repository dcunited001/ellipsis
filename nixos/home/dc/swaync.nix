{
  config,
  lib,
  pkgs,
  ...
}:

{

  hjem.users.dc.systemd.services.swaync = {
    unitConfig = {
      Description = "Sway Notification Center";
      Documentation = "https://github.com/ErikReider/SwayNotificationCenter";
      ConditionEnvironment = "WAYLAND_DISPLAY";
    };
    serviceConfig = {
      BusName = "org.freedesktop.Notifications";
      ExecStart = "${pkgs.swaynotificationcenter}/bin/swaync -c \"\${XDG_CONFIG_HOME}/swaync/config.json\" -c \"\${XDG_CONFIG_HOME}/swaync/style.css\"";
      ExecReload = "${pkgs.swaynotificationcenter}/bin/swaync-client --reload-config ; ${pkgs.swaynotificationcenter}/bin/swaync-client --reload-cs";
      Restart = "on-failure";
      RestartSec = 5;
    };
    environment.PATH = lib.mkForce null;
    environment.XDG_CONFIG_HOME = lib.mkForce null;
    environment.XDG_DATA_DIRS = lib.mkForce null;

    partOf = [ "graphical-session.target" ];
    wantedBy = [ "graphical-session.target" ];
    after = [ "graphical-session.target" ];
  };

}
