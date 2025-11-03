{
  inputs,
  config,
  lib,
  pkgs,
  ...
}:
{
  hjem.users.dc.files = {
    "bin/hypridle-kill" = {
      executable = true;
      text = ''
        #!/usr/bin/env bash
        pidHypridle=$(pidof hypridle);
        if [ $? -ne 0 ]; then echo "Failed to get hypridle pid"; fi
        if [ -n "$pidHypridle" ]; then
          echo "Attempting to SIGTERM $pidHypridle";
          kill -SIGTERM $pidHypridle
          result=$?
          sleep 1;
          if [ $? -ne 0 ]; then
            echo "Failed: hypridle $pidHypridle could not be killed";
            exit 1;
          else
            echo "Success: hypridle $pidHypridle killed";
            exit 0;
          fi
        fi
        echo "Couldn't kill hypridle pid $pidHypridle" && exit 1
      '';
    };
  };

  hjem.users.dc.systemd.services.hypridle = {
    unitConfig = {
      Description = "Hypridle with no smartcard";
      Documentation = "https://github.com/hyprwm/hypridle";
      ConditionEnvironment = "WAYLAND_DISPLAY";
    };
    serviceConfig = {
      ExecStart = "${lib.getExe pkgs.hypridle} -c  \"\${XDG_CONFIG_HOME}\"/hypr/hypridle.conf";
      ExecStop = "${config.hjem.users.dc.files."bin/hypridle-kill".target}";
      Restart = "on-failure";
      RestartSec = 5;
    };
    environment.PATH = lib.mkForce null;
    environment.XDG_CONFIG_HOME = lib.mkForce null;
    conflicts = [
      # "smartcard.target"
      "hypridleSmartcard.service"
    ];
    wantedBy = [ "graphical-session.target" ];
    partOf = [ "graphical-session.target" ];
    after = [
      "graphical-session.target"
      # "hypridleSmartcard.service"
    ];
  };
  hjem.users.dc.systemd.services.hypridleSmartcard = {
    unitConfig = {
      Description = "Hypridle with a smartcard";
      Documentation = "https://github.com/hyprwm/hypridle";
      ConditionEnvironment = "WAYLAND_DISPLAY";
    };
    serviceConfig = {
      ExecStart = "${lib.getExe pkgs.hypridle} -c  \"\${XDG_CONFIG_HOME}\"/hypr/hypridle.smartcard.conf";
      ExecStop = "${config.hjem.users.dc.files."bin/hypridle-kill".target}";
      # TODO: the system reaches smartcard target regardless of device status...
      # Restart = "on-failure"
      # RestartSec = 5;
    };
    environment.PATH = lib.mkForce null;
    environment.XDG_CONFIG_HOME = lib.mkForce null;
    conflicts = [
      "hypridle.service"
    ];
    requires = [ "smartcard.target" ];
    partOf = [ "graphical-session.target" ];
    wantedBy = [ "graphical-session.target" ];
    after = [
      "graphical-session.target"
      "hypridle.service"
    ];
  };

}
