{
  inputs,
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.home.dc.waybar;

in
{

  options.dc.home.waybar = {
    enable = lib.mkEnableOption "waybar, a highly customizable Wayland bar for Sway and Wlroots based compositors";
  };

  config = lib.mkIf cfg.enable {

    # omg jsonc lol
    # https://gist.github.com/mkows/60203538829f52f43834940c19f492f4
    environment.systemPackages = [
      pkgs.jq
      pkgs.jsmin
    ];

    # hjem.users.dc.files = {
    #   "bin/waybar-config-merge" = {
    #     text = ''
    #       #!/usr/bin/env bash
    #       jqQuery = "jq -s 'reduce .[] as $item ({}; . * $item)'";

    #       declare -a waybarjson;
    #       waybarjson=(${cfg.waybarJson});
    #       waybarcfghome=$XDG_CONFIG_HOME/waybar
    #       ${lib.getExe pkgs.jq} <(${lib.getExe pkgs.jsmin}) \$\{waybarjson[@]}

    #     '';
    #   };
    # };

    hjem.users.dc.systemd.services.waybar = {
      unitConfig = {
        Description = "Highly customizable Wayland bar for Sway and Wlroots based compositors";
        Documentation = "https://github.com/Alexays/Waybar/wiki/";
      };
      environment.PATH = lib.mkForce null;

      serviceConfig = {
        # ExecStartPre = "#{lib.getExe pkgs.jq}";
        ExecStart = "${lib.getExe pkgs.waybar}";
        ExecReload = "kill -SIGUSR2 $MAINPID";
        Slice = [ "app.slice" ];
      };

      requisite = [ "graphical-session.target" ];
      wantedBy = [ "graphical-session.target" ];
    };
  };
}
