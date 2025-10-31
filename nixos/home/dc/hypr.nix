{
  inputs,
  config,
  lib,
  pkgs,
  ...
}:
let
  hjemFiles = ./. + "../../../../gh/f";
  dc = "dc";
in
{

  systemd.user.services.fumon = {
    unitConfig = {
      Description = "Failed unit monitor";
      Documentation = "man:fumon(1) man:busctl(1)";
      Requisite = "graphical-session.target";
    };
    serviceConfig = {
      Type = "exec";
      ExecCondition = "/bin/sh -c \"command -v notify-send > /dev/null\"";
      ExecStart = "fumon";
      Restart = "on-failure";
      Slice = [ "background-graphical.slice" ];
    };

    environment.PATH = lib.mkForce null;
    after = [ "graphical-session.target" ];
    wantedBy = [ "graphical-session.target" ];
  };

}
