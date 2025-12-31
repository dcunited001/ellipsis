{
  config,
  lib,
  pkgs,
  ...
}:
{

  hjem.users.dc.systemd = {
    services."ectorepo-sync@" = {
      unitConfig = {
        Description = "Repo Sync Service Template";
        Slice = [ "background.slice" ];
      };
      serviceConfig = {
        Type = "oneshot";
        ExecStart = ''/bin/sh -c "repo sync"'';
        WorkingDirectory = "/data/ecto/%i";
        NoNewPriviledges = true; # hmmm. yes i definitely want that (a lot of random sources)
        TimeoutSec = 60;
        Restart = "no";
      };
      environment.PATH = lib.mkForce null;
    };
  };

  # timers."ectorepo-sync@" = {
  #   Unit.Description = "Repo Sync Timer";
  #   Install.WantedBy = [ "timers.target" ];
  #   Timer = {
  #     Unit = "ecto.service";
  #     OnCalendar = "daily";
  #   };
  # };

  # hjem.users.dc.files = {
  #   "bin/ectorepo-sync" = {
  #     executable = true;
  #     text = ''
  #       #!/usr/bin/env bash

  #     '';
  #   };
  # };
}
