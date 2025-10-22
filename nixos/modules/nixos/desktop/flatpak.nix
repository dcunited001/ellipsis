{
  config,
  lib,
  pkgs,
  ...
}:
{
  # services.flatpak.enable = true;

  # TODO: find a clever way to get this working for a single user
  #
  # see https://github.com/gmodena/nix-flatpak/blob/main/modules/nixos.nix
  systemd.user.services.flatpak-upgrade =
    let
      bin = lib.getExe pkgs.flatpak;
      command = "${bin} --noninteractive --user upgrade";
      # I really don't want this running unless it's my user
    in
    {
      Unit = {
        Description = "Flatpak Upgrade User Packages";
        StartLimitIntervalSec = "25m";
        StartLimitBurst = "4";
      };

      Service = {
        Environment = [
          # TODO: nix: ensure the environment is set
        ];
        Type = "oneshot";
        ExecStart = command;
        Restart = "on-failure";
        RestartSec = "2m";
      };
    };

  systemd.user.timers.flatpak-upgrade = {
    Unit.Description = "Flatpak Upgrade Timer";
    Install.WantedBy = [ "timers.target" ];
    Timer = {
      Unit = "rustic-backup.service";
      OnCalendar = "hourly"; # TODO: Make configurable!
    };
  };
}
