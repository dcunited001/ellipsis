{
  inputs,
  config,
  lib,
  pkgs,
  ...
}:
let
  elephantPkg = inputs.elephant.packages.${pkgs.stdenv.system}.elephant-with-providers;
  walkerPkg = inputs.walker.packages.${pkgs.stdenv.system}.walker;
in
{
  programs.walker = {
    enable = true;
    package = walkerPkg;
    elephant.package = elephantPkg;
  };

  hjem.users.dc.files = {
    "bin/ofwalker" = {
      executable = true;
      text = ''
        #!/usr/bin/env bash
        lsof -p $(pgrep walker | head -n1)
      '';
    };
    "bin/ofelephant" = {
      executable = true;
      text = ''
        #!/usr/bin/env bash
        lsof -p $(pgrep elephant | head -n1)
      '';
    };
  };

  # the default theme is also in ~/.config/walker/themes/default/{style.css,*.xml}
  #
  # walker module already does this:
  #
  # environment.etc = {
  #   "xdg/walker/config.toml" = lib.importTOML "./walker.toml";
  # };
  systemd.user.services.elephant = {
    unitConfig = {
      Description = "Elephant Launcher and Indexer Service";
      Documentation = "https://github.com/abenz1257/elephant";
      Requires = [ "sockets.target" ];
      After = [ "graphical-session.target" ];
      PartOf = [ "graphical-session.target" ];
      ConditionEnvironment = "WAYLAND_DISPLAY";
    };
    serviceConfig = {
      ExecStart = "${elephantPkg}/bin/elephant"; # "${lib.optionalString elephantCfg.debug "--debug"}";
      # ExecReload = "${elephantCfg.package}/bin/elephant ${}";

      Restart = "on-failure";
      RestartSec = 5;
    };
  };

  systemd.user.sockets.elephant = {
    unitConfig = {
      Description = "Elephant Launcher and Indexer Socket";
      Documentation = "https://github.com/abenz1257/elephant";
    };
    socketConfig = {
      # l, err := net.ListenUnix("unix", &net.UnixAddr{ ... }; # SOCK_STREAM =~ "unix"
      #
      # https://github.com/abenz1267/elephant/blob/master/internal/comm/comm.go#L41
      ListenStream = "%t/elephant/elephant.sock";
      SocketMode = "0600";
      Service = "elephant.service"; # redundant
      DirectoryMode = "0700";
    };
    wantedBy = [ "sockets.target" ];
  };

}

# elephantPkgs = inputs.elephant.packages.${pkgs.stdenv.system};
