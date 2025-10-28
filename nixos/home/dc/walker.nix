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
  elephantSystemD = false;
  hjemFiles = ./. + "../../../../gh/f";
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
    "bin/dwalker-man" = {
      # TODO: maybe add options, ensure man -k args re-quoted,
      #
      # - validate page exists, require < 2000 results
      # - fix less colors (when launched via UWSM) ... or just open with emacs.
      executable = true;
      text = ''
        #!/usr/bin/env bash
        manquery="$(walker --dmenu --keepopen -p "Query for man -k ...")"
        manpage="$(man -k "$manquery" | cut -d' ' -f1,2 | walker --dmenu -p "Open ..." | cut -f1 -d' ')"
        setsid uwsm app -- alacritty --class 'Alacritty:org.dc.tuitray' -T 'tuitray:man' -e man "$manpage"
      '';

      # probably more diffucult to handle errors
      # paste <(man -k "ls"  | sort | cut -d' ' -f2 | tr -d '()' ) \
      #       <(man -k "ls" | sort | cut -d' ' -f1,3-) | tr '\t' ' '
    };
    "bin/ofelephant" = {
      executable = true;
      text = ''
        #!/usr/bin/env bash
        lsof -p $(pgrep elephant | head -n1)
      '';
    };
    # ".screen/walker.screenrc".source = (hjemFiles + "/.screen/walker.screenrc");
  };

  # the default theme is also in ~/.config/walker/themes/default/{style.css,*.xml}
  #
  # walker module already does this:
  #
  # environment.etc = {
  #   "xdg/walker/config.toml" = lib.importTOML "./walker.toml";
  # };
  systemd.user.services.elephant = lib.mkIf elephantSystemD {
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

  systemd.user.sockets.elephant = lib.mkIf elephantSystemD {
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
