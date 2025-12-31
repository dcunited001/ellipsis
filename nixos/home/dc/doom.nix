{
  config,
  lib,
  pkgs,
  ...
}:
{

  hjem.users.dc.systemd.services."doom2" = {
    unitConfig = {
      Description = "Doom Emacs 2";
      Documentation = "info:emacs man:emacs(1) https://gnu.org/software/emacs/";
      StartLimitIntervalSec = 300;
      StartLimitBurst = 3;
    };
    serviceConfig = {
      Type = "simple";
      Slice = [ "app.slice" ];
      ExecStart = ''/bin/sh -c "/home/dc/.config/guix/current/bin/guix shell -p $DOOMDIR/$GPROFILE -- emacs --init-directory=$EMACSDIR --fg-daemon=\"$EMACSSOCKET\""'';
      ExecStop = ''/bin/sh -c "/home/dc/.config/guix/current/bin/guix shell -p $DOOMDIR/$GPROFILE -- emacsclient --socket=\"$EMACSSOCKET\" -e '(kill-emacs)'"'';
      # ExecStart = ''/bin/sh -c "/home/dc/.config/guix/current/bin/guix shell -p $DOOMDIR/$GPROFILE -- emacs --init-directory=$EMACSDIR --fg-daemon=doom"'';
      # ExecStop = ''/bin/sh -c "/home/dc/.config/guix/current/bin/guix shell -p $DOOMDIR/$GPROFILE -- emacsclient --socket=$EMACSSOCKET -e '(kill-emacs)'"'';

      SuccessExitStatus = 15;
      Restart = "on-failure";
      RestartSec = 30;
      TimeoutStartSec = 300;

      Nice = -13;
      # CPUAffinity = "0-3";
    };

    environment.PATH = lib.mkForce null;
    environment.EMAIL = "aionfork@gmail.com";
    environment.SSH_AUTH_SOCK = "/run/user/1000/gnupg/S.gpg-agent.ssh";
    environment.EMACS = "/home/dc/.doom.d/.guix-profile/bin/emacs";
    environment.EMACSDIR = "/home/dc/.emacs.doom";
    environment.DOOMDIR = "/home/dc/.doom.d";
    environment.GPROFILE = ".guix-profile";
    # TODO fix socket (keep as such for now, until I've tested)
    # environment.EMACSSOCKETPATH = "/run/user/1000/emacs/doom2";
    # environment.EMACSSOCKET = "doom";
    environment.EMACSSOCKET = "doom2";
    environment.EMACSSOCKETPATH = "/run/user/1000/emacs/doom2";
    environment.EMACS_SOURCE = "/data/ecto/emacs/emacs/src";

    after = [ "graphical-session.target" ];
    wantedBy = [ "graphical-session.target" ];
    # requires = [ "doom.socket" ];
  };

  # hjem.users.dc.systemd.sockets.doom = {
  #   unitConfig = {
  #     Description = "Socket for Doom Emacs";
  #     Documentation = "info:emacs man:emacs(1) https://gnu.org/software/emacs/";
  #   };
  #   socketConfig = {
  #     ListenSocket = "%t/emacs/doom.socket";
  #     SocketMode = "0700";
  #     Service = "doom.service";
  #     DirectoryMode = "0700";
  #   };
  #   partOf = [ "doom.service" ];
  #   wantedBy = [ "sockets.target" ];
  # };
}
