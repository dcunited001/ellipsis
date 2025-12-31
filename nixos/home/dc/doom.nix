{
  config,
  lib,
  pkgs,
  ...
}:
{

    unitConfig = {
      Description = "Doom Emacs Test";
      Documentation = "info:emacs man:emacs(1) https://gnu.org/software/emacs/";
      StartLimitIntervalSec = 300;
      StartLimitBurst = 3;
    };
    serviceConfig = {
      Type = "simple";
      ExecStart = ''/bin/sh -c "/home/dc/.config/guix/current/bin/guix shell -p $DOOMDIR/$GPROFILE -- emacs --init-directory=$EMACSDIR --fg-daemon=$EMACSSOCKET"'';
      ExecStop = ''/bin/sh -c "/home/dc/.config/guix/current/bin/guix shell -p $DOOMDIR/$GPROFILE -- emacsclient --socket=$EMACSSOCKET -e '(kill-emacs)'"'';
      # ExecStart = ''/bin/sh -c "/home/dc/.config/guix/current/bin/guix shell -p $DOOMDIR/$GPROFILE -- emacs --init-directory=$EMACSDIR --fg-daemon=doom"'';
      # ExecStop = ''/bin/sh -c "/home/dc/.config/guix/current/bin/guix shell -p $DOOMDIR/$GPROFILE -- emacsclient --socket=$EMACSSOCKET -e '(kill-emacs)'"'';
      TimeoutStartSec = 300;
      Restart = "on-failure";
      RestartSec = 30;
      SuccessExitStatus = 15;

      Slice = [ "app.slice" ];
      # Nice = -13;
      # CPUAffinity = "0-3";
    };

    # pinning CPUAffinity
    #
    # - seems to help a lot (after 2 minutes)... like a ton
    # - but all forked processes have identitcal CPUAffinity
    #

    environment.PATH = lib.mkForce null;
    environment.EMAIL = "aionfork@gmail.com";
    environment.SSH_AUTH_SOCK = "/run/user/1000/gnupg/S.gpg-agent.ssh";
    environment.EMACS = "/home/dc/.doom.d/.guix-profile/bin/emacs";
    environment.EMACSDIR = "/home/dc/.emacs.doom";
    environment.DOOMDIR = "/home/dc/.doom.d";
    environment.GPROFILE = ".guix-profile";
    # NOTE FIX SOCKET: environment.EMACSSOCKETPATH = "/run/user/1000/emacs/doom2";
    # environment.EMACSSOCKET = "doom";
    environment.EMACSSOCKET = "doom2";
    environment.EMACSSOCKETPATH = "/run/user/1000/emacs/doom2";
    # environment.EMACS_SOURCE = /data/ecto/emacs/emacs/src;

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
