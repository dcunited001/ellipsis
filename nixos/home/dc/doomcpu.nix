{
  config,
  lib,
  pkgs,
  ...
}:
{

  # This is a template service for doom emacs. I'm not using this for now, and
  # the template %i instancing probably isn't great.
  #
  # - Emacs may benefit a ton from cache locality
  # - problems, esp. with templating: multiple servers want to open/read/write
  #   to common files (like $EMACSDIR/.local). potentially nasty problems
  #   there. a separate build/profile is needed :(

  # startup
  #
  # - systemctl start doomcpu@28-31
  # - doomclient -s doomcpu-28-31
  # - systemctl --user show --property CPUAffinity # CPUAffinity=28-31
  # - btop
  #
  # Then move somthing heavy & check the cores
  #
  # - C-u M-s M-r RET /data/ecto # search everything not compressed, page & open buffers

  # pinning CPUAffinity
  #
  # - seems to help a lot (after 2 minutes)... like a ton
  # - but all forked processes have identitcal CPUAffinity (kinda sucks)
  # - must set CPUAffinity before service starts

  # - IDEA... idk set up OverlayFS mount that contains the doomemacs
  #   nativecomp build. that is, build doom emacs and "export" the build into
  #   the mounted overlay (in memory only, so multple servers aren't stomping
  #   on each other)

  hjem.users.dc.systemd.services."doomcpu@" = {
    unitConfig = {
      Description = "Doom Emacs CPU %i";
      Documentation = "info:emacs man:emacs(1) https://gnu.org/software/emacs/";
      StartLimitIntervalSec = 300;
      StartLimitBurst = 3;
    };
    serviceConfig = {
      Type = "simple";
      ExecStart = ''/bin/sh -c "/home/dc/.config/guix/current/bin/guix shell -p $DOOMDIR/$GPROFILE -- emacs --init-directory=$EMACSDIR --fg-daemon=\"$EMACSSOCKET\""'';
      ExecStop = ''/bin/sh -c "/home/dc/.config/guix/current/bin/guix shell -p $DOOMDIR/$GPROFILE -- emacsclient --socket=\"$EMACSSOCKET\" -e '(kill-emacs)'"'';
      # ExecStart = ''/bin/sh -c "/home/dc/.config/guix/current/bin/guix shell -p $DOOMDIR/$GPROFILE -- emacs --init-directory=$EMACSDIR --fg-daemon=doom"'';
      # ExecStop = ''/bin/sh -c "/home/dc/.config/guix/current/bin/guix shell -p $DOOMDIR/$GPROFILE -- emacsclient --socket=$EMACSSOCKET -e '(kill-emacs)'"'';
      TimeoutStartSec = 300;
      Restart = "on-failure";
      RestartSec = 30;
      SuccessExitStatus = 15;

      Slice = [ "app.slice" ];
      Nice = -13;
      CPUAffinity = "%i";
    };

    environment.PATH = lib.mkForce null;
    environment.EMAIL = "aionfork@gmail.com";
    environment.SSH_AUTH_SOCK = "/run/user/1000/gnupg/S.gpg-agent.ssh";
    environment.EMACS = "/home/dc/.doom.d/.guix-profile/bin/emacs";
    environment.EMACSDIR = "/home/dc/.emacs.doom";
    environment.DOOMDIR = "/home/dc/.doom.d";
    environment.GPROFILE = ".guix-profile";
    # NOTE FIX SOCKET: environment.EMACSSOCKETPATH = "/run/user/1000/emacs/doom2";
    # environment.EMACSSOCKET = "doom";
    environment.EMACSSOCKET = "doomcpu-%i";
    environment.EMACSSOCKETPATH = "/run/user/1000/emacs/doomcpu-%i";
    # environment.EMACS_SOURCE = /data/ecto/emacs/emacs/src;

    after = [ "graphical-session.target" ];
    wantedBy = [ "graphical-session.target" ];
  };
}
