{
  config,
  lib,
  pkgs,
  ...
}:

{

  hjem.users.dc.systemd.services.emacs-hop = {
    unitConfig = {
      Description = "Emacs Hop";
      Documentation = "info:emacs man:emacs(1) https://gnu.org/software/emacs/";
      StartLimitIntervalSec = 300;
      StartLimitBurst = 3;
    };
    serviceConfig = {
      Type = "simple";
      ExecStart = "/bin/sh -c \"/etc/profiles/per-user/dc/bin/emacs --init-directory=$EMACSDIR";
      ExecStop = "/bin/sh -c \"/etc/profiles/per-user/dc/bin/emacsclient --socket=hop -e '(kill-emacs)'\"";
      TimeoutStartSec = 300;
      Restart = "on-failure";
      RestartSec = 30;
      SuccessExitStatus = 15;

      Slice = [ "app.slice" ];
    };

    # TODO: how to ensure that other environment vars are correct?
    environment.PATH = lib.mkForce null;
    environment.EMAIL = "aionfork@gmail.com";
    environment.SSH_AUTH_SOCK = "/run/user/1000/gnupg/S.gpg-agent.ssh";
    environment.EMACSDIR = "/home/dc/.emacs.d";

    # environment.EMACSSOCKETPATH = "/run/user/1000/emacs";
    # environment.EMACS = "/usr/bin/emacs";

    after = [ "graphical-session.target" ];
    wantedBy = [ "graphical-session.target" ];
    requires = [ "emacs-hop.socket" ];
  };

  hjem.users.dc.systemd.sockets.emacs-hop = {
    unitConfig = {
      Description = "Socket for Emacs Hop";
      Documentation = "info:emacs man:emacs(1) https://gnu.org/software/emacs/";
    };
    socketConfig = {
      ListenSocket = "%t/emacs/hop.socket";
      SocketMode = "0700";
      Service = "emacs-hop.service";
      DirectoryMode = "0700";
    };
    partOf = [ "emacs-hop.service" ];
    wantedBy = [ "sockets.target" ];
  };
}
