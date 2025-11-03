{
  inputs,
  config,
  lib,
  pkgs,
  ...
}:
let
  yktouch = pkgs.yubikey-touch-detector;
in
{

  # systemd.packages = [ pkgs.yubikey-touch-detector ];

  hjem.users.dc.systemd.services.yubikey-touch-detector = {
    path = [ pkgs.gnupg ];

    environment = {
      YUBIKEY_TOUCH_DETECTOR_LIBNOTIFY = builtins.toString true;
      YUBIKEY_TOUCH_DETECTOR_NOSOCKET = builtins.toString false;
      YUBIKEY_TOUCH_DETECTOR_VERBOSE = builtins.toString true;
    };

    serviceConfig = {
      ExecStart = "${lib.getExe yktouch}";
      Slice = [ "background-graphical.slice" ];
    };

    # NOTE: need yubikey-touch-detector to restart when i recycle gpg-agent
    # sockets. otherwise it gets a stale socket and complicated refreshing
    # gpg/sockets
    requires = [
      "yubikey-touch-detector.socket"
      "gpg-agent-ssh.socket"
    ];
    after = [
      "gpg-agent-ssh.socket"
      "yubikey-touch-detector.socket"
    ];
    wantedBy = [ "graphical-session.target" ];
    # partOf = [ "graphical-session.target" ];
  };

  hjem.users.dc.systemd.sockets.yubikey-touch-detector = {
    unitConfig = {
      Description = "Unix socket activation for YubiKey touch detector service";
    };

    socketConfig = {
      ListenStream = "%t/yubikey-touch-detector.socket";
      SocketMode = "660";
      RemoveOnStop = "yes";
      DirectoryMode = "0700";
    };

    # with PartOf=yubikey-touch-detector, stopping either the service or
    # socket will stop both (the socket file is also removed on stop)
    partOf = [ "yubikey-touch-detector.service" ];
    wantedBy = [ "sockets.target" ];
  };
}
