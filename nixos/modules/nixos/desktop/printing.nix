{ pkgs, ... }: {
  # wheel can print. add other users to "lp" group
  services.printing = {
    enable = true;
    drivers = [ pkgs.epson-escpr ];

    # defaults
    defaultShared = false;
    openFirewall = false;
    browsing = false;

    # whether to receive printer configs over mdns
    browsed.enable = false;

    # CUPS web admin
    allowFrom = [ "127.0.0.1" ];
    listenAddresses = [ "127.0.0.1:631" ];
  };
}
