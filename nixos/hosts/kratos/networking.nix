{
  config,
  lib,
  pkgs,
  ...
}:
{
  services.avahi = {
    allowInterfaces = [
      "enp4s0"
    ];
    denyInterfaces = [
      "enp5s0"
      "ztugatqaer"
    ];
    ipv4 = true;
    nssmdns4 = true;
    ipv6 = false;
    nssmdns6 = false;
    publish = {
      enable = true;
      workstation = false;
      addresses = false;
    };
    wideArea = false;
    allowPointToPoint = false;
  };

  # for split-brane dns ... idk that this is sufficient
  services.resolved.enable = true;
  services.resolved = {
    settings.Resolve.DNS = config.networking.nameservers;
  };

  networking.hostName = "kratos";
  networking.networkmanager.enable = true;
  networking.networkmanager = {
    dns = "systemd-resolved";
    plugins = [ pkgs.networkmanager-openvpn ];
  };
}
