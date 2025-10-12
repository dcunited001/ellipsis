{ config, lib, pkgs, ... }: {
  services.avahi = { 
    denyInterfaces = [ "enp5s0" "ztugatqaer" ];
    ipv4 = true;
    nssmdns4 = true;
    ipv6 = false;
    nssmdns6 = false;
    publish = {
      enable = false;
      workstation = false;
      addresses = false;
    };
    wideArea = false;
    allowPointToPoint = false;
  };
}
