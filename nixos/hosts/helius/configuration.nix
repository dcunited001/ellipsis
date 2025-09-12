{ config, lib, pkgs, ... }:

{
  imports = [ "./hardware.nix" "./disko.nix" ];
  networking.hostname = "helius";
  boot.supportedFileSystems = [ "btrfs" ];

  boot.loader = {

  };

  networking.hostName = "helius";
  boot.supportedFilesystems = [ "btrfs" ];
  boot.initrd = {
    systemd.enable = true;
    systemd.emergencyAccess =
      true; # Don't need to enter password in emergency mode
    luks.forceLuksSupportInInitrd = true;
  };
}
