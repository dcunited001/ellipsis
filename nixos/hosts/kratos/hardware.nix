{ config, lib, pkgs, modulesPath, ... }:

{
  imports = [ (modulesPath + "/installer/scan/not-detected.nix") ];

  boot.initrd.availableKernelModules =
    [ "nvme" "xhci_pci" "ahci" "usbhid" "usb_storage" "sd_mod" ];
  boot.initrd.kernelModules = [ "dm-snapshot" ];

  fileSystems."/" = {
    device = "/dev/disk/by-uuid/415478b1-89ab-4851-96c5-9a9bc0ebbceb";
    fsType = "btrfs";
    options = [ "subvol=@" ];
  };

  fileSystems."/nix" = {
    device = "/dev/disk/by-uuid/415478b1-89ab-4851-96c5-9a9bc0ebbceb";
    fsType = "btrfs";
    options = [ "subvol=@nix" ];
  };

  fileSystems."/var" = {
    device = "/dev/disk/by-uuid/415478b1-89ab-4851-96c5-9a9bc0ebbceb";
    fsType = "btrfs";
    options = [ "subvol=@var" ];
  };

  fileSystems."/var/log" = {
    device = "/dev/disk/by-uuid/415478b1-89ab-4851-96c5-9a9bc0ebbceb";
    fsType = "btrfs";
    options = [ "subvol=@log" ];
  };

  fileSystems."/var/tmp" = {
    device = "/dev/disk/by-uuid/415478b1-89ab-4851-96c5-9a9bc0ebbceb";
    fsType = "btrfs";
    options = [ "subvol=@tmp" ];
  };

  fileSystems."/var/cache" = {
    device = "/dev/disk/by-uuid/415478b1-89ab-4851-96c5-9a9bc0ebbceb";
    fsType = "btrfs";
    options = [ "subvol=@cache" ];
  };

  fileSystems."/boot" = {
    device = "/dev/disk/by-uuid/A4BD-1B14";
    fsType = "vfat";
    options = [ "fmask=0022" "dmask=0022" ];
  };

  fileSystems."/data" = {
    device = "/dev/disk/by-uuid/616d45d0-155e-47bd-96be-1ec0b92401bc";
    fsType = "ext4";
  };

  fileSystems."/steam" = {
    device = "/dev/disk/by-uuid/59707d1b-d5a6-481f-8d4e-422fc3afa11c";
    fsType = "ext4";
  };

  fileSystems."/data/vm" = {
    device = "/dev/disk/by-uuid/4a7e3836-2a8b-4eab-a01a-b596acce77cc";
    fsType = "ext4";
  };

  fileSystems."/home" = {
    device = "/dev/disk/by-uuid/9cedc0b0-344f-476c-bae1-20b7993b1a6d";
    fsType = "btrfs";
  };

  fileSystems."/gnu" = {
    device = "/dev/disk/by-uuid/415478b1-89ab-4851-96c5-9a9bc0ebbceb";
    fsType = "btrfs";
    options = [ "subvol=@guix" ];
  };

  swapDevices = [ ];

  # Enables DHCP on each ethernet and wireless interface. In case of scripted networking
  # (the default) this is the recommended approach. When using systemd-networkd it's
  # still possible to use this option, but it's recommended to use it in conjunction
  # with explicit per-interface declarations with `networking.interfaces.<interface>.useDHCP`.
  networking.useDHCP = lib.mkDefault true;
  # networking.interfaces.enp4s0.useDHCP = lib.mkDefault true;
  # networking.interfaces.enp5s0.useDHCP = lib.mkDefault true;

  nixpkgs.hostPlatform = lib.mkDefault "x86_64-linux";
  hardware.cpu.amd.updateMicrocode =
    lib.mkDefault config.hardware.enableRedistributableFirmware;
}
