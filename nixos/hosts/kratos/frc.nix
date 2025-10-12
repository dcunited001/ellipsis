{ config, lib, pkgs, ... }: {
  services.photonvision.enable = true;
  services.photonvision = {
    openFirewall = true;
  };
}
