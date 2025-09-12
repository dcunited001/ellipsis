{ config, lib, pkgs, ... }: {
  virtualisation.containers.enable = true;
  virtualisation.containers.storage.settings.storage = { driver = "btrfs"; };
  virtualisation.docker.enable = false;
  virtualisation.podman.enable = true;
  virtualisation.podman = {
    autoPrune.enable = true;
    dockerCompat = true;
    networkSocket.enable = false;
    defaultNetwork.settings.dns_enabled = true;
  };

  virtualisation.oci-containers.backend = "podman";

  environment.systemPackages = [ pkgs.podman-compose ];
}
