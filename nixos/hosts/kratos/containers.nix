{ config, lib, pkgs, ... }: {
  virtualisation.containers.enable = true;
  virtualisation.containers.storage.settings.storage = {
    driver = "overlay"; # just use overlayfs; files are large anyways.
    # runroot = "/run/containers/storage";
    # graphroot = "/data/vm/pod/root/containers/storage";
    # rootless_storage_path = "$HOME/.local/share/containers/storage"
    # rootless_storage_path = "/data/vm/pod/$USER/containers/storage";
  };

  virtualisation.containers.containersConf.settings = {
    containers.dns_servers = [ "8.8.8.8" "8.8.4.4" ];
  };

  # hm.custom.persist = {
  #   home.cache = {
  #     directories = [ ".local/share/containers" ];
  #   };
  # };

  # virtualisation.oci-containers.containers = {
  #   test = {
  #     autoStart = true;
  #     # your rootless user name (the user must exist in the system).
  #     podman = { user = "nix"; sdnotify = "conmon"; }; # conmon/healthy/container
  #     image = "docker.io/hello-world";
  #     login = { username, passwordFile, registry }; # registry login
  #     ports = [];
  #
  #   };

  # ---------------------------------------------
  # defaults
  #
  # virtualisation.containers.registries.insecure = ["docker.io" "quay.io"];
  # virtualisation.containers.policy = {
  #   default = [{ type = "insecureAcceptAnything"; }];
  #   transports = {
  #     docker-daemon = { "" = [{ type = "insecureAcceptAnything"; }]; };
  #   };
  # };

  # ---------------------------------------------
  # remote access
  #
  # virtualisation.podman.networkSocket
  # virtualisation.podman.networkSocket.server = "ghostunnel" # envoy for podman
  #

  # ---------------------------------------------
  # devices
  #
  # # for nvidia GPU
  # hardware.nvidia-container-toolkit.enable = true
}
