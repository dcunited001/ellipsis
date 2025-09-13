{ config, lib, pkgs, ... }: {
  virtualisation.containers.enable = true;
  virtualisation.containers.storage.settings.storage = {
    driver = "btrfs";
    # runroot = "/run/containers/storage";
    # graphroot = "/data/vm/pod/root/containers/storage";
    # rootless_storage_path = "$HOME/.local/share/containers/storage"
    # rootless_storage_path = "/data/vm/pod/$USER/containers/storage";
  };

  virtualisation.containers.containersConf.settings = {
    containers.dns_servers = [ "8.8.8.8" "8.8.4.4" ];
  };
}
