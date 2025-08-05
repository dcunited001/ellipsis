{ config, lib, pkgs, ... }: {
  # TODO: maybe remove gpg-agent conf from here.

  programs.gnupg = {
    agent = {
      enable = true;
      enableSSHSupport = true;
      pinentryPackage = pkgs.pinentry-qt;

      settings = {
        no-allow-external-cache = true;
        default-cache-ttl = 60;
        max-cache-ttl = 120;
        keep-display = true;

      };
    };
    dirmngr.enable = false;
  };
}
