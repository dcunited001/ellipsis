{ config, lib, pkgs, ... }: {
  # TODO: maybe remove gpg-agent conf from here.

  programs.gnupg = {
    agent = {
      enable = true;
      enableSSHSupport = true;
      pinentryPackage = pkgs.pinentry-qt;

      settings = {
        no-allow-external-cache = "";
        no-allow-mark-trusted = "";
        default-cache-ttl = 60;
        max-cache-ttl = 120;
        keep-display = "";
        # no-allow-emacs-pinentry = ""; # invalid, apparently
        no-allow-loopback-pinentry = "";
      };
    };
    dirmngr.enable = false;
  };
}
