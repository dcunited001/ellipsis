{
  pkgs,
  ...
}:
{
  # TODO: maybe remove gpg-agent conf from here.

  programs.gnupg = {
    agent = {
      enable = true;
      enableSSHSupport = true;

      # https://github.com/NixOS/nixpkgs/blob/eb6fbcf24f82bfef9c8cae5d2d4018fbe972fe8f/nixos/modules/programs/gnupg.nix#L215
      #
      # - doesn't end up in /run/current-system/sw/bin
      # - so if you override the GNUPGHOME/gpg-agent.conf, it's unavailable
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

  # TODO: check out openscap (NIST security reporting tool)
  # TODO: security.pam.p11.enable
  environment.systemPackages = [
    pkgs.pcsc-tools
    pkgs.opensc
    pkgs.pinentry-qt
  ];
}
