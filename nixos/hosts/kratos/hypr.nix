{ config, lib, pkgs, ... }:

{
  programs = {
    uwsm = {
      waylandCompositors = {
        # TODO: Fix hyprland-debug, doesn't get launched by uwsm
        hyprland-debug = {
          prettyName = "hyprland-debug";
          binPath = "/run/current-system/sw/bin/Hyprland";
          comment = "Run Hyprland with env-hyprland-debug";
        };
      };
    };

    hyprland = {
      enable = true;
      withUWSM = true; # recommended for most users
      xwayland.enable = true; # Xwayland can be disabled

      # TODO: fixed broken links in hyprland. adds system bin dir
      systemd.setPath.enable = true;
    };

    # TODO: set dconf.profiles.user.databases?
    # https://wiki.hypr.land/Nix/Hyprland-on-NixOS/#fixing-problems-with-themes
    dconf.enable = true;
    seahorse.enable = true;
    neovim.enable = true; # defaultEditor = true;
  };

  # hints electron apps to use wayland
  environment.sessionVariables.NIXOS_OZONE_WL = "1";

  # TODO: hypr: move HYPRHOST into a hypr-module? or home-manager?
  # requires: hyprlang >= 0.6.4 (and hyprutils >= 0.8.1)
  environment.sessionVariables.HYPRHOST = "kratos";
  environment.sessionVariables.HYPRHOSTKRATOS = "kratos";

  # ---------------------------------------------
  # HYPRIDLE & HYPRLOCK

  # hyprlock is run via hypridle (via uwsm app ... via exec-onde in autostarts)
  security.pam.services.hyprlock = { };

  environment.systempackages = [ pkgs.hypridle pkgs.hyprland ];
}
