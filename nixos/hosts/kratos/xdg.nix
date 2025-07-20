{ config, lib, pkgs, ... }:
  # some basic configuration for all users should go here
{
  xdg.portal.enable = true;
  xdg.portal = {
    # this includes xdg-desktop-portal-hyprland twice
    # extraPortals = with pkgs; [ xdg-desktop-portal-hyprland ];
    xdgOpenUsePortal = true; # see nixos/nixpkgs#160923
  };
}

  # xdg =  {
  #   autostart.enable = true; # default
  #   menus.enable = true; # default
  #   sounds.enable = true; # default
  #
  #   icons.enable = true; # default
  #   icons = { fallbackCursorThemes = [...] };
  #
  #   mime.enable = true; # default
  #   mime = {
  #     addedAssociations = { ... };
  #     removedAssociations = <repeated>;
  #     defaultAssociations = <repeated>;
  #   };
  #
  #   portal.enable = true; # default
  #   # oof, the user's desktop portal is tightly coupled to the system
  #   portal.configPackages = [ pkgs.hyprland ] # default
  #   portal.xdgOpenUsePortal = false;
  #   portal.extraPortals = [ pkgs.xdg-desktop-portal-hyprland
  #                           pkgs.xdg-desktop-portal-hyprland # twice?
  #                           pkgs.xdg-desktop-portal-gtk ];
  #   portal.wlr  = { enable = false; settings = [ ... ] }; # defaults
  #   portal.lxqt = { enable = false; styles   = [ ... ] }; # defaults
  #   terminal-exec.enable = false; # default
  #   terminal-exec = {
  #     package = nixdrv;
  #     settings = {};
  #   };
  # }
