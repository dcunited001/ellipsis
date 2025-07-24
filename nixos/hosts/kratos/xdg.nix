{ config, lib, pkgs, ... }: {
  xdg.portal.enable = true;
  xdg.portal = {
    # this includes xdg-desktop-portal-hyprland twice
    # extraPortals = with pkgs; [ xdg-desktop-portal-hyprland ];
    xdgOpenUsePortal = true; # see nixos/nixpkgs#160923
  };

  environment.systemPackages = [ pkgs.gsettings-desktop-schemas ];
}
