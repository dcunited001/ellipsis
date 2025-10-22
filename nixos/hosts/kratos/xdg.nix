{
  config,
  lib,
  pkgs,
  ...
}:
{
  xdg.portal.enable = true;
  xdg.portal = {
    # this includes xdg-desktop-portal-hyprland twice
    # extraPortals = with pkgs; [ xdg-desktop-portal-hyprland ];
    xdgOpenUsePortal = true; # see nixos/nixpkgs#160923
  };

  # run nwg-look and apply. theme files aren't in dotfiles
  environment.systemPackages = [
    pkgs.gsettings-desktop-schemas
    pkgs.nordic
    pkgs.adwaita-qt
    pkgs.gnome-themes-extra
  ];

  qt.enable = true;
  qt = {
    platformTheme = "qt5ct"; # gnome,gtk2,kde,kde6,lxqt,qt5ct
    style = "adwaita-dark";
  };
}
