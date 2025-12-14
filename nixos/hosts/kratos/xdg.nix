{
  config,
  lib,
  pkgs,
  ...
}:
{
  # maybe use `exec-once dex -a ~/.config/autostart/{list,of}.desktop`
  xdg.autostart.enable = true;
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
    pkgs.adwaita-qt6
    pkgs.adwaita-icon-theme
    pkgs.gnome-themes-extra
    # too slow when searching on nixos (too many packages, GPU rendering?)
    # otherwise great
    # pkgs.kdePackages.plasma-sdk # env QT_STYLE_OVERRIDE= iconexplorer
    pkgs.pantheon.elementary-iconbrowser
  ];

  qt.enable = true;
  qt = {
    platformTheme = "qt5ct"; # gnome,gtk2,kde,kde6,lxqt,qt5ct
    style = "adwaita-dark";
  };
}
