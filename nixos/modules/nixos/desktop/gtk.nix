{ config, pkgs, ... }:
let
  gschema = pkgs.gsettings-desktop-schemas;
in
{
  xdg.icons.enable = true;

  # also, dconf.profiles, dconf.packages
  # https://github.com/NixOS/nixpkgs/blob/nixos-25.11/nixos/modules/programs/dconf.nix#L230-L257
  programs.dconf.enable = true;
  environment.systemPackages = [
    pkgs.glib
    pkgs.gsettings-desktop-schemas
    pkgs.nordzy-icon-theme
    pkgs.adwaita-icon-theme # includes 16x16 svg's only
    pkgs.adwaita-icon-theme-legacy
  ];

  gtk.iconCache.enable = true;

  # otherwise gsettings-schemas isn't linked in /run/current-system/sw/share
  environment.pathsToLink = [
    # "/libexec" # for polkit? see mic92
    "/share/gsettings-schemas" # for XDG_DATA_DIRS
  ];

  environment.sessionVariables.XDG_DATA_DIRS = [
    # -> "/run/current-system/sw/share/gsettings-schemas/gsettings-desktop-schemas-49.1"
    "${gschema}/share/gsettings-schemas/${gschema.name}"
  ];
}

# =============================================
# GTK setup without home-manager:
#
# - it's weird. I set this up on guix.
# - there, the icon search seemed to fallthrough to what's available
# - but on this system, only finds current theme's icons. idk? maybe wrong
# - different apps have different missing icons
# - setting dconf fixed missing/inconsistent icons, but not fallthrough
# - getting gsettings-desktop-schemas to work is definitely a hack
#
# but you can't be serious: my notification scripts need arbitrary icon id's?
# that are unique to the icon theme? damit

# ----------------------------------
# /run/currrent-system/lib/gtk-{3,4}.0: point to FCITX gtk libs

# i have a weird setup for that (because no home-manager)

# ----------------------------------
# ~/.config/gtk-3.0/settings.ini
#
# - created around install time, but it's a file

# ~/.config/gtk-4.0/gtk{.css,-dark.css}
#
# - two links to Nordic-darker.* in /run/current-system/sw/share/themes...
# - but idk how they got there. never updated/replaced since install time

# ----------------------------------
# gsettings schema setup

# directly to dconf
#
# dconf write /org/gnome/desktop/interface/icon-theme 'Nordzy'

# budimanjojo sets these from a script ./bin/configure-gtk script (XDG env not persisted)
#
# https://github.com/budimanjojo/dotfiles/blob/2462a4ebe229e6321d6c39448e43dfe78c73389b/packages/configure-gtk/default.nix#L20

# via gsettings
#
# gsettings set org.gnome.desktop.interface gtk-theme $gtkTheme
# gsettings set org.gnome.desktop.interface icon-theme $gtkIcons
# gsettings set org.gnome.desktop.interface cursor-theme $gtkCursors
# gsettings set org.gnome.desktop.interface font-name $gtkFont            # 'UbuntuMono Nerd Font 12'
# gsettings set org.gnome.desktop.wm.preferences button-layout ':appmenu' # :appmenu

# Mic92 sets these before launching sway
#
# https://github.com/Mic92/dotfiles/blob/c1ac0c4314b32c722dc6a0d3960a74f46326120a/nixosModules/sway.nix#L82
