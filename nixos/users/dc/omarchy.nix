{
  inputs,
  lib,
  pkgs,
  ...
}:
let
  omarchy-path = "$HOME/.nix-profile/share/omarchy";
  omarchy-quattro = (
    pkgs.callPackage (lib.custom.relativeToRoot "pkgs/common/omarchy-quattro/package.nix") { }
  );
  tensaku = (pkgs.callPackage (lib.custom.relativeToRoot "pkgs/common/tensaku/package.nix") { });

  ttfx = (pkgs.callPackage (lib.custom.relativeToRoot "pkgs/common/ttfx/package.nix") { });
in
{
  environment.sessionVariables.OMARCHY_PATH = omarchy-path;
  environment.sessionVariables.XDG_DATA_DIRS = [ "$HOME/.local/share/flatpak/exports/share" ];
  environment.pathsToLink = [ "/share/omarchy" ];

  # see also nixos/home/dc/xdg-terminal.nix

  users.users.dc.packages = [
    omarchy-quattro
    pkgs.quickshell
    pkgs.libxkbcommon # omarchy menu keybindings --print
    pkgs.bat
    # pkgs.bolt # boltctl
    # pkgs.crush # harnesses should be installed via mise
    pkgs.ffmpegthumbnailer
    # pkgs.foot
    pkgs.fzf
    pkgs.gtk3 # gtk-launch
    pkgs.gum
    # pkgs.herdr

    pkgs.hyprpicker
    pkgs.hyprsunset
    pkgs.imv
    pkgs.inotify-tools
    pkgs.inxi
    # pkgs.kdenlive
    # pkgs.lazydocker
    # pkgs.lazygit
    pkgs.libsecret # secret-tool
    pkgs.nautilus
    tensaku # probably need to set up a config
    ttfx
    pkgs.xdg-terminal-exec
    pkgs.wtype
  ];

  environment.sessionVariables.PATH = [
    "${omarchy-path}/bin"
  ];

  environment.systemPackages = [

  ];

  programs.localsend.enable = true;
  # programs.localsend.openFirewall = true;
  networking.firewall.interfaces.enp4s0 = {
    allowedTCPPorts = [ 53317 ];
    allowedUDPPorts = [ 53317 ];
  };
}
