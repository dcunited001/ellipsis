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
in
{
  environment.sessionVariables.OMARCHY_PATH = omarchy-path;
  environment.pathsToLink = [ "/share/omarchy" ];

  users.users.dc.packages = [
    omarchy-quattro
    pkgs.quickshell
    pkgs.libxkbcommon # omarchy menu keybindings --print
    pkgs.bat
    # pkgs.bolt # boltctl
    pkgs.ffmpegthumbnailer
    # pkgs.foot
    # pkgs.fzf
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
    pkgs.wtype
    tensaku # probably need to set up
  ];

  environment.sessionVariables.PATH = [
    "${omarchy-path}/bin"
  ];

}
