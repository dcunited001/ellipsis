# This file defines overlays/custom modifications to upstream packages
#

{ inputs, ... }:

let
  additions =
    final: prev:
    (prev.lib.packagesFromDirectoryRecursive {
      callPackage = prev.lib.callPackageWith final;
      directory = ../pkgs/common;
    });

  linuxModifications = final: prev: prev.lib.mkIf final.stdenv.isLinux { };

  modifications = final: prev: {
    # hyprland = prev.hyprland.overrideAttrs {
    #   mesa = final.unstable.mesa;
    # };
    # steam = prev.steam.overrideAttrs {
    #   mesa = final.unstable.mesa;
    # };
    #
  };
in
{
  default =
    final: prev:
    (additions final prev) // (modifications final prev) // (linuxModifications final prev);
}

# stable-packages = final: prev: {
#   stable = import inputs.nixpkgs-stable {
#     inherit (final) system;
#     config.allowUnfree = true;

#     overlays = [ ];
#   };
# };

# unstable-packages = final: prev: {
#   unstable = import inputs.nixpkgs-unstable {
#     inherit (final) system;
#     config.allowUnfree = true;
#   };
# };
