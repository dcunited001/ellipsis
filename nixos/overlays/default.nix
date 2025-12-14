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

  # apparantly a very bad way to do this;
  #
  # hyprland-packages = final: prev: {
  #   # hyprland = import inputs.hyprland.packages.${prev.stdenv.hostPlatform.system} {
  #   hyprland = import "${inputs.hyprland.outPath}/nix" {
  #     inherit (final) system;
  #     config.allowUnfree = true;
  #     overlays = [
  #       (hyprland_final: hyprland_prev: {
  #         hyprlang = hyprland_prev.hyprlang.overrideAttrs (
  #           previousAttrs:
  #           let
  #             version = "0.6.6";
  #           in
  #           rec {
  #             inherit version;
  #             src = prev.fetchFromGitHub {
  #               owner = "hyprwm";
  #               repo = "hyprlang";
  #               rev = "0.6.6";
  #               sha256 = "sha256-AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA=";
  #             };
  #           }
  #         );
  #       })
  #     ];
  #   };
  # };
in
{
  default =
    final: prev:
    (additions final prev) // (modifications final prev) // (linuxModifications final prev);
  # // (hyprland-packages final prev);
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
