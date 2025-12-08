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
    nh = prev.nh.overrideAttrs {
      # src = inputs.nixpkgs.legacyPackages.${pkgs.stdenv.hostPlatform.system}.fetchFromGithub {
      src = inputs.nixpkgs.legacyPackages.${prev.stdenv.hostPlatform.system}.fetchFromGitHub {
        owner = "nix-community";
        repo = "nh";
        rev = "master";
        sha256 = "sha256-V7lRjvq6vFq0hR4fNYFznahMo/NayVHK18ELKL4Q0To=";
      };
    };
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
