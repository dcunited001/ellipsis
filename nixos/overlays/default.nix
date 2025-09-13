# This file defines overlays/custom modifications to upstream packages
#

{ inputs, ... }:

let
  additions = final: prev:
    (prev.lib.packagesFromDirectoryRecursive {
      callPackage = prev.lib.callPackageWith final;
      directory = ../pkgs/common;
    });

  linuxModifications = final: prev: prev.lib.mkIf final.stdenv.isLinux { };

in {
  default = final: prev:
    (additions final prev) // (linuxModifications final prev);
}
