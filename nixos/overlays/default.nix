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

  stable-packages = final: prev: {
    stable = import inputs.nixpkgs-stable {
      inherit (final) system;
      config.allowUnfree = true;

      # https://discourse.nixos.org/t/intel-ax210-ap-mode/66087/9
      overlays = [
        (final: prev: {
          hostapd = prev.hostapd.overrideDerivation (old: {
            patches = (old.patches or [ ]) ++ [
              (final.fetchpatch {
                url = "https://tildearrow.org/storage/hostapd-2.10-lar.patch";
                #url = "https://github.com/randomizedcoder/tildearrow_hostapd_patch/hostapd-2.10-lar.patch";
                sha256 = "USiHBZH5QcUJfZSxGoFwUefq3ARc4S/KliwUm8SqvoI=";
              })
            ];
          });
        })
      ];
    };
  };
  unstable-packages = final: prev: {
    unstable = import inputs.nixpkgs-unstable {
      inherit (final) system;
      config.allowUnfree = true;
    };
  };

in {
  default = final: prev:

    (additions final prev)
    # // (modifications final prev)
    // (linuxModifications final prev) // (stable-packages final prev)
    // (unstable-packages final prev);
}
