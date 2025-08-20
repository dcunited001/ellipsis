{ pkgs, ... }: {
  # TODO: relocate boolean args to toggle the module
  # - lib.mkIf cfg.guixEnable true ... or something
  # - expose extraArgs

  # defaults
  # extraArgs = [];
  # storeDir = "/gnu/store";
  # group = "guixbuild";
  # nrBuildUsers = 10;

  services.guix.enable = true;
  services.guix = {
    stateDir = "/gnu/var";

    # does this need to be a derivation?
    substituters = {
      urls = [
        "https://bordeaux.guix.gnu.org"
        "https://ci.guix.gnu.org"
        "https://berlin.guix.gnu.org"
        "https://substitutes.nonguix.gnu.org"
      ];
      authorizedKeys = let
        r = "https://raw.githubusercontent.com/dcunited001/ellipsis";
        c = "3fc27f4c711ce6507f7a1d6cd4fdcbb4de03a8f7";
        p = "nixos/modules/services/guix";
      in [
        (builtins.fetchurl {
          url = "${r}/${c}/${p}/bordeaux.guix.gnu.org.pub";
          sha256 =
            "2787b56a1a8682853819fdd457773247040d3893ec8f255bd71093281cabf533";
        })
        (builtins.fetchurl {
          url = "${r}/${c}/${p}/berlin.guix.gnu.org.pub";
          sha256 =
            "0f7e51614eac75353196b650e73b7b5eeee59824b0f78935da0d6717ddd1792f";
        })
        (builtins.fetchurl {
          url = "${r}/${c}/${p}/substitutes.nonguix.org.pub";
          sha256 =
            "e784a3187cada58ac33c98cb0a7c3dc5ce253509fe0a8c6c717dedbe02b6c648";
        })
      ];
    };

    # https://substitutes.nonguix.org/signing-key.pub
    # https://codeberg.org/guix/guix/raw/branch/master/etc/substitutes/bordeaux.guix.gnu.org.pub
    # https://codeberg.org/guix/guix/raw/branch/master/etc/substitutes/berlin.guix.gnu.org.pub

    gc.enable = true;
    gc = {
      dates = "04:15";
      extraArgs = [ "--delete-generations=1m" "--optimize" ];
    };
  };
}
