{
  inputs,
  config,
  lib,
  pkgs,
  importTOML,
  ...
}:
let
  elephantPkgs = inputs.elephant.packages.${pkgs.stdenv.system};
  elephantProv = elephantPkgs.elephant-providers.overrideAttrs (
    fi: pr: { buildInputs = (pr.buildInputs or [ ]) ++ [ pkgs.wayland ]; }
  );
  elephantProvWithWayland = elephantPkgs.elephant-with-providers.overrideAttrs (
    fi: pr: {
      buildInputs = [
        elephantPkgs.elephant
        elephantProv
      ];
    }
  );
  # elephantCfg =
in
{
  programs.walker = {
    enable = true;
    elephant = {
      enable = lib.mkForce false; # doesn't override (bc of implementation?)
      # package = elephantProvWithWayland;
    };
  };

  # the default theme is just in
  # ~/.config/walker/themes/default/{style.css,*.xml}
  environment.etc = {
    "xdg/walker/config.toml" = importTOML "./walker.toml";
  };

}

# kept overriding elephant-with-providers ... didn't realize i needed 2 chain
#
# elephantProv = elephantPkgs.elephant-providers.overrideAttrs (
#   fi: pr: { buildInputs = (pr.buildInputs or [ ]) ++ [ pkgs.wayland ]; }
# );
# (fi: pr: { excludedProviders = pr.excludedProviders ++ ["windows"]; });
# (fi: pr: { buildInputs = pr.buildInputs ++ [ pkgs.wayland pkgs.wayland-protocols pkgs.wayland-utils ]; });
# (fi: pr: { excludedProviders = pr.excludedProviders ++ ["windows"]; });
# (fi: pr: { nativeBuildInputs = pr.nativeBuildInputs ++ [ pkgs.wayland ]; });
# fi.buildInputs = pr.buildInput ++ pkgs.hyprland.buildInputs; });



