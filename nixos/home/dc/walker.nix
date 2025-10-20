{
  inputs,
  config,
  lib,
  pkgs,
  ...
}:
{
  programs.walker = {
    enable = true;
    elephant = let 
      elephantPkgs = inputs.elephant.packages.${pkgs.stdenv.system};

      elephantProv = elephantPkgs.elephant-providers.overrideAttrs
	# (fi: pr: { excludedProviders = pr.excludedProviders ++ ["windows"]; });

        (fi: pr: { buildInputs = (pr.buildInputs or []) ++ [ pkgs.wayland ]; }); 
        # (fi: pr: { buildInputs = pr.buildInputs ++ [ pkgs.wayland pkgs.wayland-protocols pkgs.wayland-utils ]; }); 
	# (fi: pr: { excludedProviders = pr.excludedProviders ++ ["windows"]; });

        # (fi: pr: { nativeBuildInputs = pr.nativeBuildInputs ++ [ pkgs.wayland ]; }); 
        # fi.buildInputs = pr.buildInput ++ pkgs.hyprland.buildInputs; });

      elephantProvWithWayland = elephantPkgs.elephant-with-providers.overrideAttrs
        (fi: pr: { buildInputs = [ elephantPkgs.elephant elephantProv ]; }); 
      in {
        package = elephantProvWithWayland; 
      };
    };
  }


