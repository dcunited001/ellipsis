{
  pkgs,
  builtins,
  fetchurl,
  lib,
  ...
}:
let
  ndiVersion = "6"; # 6.3.2.0
  ndiInstallerName = "Install_NDI_SDK_v${ndiVersion}_Linux";
  # ndiFix = lib.overrideDerivation pkgs.ndi-6 (prev: {
  # name = "ndi-6-6.3.2.0";
  ndiFix = pkgs.ndi-6.overrideAttrs (prev: {
    src = (
      pkgs.fetchurl {
        url = "https://downloads.ndi.tv/SDK/NDI_SDK_Linux/${ndiInstallerName}.tar.gz";
        hash = "sha256:f0314f245446defc488b63ceb4689acf1a965aeefdadacb70571bb216a8cc183";
      }
    );
  });
  distroavFix = pkgs.obs-studio-plugins.distroav.override {
    ndi-6 = ndiFix;
  };
in
{
  programs.obs-studio.plugins = [ distroavFix ];

  # with pkgs.obs-studio-plugins; [
  #   # distroav
  # ];

  # https://docs.ndi.video/all/getting-started/white-paper/ndi-related-network-ports
  networking.firewall.allowedTCPPortRanges = [
    {
      from = 5960;
      to = 5965;
    }
  ];
  networking.firewall.allowedUDPPortRanges = [
    {
      from = 5960;
      to = 5965;
    }
  ];
  # { from = 6960; to = 6962; }
  # { from = 7960; to = 7962; }
}
