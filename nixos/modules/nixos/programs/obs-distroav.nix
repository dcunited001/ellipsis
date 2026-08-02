{ pkgs, ... }:
{
  programs.obs-studio.plugins = with pkgs.obs-studio-plugins; [
    distroav
  ];

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
