{ pkgs, ... }:
{
  services.displayManager.sddm.enable = true;
  services.displayManager.sddm = {
    package = pkgs.kdePackages.sddm;
    extraPackages = [ pkgs.sddm-astronaut ];
    wayland.enable = true;
    theme = "sddm-astronaut-theme";
  };

  environment.systemPackages = [ pkgs.sddm-astronaut ];

  # prevents getting stuck at shutdown
  systemd.settings.Manager = {
    DefaultTimeoutStopSec = "10s";
  };
}
