{ ... }: {
  services.earlyoom.enable = true;
  services.earlyoom = {
    enableDebugInfo = false;
    # enableNotifications = true;
    reportInterval = 0;
    freeMemThreshold = 5;
    freeSwapThreshold = 1;
  };
  # prefer-regexp "syncthing|firefox"
}
