{ ... }: {
  services.earlyoom.enable = true;
  services.earlyoom = {
    enableDebugInfo = true;
    # enableNotifications = true;
    reportInterval = 0;
    freeMemThreshold = 5;
    freeSwapThreshold = 1;
  };
  # prefer-regexp "syncthing|firefox"
}
