{
  config,
  lib,
  pkgs,
  ...
}:
{
  programs.steam = {
    enable = true;
  
    protontricks.enable = true;
    remotePlay.openFirewall = true;
    extraCompatPackages = with pkgs; [
      proton-ge-bin
    ];
  };
}

# remoteplay ports:
# https://help.steampowered.com/en/faqs/view/3E3D-BE6B-787D-A5D2
# https://help.steampowered.com/en/faqs/view/2EA8-4D75-DA21-31EB
# 
# allowedTCPPorts = [ 27036 27037 ];
# allowedUDPPorts = [ 10400 10401 ];
# allowedUDPPortRanges = [{ from = 27031; to = 27035; }];
