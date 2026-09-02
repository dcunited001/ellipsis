{
  config,
  lib,
  pkgs,
  ...
}:
{
  sops.secrets.gns3 = { };
  services.gns3-server = {
    enable = true;

    auth = {
      enable = true;
      user = "gns3";
      passwordFile = "/run/secrets/gns3passwordFile";
    };

  };
}
