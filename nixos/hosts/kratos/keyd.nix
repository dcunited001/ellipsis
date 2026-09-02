{
  config,
  lib,
  pkgs,
  ...
}:
{
  services.keyd.enable = true;
  services.keyd.keyboards = {
    default = {
      ids = [ "*" ];
      settings = {
        main = {
          insert = "f24";
        };
      };
    };
  };
}
