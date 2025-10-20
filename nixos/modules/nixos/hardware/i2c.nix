{
  config,
  lib,
  pkgs,
  ...
}:

{
  boot.kernelModules = [ "i2c_dev" ];
  users.groups.i2c = { };

  # the default i2c module also grants serial access to anyone logged in via a
  # seat (physically present).
  #
  #  ACTION=="add",
  #  KERNEL=="i2c-[0-9]*",
  #  TAG+="uaccess",
  #  GROUP="${cfg.group}",
  #  MODE="660"
  #
  # https://github.com/bigbigmdm/IMSProg/issues/98
}
