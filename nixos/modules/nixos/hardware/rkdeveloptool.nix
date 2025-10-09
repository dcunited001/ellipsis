{ config, lib, pkgs, ... }:
let
  # rockchip's udev rules haven't been updated since 2021
  # 
  # - they don't include model 350b for the rk3588 (orangepi 5)
  # - they set everything to 666 and writable by users
  #
  # this is based on the scdaemon service

  rkdeveloptoolRev = "304f073752fd25c854e1bcf05d8e7f925b1f4e14";

  rockchipRules = pkgs.fetchurl {
    url =
      "https://raw.githubusercontent.com/rockchip-linux/rkdeveloptool/${rkdeveloptoolRev}/99-rk-rockusb.rules";
    sha256 = "0x1nkvfzidzjx0dis9xkw2yw09bv81zqkby12sbhlmgs607a68lf";
  };

  destination = "99-rk-rockusb.rules";

  # ran `udevadm verify --resolve-names=never --no-style 99-rk-rockusb.rules.test`
  rockchipUdevRulesPkg = pkgs.runCommand "rockchip-udev-rules" { } ''
    loc="$out/lib/udev/rules.d/"
    mkdir -p "''${loc}"
    sed 's/LABEL="end_rules"/# RK3588 \nATTRS{idVendor}=="2207", ATTRS{idProduct}=="350b", MODE="0660", GROUP="plugdev"\n\nLABEL="end_rules"/g' "${rockchipRules}" > "${rockchipRules}.1"
    sed 's/ MODE="0666", GROUP="users"/ MODE="0660", GROUP="plugdev"/g' "${rockchipRules}" > "${rockchipRules}.2"
    cp "${rockchipRules}.2" "''${loc}/${destination}"
  '';

  # too many derivations using sed ... but whatever. nicer facilities would
  # require the full stdenv.mkDerivation
  cfg = config.hardware.rkdeveloptool;
in {
  options.hardware.rkdeveloptool = {
    enable = lib.mkEnableOption "udev rules for rkdeveloptool and Rockchip";
    # don't install this by default
    # package = lib.mkDefault pkgs.rkdeveloptool;
  };

  config = lib.mkIf cfg.enable {
    services.udev.packages = [ rockchipUdevRulesPkg ];
    users.groups.plugdev = { };
  };
}
