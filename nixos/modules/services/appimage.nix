{ config, lib, pkgs, ... }:

{
  environment.systemPackages = [ pkgs.appimage-run ];
}
