{
  config,
  lib,
  pkgs,
  ...
}:
{
  programs.gpu-screen-recorder.enable = true;
  users.users.dc.packages = [ pkgs.gpu-screen-recorder-gtk ];

  # programs.gpu-screen-recorder = {
  #   ui.enable = true;
  # };
}
