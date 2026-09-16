{

  lib,
  pkgs,
  ...
}:
{
  users.users.dc.packages = [
    # kdenlive adds 400 megs and a ton of KDE/Plasma deps
    pkgs.kdePackages.kdenlive
    pkgs.ffmpeg-full
    # pkgs.davinci-resolve # seems like PITA
  ];
}
