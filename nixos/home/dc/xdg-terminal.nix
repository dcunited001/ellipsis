{
  inputs,
  config,
  lib,
  pkgs,
  ...
}:
let
  foo = "bar";
in
{
  # Terminal apps need a translation layer for omarchy-launch-tui to function.
  # This could be done in derivations... but nope I don't feel like it

  # pushd `nwix alacritty`
  # cat "$(pwd)/share/applications/Alacritty.desktop" | wl-copy # and paste
  # ssh myhost ~/.local/share/applications/Alacritty.desktop" | wl-copy # and paste/scrape

  # pushd `nwix ghostty`
  # cat "$(pwd)/share/applications/com.mitchellh.ghostty.desktop" | wl-copy # and paste
  # ssh myhost ~/.local/share/applications/com.mitchellh.ghostty.desktop" | wl-copy # and paste/scrape
  hjem.users.dc.xdg.data = {
    files = {
      "applications/Alacritty.desktop" = {
        text = ''
          [Desktop Entry]
          Type=Application
          TryExec=alacritty
          Exec=alacritty
          Icon=Alacritty
          Terminal=false
          Categories=System;TerminalEmulator;

          Name=Alacritty
          GenericName=Terminal
          Comment=A fast, cross-platform, OpenGL terminal emulator
          StartupNotify=true
          StartupWMClass=Alacritty
          Actions=New;

          [Desktop Action New]
          Name=New Terminal
          Exec=alacritty
          X-TerminalArgExec=-e
          X-TerminalArgAppId=--class=
          X-TerminalArgTitle=--title=
          X-TerminalArgDir=--working-directory=
        '';
        clobber = true;
      };
    };

    # already correct, but doesn't work with MOD3
    # "applications/com.mitchellh.ghostty.desktop" = { };

    # "applications/Foot.desktop" = { };

    # "applications/Kitty.desktop" = { };
  };
}
