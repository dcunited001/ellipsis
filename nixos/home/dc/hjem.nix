{
  inputs,
  lib,
  pkgs,
  ...
}:
let
  guixHome = ./. + "../../../../gh/f";
in
{
  hjem = {
    users = {
      dc = {
        enable = true;

        files = {
          "bin/omarchy-find-webapp" = {
            executable = true;
            text = ''
              #!/usr/bin/env bash
              omarchyFindWebapp() {
                appname="$1"
                browser=$(xdg-settings get default-web-browser)
                for desktopFile in {~/.local,~/.nix-profile,/usr}/share/applications/$appname.desktop; do
                  [[ -f $desktopFile ]] \
                  && grep -qe '^Exec=.*omarchy-launch-webapp' $desktopFile \
                  && echo "$desktopFile"
                done
              }

              omarchyFindWebapp $1
            '';
          };

          # sources /etc/bashrc, then ~/.bashrc
          ".bashrc".text = ''
              for shrc in $HOME/.local/share/gh/f/bash/{failias.sh}; do
              [[ -f $shrc ]] && source $shrc
            done
          '';
        };

        xdg.data = {
          # failias.sh: the aliases that almost were
          "gh/f/bash/failias.sh" = lib.readFile (guixHome + "/bash/rc/failias.sh");
        };

        # xdg.config
        # xdg.cache
        # xdg.data
        # xdg.state

        # xdg.data = ;
      };
    };
  };
}
