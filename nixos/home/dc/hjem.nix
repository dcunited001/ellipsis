{
  inputs,
  lib,
  pkgs,
  ...
}:
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
          ".bashrc".text = ''
            # for shrc in $HOME/.local/share/gh/f/bash/{functions-hypr.sh}; do
            #  source $shrc
            # done
          '';
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
