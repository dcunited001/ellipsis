{
  inputs,
  lib,
  pkgs,
  ...
}:
let
  inherit (lib.fileset)
    unions
    fromSource
    toSource
    toList
    ;
  inherit (lib) fileContents;
in
{

  hjem = {
    linker = inputs.hjem.packages.${pkgs.stdenv.hostPlatform.system}.smfh;
    users = {
      dc = {
        enable = true;

        # fdsa = lib.mapAttrsToList; # lib.fileContents;

        # let root = ./../gh; in {fdsa = lib.fileset.unions [(root + /f/bash)];}
        # let root = ./../gh;
        # in lib.fileset.toSource {
        #   inherit root;
        #   fileset = lib.fileset.unions [(root + /f/.bashrc)];
        #   }

        files = {
          "bin/omarchy-find-webapp".text = ''
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
          ".bashrc".text = ''
            # for shrc in $HOME/.local/share/gh/f/bash/{functions-hypr.sh}; do
            #  source $shrc
            # done
          '';
          # ".bar".source = "/.bash_logout";
        }; # // dfList;

        # xdg.config
        # xdg.cache
        # xdg.data
        # xdg.state

        xdg.data = {
          files = {
            "applications/chromium-browser.desktop" = {
              text = ''
                [Desktop Entry]
                StartupWMClass=chromium-browser
                Version=1.0
                Name=Chromium
                GenericName=Web Browser
                Exec=uwsm app -- chromium %U
                Categories=Network;WebBrowser;
                MimeType=application/pdf;application/rdf+xml;application/rss+xml;application/xhtml+xml;application/xhtml_xml;application/xml;image/gif;image/jpeg;image/png;image/webp;text/html;text/xml;x-scheme-handler/http;x-scheme-handler/https;x-scheme-handler/webcal;x-scheme-handler/mailto;x-scheme-handler/about;x-scheme-handler/unknown
                Actions=new-window;new-private-window;
                [Desktop Action new-window]
                Exec=chromium
                [Desktop Action new-private-window]
                Exec=chromium --incognito
                Terminal=false
                Type=Application
                Icon=/home/dc/.nix-profile/share/icons/hicolor/256x256/apps/chromium.png
                StartupNotify=true
              '';
              clobber = true;
            };
            "applications/Onshape.desktop" = {
              text = ''
                [Desktop Entry]
                Version=1.0
                Name=Onshape
                Comment=Onshape
                # Exec=omarchy-launch-webapp https://cad.onshape.com
                Exec=uwsm app -- chromium --app="https://cad.onshape.com"
                Terminal=false
                Type=Application
                Icon=/home/dc/.local/share/applications/icons/Onshape.png
                StartupNotify=true
              '';
              clobber = true;
            };

          };
        };
      };
    };
  };
}

# getContents = (dest: src: lib.fileContents src);
# root = "./..";

# let?
#   bashFiles =
#     lib.mapAttrs getContents { ".bashrc" = ../../../../gh/f/.bashrc; };
# in { ".foo".text = "bar"; } // bashFiles;

# nix eval .#nixosConfigurations.kratos.config.hjem.users.dc.files.'".foo"' --json | jq
