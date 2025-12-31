{
  inputs,
  config,
  lib,
  pkgs,
  ...
}:
let
  translateCrx = "edanbjnaiofggfmimiidpfmhggkbokck";
  meetCrx = "kjgfgldnnfoeklkmfkjfagphfepbbdan";
in
{
  hjem.users.dc.xdg.data = {
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
          Exec=uwsm app -- chromium
          [Desktop Action new-private-window]
          Exec=uwsm app -- chromium --incognito
          Terminal=false
          Type=Application
          Icon=chromium
          # Icon=/home/dc/.nix-profile/share/icons/hicolor/256x256/apps/chromium.png
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
      # TODO don't use nwg-drawer to launch (it calls `/usr/bin/env $Exec` and fails)
      # - that requires an absolute path for $Exec, which can still be a symlink
      #
      # NOTE
      # - these don't have exec bits set, so `/usr/bin/env xdg-open` doesn't work
      # - if xdg-open doesn't start the app, then StartupWMClass isn't applied
      # - Chrome profile names won't be consistent across multiple machines.......
      # - ...................... ad infinitum
      "applications/Translate.desktop" =
        let
          profile = "Profile 3"; # ........ possible to rename profiles? idk
          crx = translateCrx;
          wmclass = "translate";
        in
        {
          text = ''
            #!/usr/bin/env xdg-open
            [Desktop Entry]
            Version=1.0
            Terminal=false
            Type=Application
            Name=Google Translate (Hjem)
            Exec=uwsm app -- chromium "--profile-directory=${profile}" --app-id=${crx}
            Icon=chrome-${crx}-${builtins.replaceStrings [ " " ] [ "_" ] profile}
            StartupWMClass=crx_${wmclass}_${crx}
            StartupNotify=true
          '';
          clobber = true;
        };
      "applications/Meet.desktop" =
        let
          profile = "Profile 2";
          crx = meetCrx;
          wmclass = "meet";
        in
        {
          text = ''
            #!/usr/bin/env xdg-open
            [Desktop Entry]
            Version=1.0
            Terminal=false
            Type=Application
            Name=Google Meet (Hjem)
            Exec=uwsm app -- chromium "--profile-directory=${profile}" --app-id=${crx}
            Icon=chrome-${crx}-${builtins.replaceStrings [ " " ] [ "_" ] profile}
            StartupWMClass=crx_${wmclass}_${crx}
            StartupNotify=true
          '';
          clobber = true;
        };
    };
  };
}
