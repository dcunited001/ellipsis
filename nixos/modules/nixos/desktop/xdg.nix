{ pkgs, ... }:
{
  environment.sessionVariables = {
    # TODO: nixos: remove unnecessary reliance on $HOME/bin (use writeScriptBin)
    PATH = "$HOME/bin";
    XDG_CONFIG_HOME = "$HOME/.config";
    XDG_CACHE_HOME = "$HOME/.cache";
    XDG_DATA_HOME = "$HOME/.local/share";
    XDG_STATE_HOME = "$HOME/.local/state";
  };

  # /etc/xdg (if enabled, i think)
  #   -> .guix-home/profile  -> flatpak user/system  -> home-manager?
  #   -> .nix-profile        -> nix user XDG         -> nix user/system profiles
  #
  # echo -n $XDG_DATA_DIRS | tr ':' '\n' \
  # | xargs -I\{\} ls -1 \{\}/applications 2>/dev/null'

  xdg.mime.enable = true;
  xdg.mime = {
    # for users 2 override system defaults (prefer "added")
    # - stops processing on: first find @ highest mimeapps.list precedence
    defaultApplications = {
      # mail
      "x-scheme-handler/mailto" = [ "org.mozilla.Thunderbird.desktop" ];
      "application/calendar+json" = [ "org.mozilla.Thunderbird.desktop" ];
      "application/calendar+xml" = [ "org.mozilla.Thunderbird.desktop" ];
      "text/calendar" = [ "org.mozilla.Thunderbird.desktop" ];

      # web
      "x-scheme-handler/https" = [ "chromium-browser.desktop" ];
      "application/x-extension-htm" = [ "chromium-browser.desktop" ];
      "application/x-extension-html" = [ "chromium-browser.desktop" ];
      "application/x-extension-shtml" = [ "chromium-browser.desktop" ];
      "application/x-extension-xht" = [ "chromium-browser.desktop" ];
      "application/x-extension-xhtml" = [ "chromium-browser.desktop" ];

      # text
      "text/org" = [ "doomclient.desktop" ];
      "text/plain" = [ "doomclient.desktop" ];
    };

    # assume the application's .desktop handles adding mimetype claims
    addedAssociations = {
      # rss
      "application/rss+xml" = [
        "chromium-browser.desktop"
        "doomclient.desktop"
        "org.mozilla.Thunderbird.desktop"
      ];

      # browsers + emacs
      "x-scheme-handler/file" = [
        "chromium-browser.desktop"
        "doomclient.desktop"
      ];
      "image/svg+xml" = [
        "chromium-browser.desktop"
        "doomclient.desktop"
      ];
      "text/html" = [ "chromium-browser.desktop" ];

      # browsers only
      "application/xhtml+xml" = [ "chromium-browser.desktop" ];
      "x-scheme-handler/chrome" = [ "chromium-browser.desktop" ];

      # emacs only
      "application/log" = [ "doomclient.desktop" ];
    };

    # used 2: inhibit .desktop files that claim mimetype
    # - (esp. if distro default)
    # - processed after "added" at same XDG level
    # - can't add again once removed
    # - similar to CSS, specificity affects removal
    removedAssociations = {
      # nothing automagically opens http://probably.dumb.com
      # - a .desktop to call a notifier app would be better...
      # - but it needs to handle URI injection (not bash)
      # - d-bus subscription to listen for "" would be simpler
      "x-scheme-handler/http" = [ "" ];

      # this was in my mimetypes? it has a space in the name?
      "x-scheme-handler/podman-desktop" = [ "Podman Desktop.desktop" ];

      # chat
      "x-scheme-handler/io.element.desktop" = [
        "io.element.Element.desktop"
        "Element.desktop"
      ];
      "x-scheme-handler/element" = [ "io.element.Element.desktop" ];
      "x-scheme-handler/slack" = [
        "slack.desktop"
        "Slack.desktop"
      ];
      "x-scheme-handler/https" = [ "chromium-browser.desktop" ];
      "application/x-extension-htm" = [ "chromium-browser.desktop" ];
      "application/x-extension-html" = [ "chromium-browser.desktop" ];
      "application/x-extension-shtml" = [ "chromium-browser.desktop" ];
      "application/x-extension-xht" = [ "chromium-browser.desktop" ];
      "application/x-extension-xhtml" = [ "chromium-browser.desktop" ];

      # "x-scheme-handler/zoommtg" = ["us.zoom.Zoom.desktop"];

      # things i don't want emacs to open
      "application/x-shellscript" = [ "doomclient.desktop" ];
    };
  };
}
