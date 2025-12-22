{
  inputs,
  config,
  lib,
  pkgs,
  ...
}:
let
  hjemFiles = ./. + "../../../../gh/f";
in
{
  hjem.users.dc.enable = true;

  hjem.users.dc = {

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
      ".screen/journal-gpg".source = (hjemFiles + "/.screen/journal-gpg.screenrc");

      # sources /etc/bashrc, then ~/.bashrc
      ".bashrc".text = ''
        for shrc in $HOME/.local/share/gh/f/bash/rc/{failias.sh,util.sh,mise.sh}; do
          [[ -f $shrc ]] && source $shrc
        done
      '';
    };

    xdg.data.files = {
      # failias.sh: the aliases that almost were
      "gh/f/bash/rc/mise.sh".source = (hjemFiles + "/bash/rc/mise.sh");
      "gh/f/bash/rc/failias.sh".source = (hjemFiles + "/bash/rc/failias.sh");
      "applications/io.elementary.iconbrowser.desktop".text = ''
        [Desktop Entry]
        Version=1.0
        Type=Application
        Name=Icon Browser
        Comment=Browse and find system icons
        Keywords=icons;iconography;
        Icon=io.elementary.iconbrowser
        Exec=io.elementary.iconbrowser
        Terminal=false
      '';
    };

    xdg.config.files = {
      # - use nix-ld to avoid all_compile=true ... probably going to want to um lock the flake
      # - `mise completion bash --include-bash-completion-lib > ~/.local/share/bash-completions/mise`
      "mise/config.toml.ohhh".text = ''
        [settings]
        all_compile = false
        # jobs = 12

        [tools]
        node = "latest"
        usage = "latest"
      '';
    };

    # xdg.cache
    # xdg.data
    # xdg.state

    # xdg.data = ;
  };

  # hjem.users.dc.xdg.cache.files = {
  #   "test" = {
  #     text = config.hjem.users.dc.files.".screen/journal-gpg".target;
  #   };
  # };
}
