{ lib, pkgs, ... }:
let
  hyprdc = (pkgs.callPackage
    (lib.custom.relativeToRoot "pkgs/common/hyprdc/package.nix") { });
in {

  users.groups = {
    dc = {
      name = "dc";
      gid = 1000;
    };
  };
  nix.settings = {
    allowed-users = [ "dc" ];
    trusted-users = [ "dc" ];
  };

  # TODO: nix: move elsewhere once flatpak is setup in home-manager
  environment.sessionVariables.XDG_DATA_DIRS =
    [ "$HOME/.local/share/flatpak/exports/share" ];

  users.users.dc = {
    uid = 1000;
    group = "dc";
    extraGroups = [ "wheel" "docker" "podman" "audio" ];
    isNormalUser = true;
    # useDefaultShell = true;
    linger = true; # continue running [oci-container] services after logout
    subUidRanges = [{
      startUid = 1000000;
      count = 65536;
    }];
    subGidRanges = [{
      startGid = 1000000;
      count = 65536;
    }];
    # keyFiles
    openssh.authorizedKeys.keys = [
      "ecdsa-sha2-nistp384 AAAAE2VjZHNhLXNoYTItbmlzdHAzODQAAAAIbmlzdHAzODQAAABhBGE6wqFapBOKBA2wCTB22nG+GANmh9JXNG54tBajKNu/Fh61ywzilEI6MYLpvolCuS0YWGAgv4h5MHzk45KnWXKJ1NSNTLJ4koa+NvAAHIVXKA19IZ+s6UyX7eyCWLx58w== cardno:19294239"
      "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAACAQDS6PQ1NqAptAIzcfJLNRxy81yqaF4gc/RAXa6e7lQw2qD4UydYIDgGoD/EYIvVq6qH7g8VXncB7RHLUfnH96Ctod3wd8nb/d8HmFNS7J1PGhvFPjS2/MIT+eZQN1cqQWyohcbJpxT1d0ynsPYrtyEREmutdpJfBg8RDSViWh7gsfXyJlVir7IIIPokJPE0KY0vNyn/sZw6dnTGFyrigCsq9TzAMvnf8ToX1neYQ0ZBS1HNQucVQ7+Xq5ehDClJ5OeeP95uH4DXe22SlZBEGEo2W4ClaXrXVgiUneF46SnfQiEORJnqKWwcr3O1Zdof+oJWIROk9CiYET9yhV58aw2uC5RoxkxE8+2TlpDaVLsi9rHYDnq9Ky2J0vRFXJJCb5PMPKiHbN2elx77rhHckVMpYl4LYA1fLEV6p2oSVoG7Rtqc8MmTf9PZCANNozaO7Y+k/XuzxjJOO4gk33lT8vhopCGvzl7hiYXBkQSolaJmh4jKYJsKngCYBzDgGUx6LSyGUCQSyujBxdlJaoH/wgAhFB/CZYQv7ISA5cmz9jiEQ2/8KuNvkcZzVcUdcfwX2wbItZ5gwnxpQar0Ea2qbE6q7iYY+zxbw7EpLtGIUHjft0I5dFqYv53ADinFyvmN1mRZ34S6+LITall3JHOA0uJ7l1iHDjBnuvPDKej8PJhc0w== cardno:7699336"
      "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAACAQDS6PQ1NqAptAIzcfJLNRxy81yqaF4gc/RAXa6e7lQw2qD4UydYIDgGoD/EYIvVq6qH7g8VXncB7RHLUfnH96Ctod3wd8nb/d8HmFNS7J1PGhvFPjS2/MIT+eZQN1cqQWyohcbJpxT1d0ynsPYrtyEREmutdpJfBg8RDSViWh7gsfXyJlVir7IIIPokJPE0KY0vNyn/sZw6dnTGFyrigCsq9TzAMvnf8ToX1neYQ0ZBS1HNQucVQ7+Xq5ehDClJ5OeeP95uH4DXe22SlZBEGEo2W4ClaXrXVgiUneF46SnfQiEORJnqKWwcr3O1Zdof+oJWIROk9CiYET9yhV58aw2uC5RoxkxE8+2TlpDaVLsi9rHYDnq9Ky2J0vRFXJJCb5PMPKiHbN2elx77rhHckVMpYl4LYA1fLEV6p2oSVoG7Rtqc8MmTf9PZCANNozaO7Y+k/XuzxjJOO4gk33lT8vhopCGvzl7hiYXBkQSolaJmh4jKYJsKngCYBzDgGUx6LSyGUCQSyujBxdlJaoH/wgAhFB/CZYQv7ISA5cmz9jiEQ2/8KuNvkcZzVcUdcfwX2wbItZ5gwnxpQar0Ea2qbE6q7iYY+zxbw7EpLtGIUHjft0I5dFqYv53ADinFyvmN1mRZ34S6+LITall3JHOA0uJ7l1iHDjBnuvPDKej8PJhc0w== cardno:19294239"
    ];

    # home-manager: ~/.nix-profile
    # these install to: /etc/profiles/per-user/$USER
    packages = with pkgs; [
      # CUSTOM
      hyprdc

      # CLI
      tree
      git-repo
      git-stack

      # CLI SHINY
      neofetch
      # starship

      # NIX
      fh
      nvd
      manix
      nix-search-cli
      nix-diff
      nixfmt
      nix-visualize

      # nil # nix lsp
      nixd
      nh

      # EDITOR
      emacs
      neovim

      # DATA
      jq
      yq
      jc
      sqlite
      sqlitebrowser

      # DATA: VIZ
      d2
      graphviz
      plantuml
      imagemagick

      # DEV
      gnumake
      tmux
      screen

      # DEV: DOOM EMACS
      ripgrep
      fd

      # DEV: CRYPT
      pass
      age
      age-plugin-yubikey
      # age-plugin-tpm
      agenix-cli
      sops

      # LANG: JAVASCRIPT
      node2nix

      # LANG: JAVA
      openjdk

      # TOOLS
      p7zip
      unzip

      # TOOLS: CONTAINERS
      dive
      podman-compose

      # TOOLS: NETWORK
      socat

      # TERM
      alacritty

      # BROWSER
      firefox
      chromium

      # IRC
      quassel # flatpak doesn't load settings

      # DESKTOP
      # thunar # programs.thunar...
      wofi
      wl-clipboard-rs
      dex

      # DESKTOP: SCREENSHOTS
      grim
      slurp
      grimblast

      # DESKTOP: AUDIO
      playerctl
      pavucontrol
      qpwgraph
      helvum

      # DESKTOP: NOTIFICATIONS
      swaynotificationcenter

      # DESKTOP: NWG
      nwg-look
      nwg-bar
      nwg-drawer
      nwg-displays
      nwg-icon-picker

      # DESKTOP: MEDIA
      vlc

      # DESKTOP: INPUT
      piper
      # xkbcomp
      # xkbprint # trying to avoid unnecessary X11 deps (? idk)
      xkbvalidate
      fcitx5-configtool

      # problematic when fcitx5 configured by system:
      # fcitx5 fcitx5-mozc fcitx5-gtk

      # DESKTOP: DEBUG
      wev

      # GTK
      dconf-editor
      gsettings-qt

      # QT

      # FONTS
      font-manager

      # HYPR
      hyprpaper
      hyprls
      hyprpolkitagent
      hyprland-qtutils # for banners/ANR

      # EWW
      eww

      # WAYBAR
      waybar
      sysz # fzf for systemd
      yad # gtk-like dmenu
    ];

  };
}
