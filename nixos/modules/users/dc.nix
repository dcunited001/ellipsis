{ pkgs, ... }:
{
  # users.users "already defined" ... damit
  # users.users = let
  #   myUsers = ["dc"];
  # in
  #   builtins.listToAttrs (map (user:
  #     {
  #       name = user;
  #       value = {
  #         isNormalUser = true;
  #         home = "/home/${user}";
  #         extraGroups = [ "wheel" ];
  #       };
  #     }
  #   ) myUsers);

  users.groups = {
    dc = { name = "dc"; gid = 1000; };
  };
  nix.settings = {
    allowed-users = [ "dc" ];
    trusted-users = [ "dc" ];
  };

  # TODO: nix: move elsewhere once flatpak is setup in home-manager
  environment.sessionVariables.XDG_DATA_DIRS = [ "$HOME/.local/share/flatpak/exports/share" ];

  users.users.dc = {
    uid = 1000;
    group = "dc";
    extraGroups = [ "wheel" ]; # Enable ‘sudo’ for the user.
    isNormalUser = true;
    # useDefaultShell = true;
    subUidRanges = [{ startUid = 1000000; count = 65536; }];
    subGidRanges = [{ startGid = 1000000; count = 65536; }];
    # keyFiles
    openssh.authorizedKeys.keys = [
      "ecdsa-sha2-nistp384 AAAAE2VjZHNhLXNoYTItbmlzdHAzODQAAAAIbmlzdHAzODQAAABhBGE6wqFapBOKBA2wCTB22nG+GANmh9JXNG54tBajKNu/Fh61ywzilEI6MYLpvolCuS0YWGAgv4h5MHzk45KnWXKJ1NSNTLJ4koa+NvAAHIVXKA19IZ+s6UyX7eyCWLx58w== cardno:19294239"
      "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAACAQDS6PQ1NqAptAIzcfJLNRxy81yqaF4gc/RAXa6e7lQw2qD4UydYIDgGoD/EYIvVq6qH7g8VXncB7RHLUfnH96Ctod3wd8nb/d8HmFNS7J1PGhvFPjS2/MIT+eZQN1cqQWyohcbJpxT1d0ynsPYrtyEREmutdpJfBg8RDSViWh7gsfXyJlVir7IIIPokJPE0KY0vNyn/sZw6dnTGFyrigCsq9TzAMvnf8ToX1neYQ0ZBS1HNQucVQ7+Xq5ehDClJ5OeeP95uH4DXe22SlZBEGEo2W4ClaXrXVgiUneF46SnfQiEORJnqKWwcr3O1Zdof+oJWIROk9CiYET9yhV58aw2uC5RoxkxE8+2TlpDaVLsi9rHYDnq9Ky2J0vRFXJJCb5PMPKiHbN2elx77rhHckVMpYl4LYA1fLEV6p2oSVoG7Rtqc8MmTf9PZCANNozaO7Y+k/XuzxjJOO4gk33lT8vhopCGvzl7hiYXBkQSolaJmh4jKYJsKngCYBzDgGUx6LSyGUCQSyujBxdlJaoH/wgAhFB/CZYQv7ISA5cmz9jiEQ2/8KuNvkcZzVcUdcfwX2wbItZ5gwnxpQar0Ea2qbE6q7iYY+zxbw7EpLtGIUHjft0I5dFqYv53ADinFyvmN1mRZ34S6+LITall3JHOA0uJ7l1iHDjBnuvPDKej8PJhc0w== cardno:7699336"
      "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAACAQDS6PQ1NqAptAIzcfJLNRxy81yqaF4gc/RAXa6e7lQw2qD4UydYIDgGoD/EYIvVq6qH7g8VXncB7RHLUfnH96Ctod3wd8nb/d8HmFNS7J1PGhvFPjS2/MIT+eZQN1cqQWyohcbJpxT1d0ynsPYrtyEREmutdpJfBg8RDSViWh7gsfXyJlVir7IIIPokJPE0KY0vNyn/sZw6dnTGFyrigCsq9TzAMvnf8ToX1neYQ0ZBS1HNQucVQ7+Xq5ehDClJ5OeeP95uH4DXe22SlZBEGEo2W4ClaXrXVgiUneF46SnfQiEORJnqKWwcr3O1Zdof+oJWIROk9CiYET9yhV58aw2uC5RoxkxE8+2TlpDaVLsi9rHYDnq9Ky2J0vRFXJJCb5PMPKiHbN2elx77rhHckVMpYl4LYA1fLEV6p2oSVoG7Rtqc8MmTf9PZCANNozaO7Y+k/XuzxjJOO4gk33lT8vhopCGvzl7hiYXBkQSolaJmh4jKYJsKngCYBzDgGUx6LSyGUCQSyujBxdlJaoH/wgAhFB/CZYQv7ISA5cmz9jiEQ2/8KuNvkcZzVcUdcfwX2wbItZ5gwnxpQar0Ea2qbE6q7iYY+zxbw7EpLtGIUHjft0I5dFqYv53ADinFyvmN1mRZ34S6+LITall3JHOA0uJ7l1iHDjBnuvPDKej8PJhc0w== cardno:19294239"
    ];

    # home-manager: ~/.nix-profile
    # these install to: /etc/profiles/per-user/<username>
    packages = with pkgs; [
      # CLI
      tree
      git-repo
      git-lfs

      # NIX
      neofetch
      starship
      nix-search-cli

      # EDITOR
      emacs
      neovim

      # DATA
      jq
      yq
      sqlite
      sqlitebrowser

      # DEV
      gnumake
      nil # nix lsp
      tmux
      screen

      # TOOLS
      graphviz

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
      nwg-bar
      nwg-drawer
      swaynotificationcenter

      # DESKTOP DEBUG
      wev

      # FONTS
      font-manager

      # HYPR
      waybar
      hypridle
      eww
    ];
  };
}
