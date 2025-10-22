{ pkgs, ... }:
{
  users.groups = {
    dctest = {
      name = "dctest";
      gid = 1999;
    };
  };
  # necessary?
  nix.settings = {
    allowed-users = [ "dctest" ];
    trusted-users = [ "dctest" ];
  };

  # - after creation, /etc/passwd and /etc/shadow exist
  #   but login is denied (set to prevent blank passwd?)
  # - this (early) config doesn't result in $XDG_RUNTIME_DIR given `su - dctest``
  users.users.dctest = {
    uid = 1999;
    group = "dctest";
    extraGroups = [ "wheel" ]; # Enable ‘sudo’ for the user.
    isNormalUser = true;
    packages = with pkgs; [
      tree
      neovim
      jq
      yq
      ripgrep
      fd
      pass
      age
      age-plugin-yubikey
      agenix-cli
      sops
      socat
      alacritty
      wofi
      wl-clipboard-rs
      dex
      waybar
      eww
    ];
  };
}
