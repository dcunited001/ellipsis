{ pkgs, ... }: {
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
  environment.sessionVariables.XDG_DATA_DIRS =
    [ "$HOME/.local/share/flatpak/exports/share" ];

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
