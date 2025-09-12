{ config, inputs, outputs, lib, pkgs, nixpkgs-unstable, ... }:

{
  disabledModules = [
    "services/desktop-managers/gnome.nix"
    "services/web-apps/openvscode-server.nix"
    # "programs/vscode.nix"
    # "services/display-managers/gdm.nix"
  ];
  imports = lib.flatten [
    "${nixpkgs-unstable}/nixos/modules/services/desktop-managers/gnome.nix"
    "${nixpkgs-unstable}/nixos/modules/services/desktops/gnome/gcr-ssh-agent.nix"
    "${nixpkgs-unstable}/nixos/modules/services/display-managers/gdm.nix"
    "${nixpkgs-unstable}/nixos/modules/programs/vscode.nix"
    "${nixpkgs-unstable}/nixos/modules/services/web-apps/openvscode-server.nix"
    # "./hardware.nix"
    # "./disko.nix"
    ./containers.nix

    # inputs.disko.nixosModules.disko
    # "./disko.nix"
    # {
    #   _module.args = {
    #     disk = "/dev/vda";
    #     withSwap = false;
    #   };
    # }

    (map lib.custom.relativeToRoot [
      "modules/nixos/desktop/appimage.nix"
      "modules/nixos/desktop/bluetooth.nix"
      "modules/nixos/desktop/fonts.nix"
      "modules/nixos/desktop/docs.nix"
      "modules/nixos/desktop/xdg.nix"

      "modules/nixos/programs/gnupg.nix"
      "modules/nixos/desktop/pipewire.nix"

      "modules/nixos/services/nix.nix"
      "modules/nixos/services/openssh.nix"
      "modules/nixos/services/ras.nix"
      "modules/nixos/services/smartd.nix"
      "modules/nixos/services/yubikey.nix"
      # "modules/nixos/services/zerotierone.nix"

      "modules/users/frc.nix"
    ])
  ];

  # nixpkgs.unstable.config.allowUnfree = true;

  networking.hostName = "helius";
  boot.supportedFilesystems = [ "btrfs" ];

  # ---------------------------------------------
  # Boot

  config.hardware.cpu.intel.updateMicrocode = true;
  config.hardware.enableRedistributableFirmware = true;

  boot.loader = {
    systemd-boot.enable = true;
    systemd-boot.configurationLimit = 10;
    systemd-boot.consoleMode = "auto";
    efi.canTouchEfiVariables = true;
  };

  boot.initrd = {
    systemd.enable = true;
    # Don't need to enter password in emergency mode
    systemd.emergencyAccess = true;
  };

  # ---------------------------------------------
  # Nix
  system.stateVersion = "25.05";

  # nix-store --gc --print-roots
  nix.gc = {
    automatic = true;
    options = "--delete-older-than 30d";
    persistent = true;
    randomizedDelaySec = "60min";
  };

  # generate documentation for modules in configuration.nix
  # documentation.nixos.includeAllModules = true;

  nix.settings = {
    auto-optimise-store = true;
    experimental-features = [ "nix-command" "flakes" ];
    allowed-users = [ "root" "@wheel" "@builders" ];
    trusted-users = [ "@wheel" ]; # root was implicit here
    trusted-substituters = [ "https://nix-community.cachix.org" ];
    trusted-public-keys = [
      "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
    ];
  };
  nixpkgs = {
    nixpkgs.hostPlatform = "x86_64-linux";
    config = { allowUnfree = true; };
    # required for unfree packages in nixpkgs-unstable
    overlays = [ outputs.overlays.default ];
  };

  environment.systemPackages = [ pkgs.cachix pkgs.nix-tree pkgs.nix-du ];

  # ---------------------------------------------
  # Gnome

  services.desktopManager.gnome.debug = true; # maybe turn off
  services.desktopManager.gnome.enable = true;
  services.displayManager.gdm.enable = true;

  # defaults
  services.gnome = {
    core-shell.enable = true;
    core-apps.enable = true;
    # dconf-editor, devhelp, gnome-builder, sysprof
    core-developer-tools.enable = true;

    glib-networking.enable = true;

    # search indexing
    localsearch.enable = false;
    tinysparql.enable = false;

    sushi.enable = true; # file previewer for nautilus

    # initial setup
    gnome-initial-setup.enable = true;

    # keyring
    gnome-keyring.enable = true;

    # accessibility
    at-spi2-core.enable = false;
  };

  # used by Gnome 3 for thunderbolt settings
  services.hardware.bolt.enable = true;
  qt.platformTheme = "gnome";

  # ---------------------------------------------
  # Networking

  # oof: apparently wifi 6e just realllly really sucks:
  #
  # - https://discourse.nixos.org/t/intel-ax210-ap-mode/66087/9
  # - https://github.com/randomizedcoder/nixos
  # - https://github.com/randomizedcoder/nixos/blob/main/desktop/l2/hostapd-multi.nix#L2
  hardware.wirelessRegulatoryDatabase = true;

  # ---------------------------------------------
  # Environment
  environment.variables = {
    GSK_RENDERER = "ngl"; # https://nixos.wiki/wiki/GNOME#Blank_windows
    NIXOS_OZONE_WL = "1";
    EDITOR = "vim";
  };

  environment.sessionVariables = {
    # TODO: nixos: remove unnecessary reliance on $HOME/bin (use writeScriptBin)
    PATH = "$HOME/bin";
    XDG_CONFIG_HOME = "$HOME/.config";
    XDG_CACHE_HOME = "$HOME/.cache";
    XDG_DATA_HOME = "$HOME/.local/share";
    XDG_STATE_HOME = "$HOME/.local/state";
  };

  # ---------------------------------------------
  # VS Code
  programs.vscode = {
    enable = true;
    defaultEditor = false;
    package = pkgs.unstable.vscode.fhs;
    extensions = with pkgs.unstable.vscode-extensions; [
      bbenoist.nix
      golang.go
      twxs.cmake
      ms-vscode.cpptools
      redhat.java
      redhat.vscode-yaml
      ms-python.python
      ms-python.debugpy
      ms-python.vscode-pylance
      ms-python.isort
      ms-python.black-formatter
      vscjava.vscode-java-pack
    ];
  };

  # ---------------------------------------------
  # Programs
  programs.foot.enable = true;
  programs.yazi.enable = true;
  programs.neovim.enable = true;

  programs.television = {
    enable = true;
    enableZshIntegration = true;
    enableBashIntegration = false;
  };

  programs.starship = {
    enable = true;
    interactiveOnly = true; # default
  };

  programs.zsh = {
    enableCompletion = true;
    autosuggestions.enable = false;
    vteIntegration = false;
    ohMyZsh = {
      enable = true;
      theme = "avit";
    };
  };

}
