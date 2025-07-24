{ config, lib, pkgs, ... }: {
  imports = [ # Include the results of the hardware scan.
    ./hardware.nix
    ./xdg.nix
    #      ./containers.nix
    ../../modules/users/dc.nix
    ../../modules/services/bluetooth.nix
    ../../modules/services/fonts.nix
    ../../modules/services/guix.nix
    ../../modules/services/nix.nix
    ../../modules/services/openssh.nix
    ../../modules/services/pipewire.nix
    ../../modules/services/printing.nix
    ../../modules/services/ras.nix
    ../../modules/services/sddm.nix
    ../../modules/services/smartd.nix
    ../../modules/services/xdg.nix
    ../../modules/services/yubikey.nix
    ../../modules/services/zerotierone.nix
  ];

  networking.hostName = "kratos";

  # https://gist.github.com/Le0xFF/21942ab1a865f19f074f13072377126b
  boot.supportedFilesystems = [ "btrfs" ];

  # Use the systemd-boot EFI boot loader.
  boot.loader = {
    systemd-boot.enable = true;
    efi.canTouchEfiVariables = true;
  };

  # if luks devices is nonempty, merges availableKernelModules with:
  #   availableKernelModules = [dm_mod,dm_crypt,cryptd,input_leds]
  #     ++ luks.cryptoModules ++ others
  #
  # boot.kernelModules # for boot stage-2
  #   - will implicitly have stage-1, with maybe some technical discrepencies
  boot.initrd.kernelModules = [ "cryptd" "btrfs" "zstd" ];
  boot.initrd.luks.devices = {
    luksroot = {
      device = "/dev/disk/by-uuid/d02f163b-d7c6-4e6f-bb55-601e9c39200e"; # UUID
      # device = "/dev/disk/by-uuid/8691357d-bb80-2a47-8923-914370fc3e26"; # PART_UUID
      allowDiscards = true;
      preLVM = true;
    };
  };

  # Use latest kernel.
  boot.kernelPackages = pkgs.linuxPackages_latest;

  # ---------------------------------------------
  # Hardware Support
  hardware.enableRedistributableFirmware = true;
  # hardware.enableAllFirmware = true;

  # ---------------------------------------------
  # GPU
  # TODO: hardware.amdgpu.opencl.enable = true;

  # ---------------------------------------------
  # Disks

  # ---------------------------------------------
  # Filesystems

  fileSystems = {
    # ... typo: ztsd => zstd
    "/".options = [ "defaults" "noatime" "compress=zstd" ];
    "/nix".options = [ "defaults" "noatime" ];
    "/gnu".options = [ "defaults" "noatime" ];
    "/var".options = [ "defaults" "noatime" "compress=zstd" ];
    "/var/log".options = [ "defaults" "noatime" "compress=zstd" ];
    "/var/cache".options = [ "defaults" "noatime" "compress=zstd" ];
    "/var/tmp".options = [ "defaults" "noatime" "compress=zstd" ];
    "/home".options =
      [ "defaults" "noatime" "compress=zstd" "discard=async" "ssd" ];
  };
  services.gvfs.enable = true;

  services.locate.enable = true;
  services.locate = {
    # prunePaths = [ "/tmp" "/var/tmp" "/var/cache" "/var/lock" "/var/run" "/var/spool"
    #   "/nix/store" "/nix/var/log/nix" ];
    # others: /sys /run
    prunePaths = [
      "/afs"
      "/gnu/store"
      "/net"
      "/media"
      "/mnt"
      "/sfs"
      "/tmp"
      "/udev"
      "/root"
      "/proc"
    ];
    pruneBindMounts = true;
    # pruneNames = [ ".bzr" ".cache" ".git" ".hg" ".svn" ];
    # pruneFS = [ "looks good" ];
    # extraFlags = []; # arch doesn't pass anything else
  };

  time.timeZone = "America/New_York";

  i18n.defaultLocale = "en_US.UTF-8";
  console = {
    font = "Lat2-Terminus16";
    # keyMap = "us";
    # useXkbConfig = true; # use xkb.options in tty.
  };

  # services.xserver.enable = true;
  # services.xserver.xkb.layout = "us";
  # services.xserver.xkb.variant = "altgr-intl";
  # services.xserver.xkb.model = "pc104";
  # services.xserver.xkb.options = "caps:ctrl_modifier,lv3:ralt_alt,lv3:menu_switch";

  # Enable touchpad support (enabled default in most desktopManager).
  services.libinput.enable = true;

  # users.mutableUsers = false;
  users.defaultUserShell = pkgs.zsh;

  # ---------------------------------------------
  # DESKTOP stuff

  programs.git = {
    enable = true;
    prompt.enable = true;
  };
  programs.firefox.enable = true;
  programs.thunar.enable = true;
  services.flatpak.enable = true;

  programs.zsh = {
    enable = true;
    enableLsColors = true;
  };
  programs.mtr.enable = true;
  programs.gnupg.agent = {
    enable = true;
    enableSSHSupport = true;
  };

  programs.tmux.enable = true;
  programs.tmux = {
    withUtempter = false; # /var/run/utmp (to display connected users)
    secureSocket = true; # socket in /run instead of /tmp (a little to forward)
    # tmuxPlugins.copycat, tmuxPlugins.fpp, tmuxPlugins.cpu, tmuxPlugins.cpu
    plugins = [ pkgs.tmuxPlugins.sessionist pkgs.tmuxPlugins.sysstat ];
    # defaults
    shortcut = "b"; # Screen "a"
  };

  # ---------------------------------------------

  services.avahi.enable = true;

  services.gpm.enable = true;
  services.tlp.enable = true;
  services.thermald.enable = true;
  # services.earlyoom.enable = false; # needs a configuration

  # https://wiki.nixos.org/wiki/ZeroTier_One
  #
  # autojoins networks. does not leave
  # services.zerotierone.enable = true;

  networking.networkmanager.enable = true;

  # networking.firewall.enable = false;
  # networking.firewall = {
  #   allowedTCPPorts = [ ... ];
  #   allowedUDPPorts = [ ... ];
  # }

  # TODO: expose TCPDump as config option
  # TODO: accidentally deleted a networking config here.
  # tcpdump wrapper granting syscap (user: )
  programs.tcpdump.enable = true;

  # ---------------------------------------------
  # SYSTEM PACKAGES

  # TODO: nix: lots of duplicate packages end up being installed
  environment.variables.EDITOR = "vim";
  environment.systemPackages = with pkgs; [
    git
    vim
    emacs
    wget
    curl

    uwsm
    # ddcutil # TODO: services.i2c.enable, groups, kernel module

    # SYS ADMIN
    btop
    htop
    killall

    # TOOLS
    file
    lsof
    rng-tools

    # TOOLS: VM
    qemu

    # TOOLS: NETWORK
    dnsutils
    inetutils

    # TOOLS: DISK
    du-dust
    ioping
    smartmontools

    # TOOLS: FIRMWARE
    fiano
    dmidecode

    # TOOLS: HARDWARE
    sysstat
    usbutils
    hwinfo
    inxi
    glxinfo
    pciutils
    lm_sensors
    lshw
    v4l-utils
    hw-probe
    brightnessctl

  ];

  # implicit: hyprland zsh

  # ---------------------------------------------
  # WINDOW MANAGER

  # https://wiki.nixos.org/wiki/Sway
  # https://wiki.nixos.org/wiki/Hyprland
  programs = {
    uwsm = {
      waylandCompositors = {
        hyprland-debug = {
          prettyName = "hyprland-debug";
          binPath = "/run/current-system/sw/bin/Hyprland";
          comment = "Run Hyprland with env-hyprland-debug";
        };
      };
    };
    hyprland = {
      enable = true;
      withUWSM = true; # recommended for most users
      xwayland.enable = true; # Xwayland can be disabled.
    };
    dconf.enable = true;
    seahorse.enable = true;
    neovim.enable = true; # defaultEditor = true;
  };

  security.pam.services.hyprlock = { };
  # programs.swaylock.enable = true;
  security.pam.services.swaylock = { };

  system.copySystemConfiguration = true;
  system.stateVersion = "25.05";
}
