{
  inputs,
  config,
  lib,
  pkgs,
  outputs,
  ...
}:
{
  imports = lib.flatten [
    (lib.custom.relativeToRoot "hosts/common.nix")
    ./amdgpu.nix # load this here to ensure nixpkgs.config.rocmSupport early
    ./hardware.nix
    ./networking.nix
    ./openssh.nix
    ./xdg.nix
    ./fcitx5.nix
    ./input.nix
    ./containers.nix
    ./bash.nix
    ./mise.nix
    ./python.nix
    ./hypr.nix
    ./blender.nix
    ./steam.nix

    (map lib.custom.relativeToRoot [
      "users/dc"

      "modules/nixos/programs/gnupg.nix"
      "modules/nixos/programs/obs-studio.nix"
      "modules/nixos/programs/vscode.nix"

      "modules/nixos/desktop/appimage.nix"
      "modules/nixos/desktop/bluetooth.nix"
      "modules/nixos/desktop/docs.nix"
      "modules/nixos/desktop/fonts.nix"
      "modules/nixos/desktop/gtk.nix"
      "modules/nixos/desktop/ios.nix"
      "modules/nixos/desktop/pipewire.nix"
      "modules/nixos/desktop/printing.nix"
      "modules/nixos/desktop/sddm.nix"
      "modules/nixos/desktop/xdg.nix"

      "modules/nixos/hardware/i2c.nix"
      # "modules/nixos/hardware/rkdeveloptool.nix"

      "modules/nixos/services/containers.nix"
      "modules/nixos/services/earlyoom.nix"
      "modules/nixos/services/guix.nix"
      "modules/nixos/services/locate.nix"
      "modules/nixos/services/nix.nix"
      "modules/nixos/services/openssh.nix"
      "modules/nixos/services/ras.nix"
      "modules/nixos/services/smartd.nix"
      "modules/nixos/services/tpm2.nix"
      "modules/nixos/services/yubikey.nix"
      "modules/nixos/services/zerotierone.nix"

      "home/dc/hjem.nix"
      "home/dc/walker.nix"
      "home/dc/alacritty.nix"
      "home/dc/chromium.nix"
      "home/dc/doom.nix"
      # "home/dc/ectorepo.nix"
      "home/dc/hypr.nix"
      "home/dc/hypridle.nix"
      "home/dc/swaync.nix"
      "home/dc/thunderbird.nix"
      "home/dc/yktouch.nix"
      # "home/dc/waybar.nix"
    ])

    ./ollama.nix
    ./frc.nix
  ];

  # ---------------------------------------------
  # nixpkgs
  nixpkgs = {
    overlays = [ outputs.overlays.default ];
    config = {
      allowUnfree = true;
      # permittedInsecurePackages = [ "libsoup-2.74.3" ];
    };
  };

  networking.hostName = "kratos";

  # ---------------------------------------------
  # nix-ld (this also gets loaded in a lot of modules, defaults below)
  programs.nix-ld.enable = true;

  # | Compression | zlib          | zstd    |  xz    | bzip2 |
  # | Build       | stdenv.cc.cc  |         |        |       |
  # | Network     | curl          |         |        |       |
  # | Crypto      | libsodium     | openssl | libssh |       |
  # | Data        | libxml2       |         |        |       |
  # | Linux FS    | acl           | attr    |        |       |
  # | Linux       | util-linux    | systemd |        |       |

  # =============================================
  # bootloader

  # https://gist.github.com/Le0xFF/21942ab1a865f19f074f13072377126b
  boot.supportedFilesystems = [ "btrfs" ];

  boot.loader = {
    systemd-boot.enable = true;
    systemd-boot.configurationLimit = 15;
    systemd-boot.consoleMode = "auto";
    efi.canTouchEfiVariables = true;
  };

  # if luks devices is nonempty, merges availableKernelModules with:
  #   availableKernelModules = [dm_mod,dm_crypt,cryptd,input_leds]
  #     ++ luks.cryptoModules ++ others
  #
  # boot.kernelModules # for boot stage-2
  #   - will implicitly have stage-1, with maybe some technical discrepencies
  boot.initrd.kernelModules = [
    "cryptd"
    "btrfs"
    "zstd"
  ];
  boot.initrd.luks.devices = {
    luksroot = {
      device = "/dev/disk/by-uuid/d02f163b-d7c6-4e6f-bb55-601e9c39200e"; # UUID
      # device = "/dev/disk/by-uuid/8691357d-bb80-2a47-8923-914370fc3e26"; # PART_UUID
      allowDiscards = true;
      preLVM = true;
    };
  };

  # Use latest kernel.
  boot.kernelPackages = pkgs.linuxPackages_zen;
  boot.kernelModules = [ "v4l2loopback" ];
  boot.extraModulePackages = [ config.boot.kernelPackages.v4l2loopback ];

  # ---------------------------------------------
  # Appimage

  # this configures boot.binfmt.registrations.appimage for both appimage v1/v2
  programs.appimage.enable = true;
  programs.appimage.binfmt = true;

  # ---------------------------------------------
  # Hardware Support

  hardware.enableRedistributableFirmware = true;
  # hardware.enableAllFirmware = true;

  # add rockchip udev rules
  # hardware.rkdeveloptool.enable = true;

  services.gpm.enable = true; # mouse at console (sometimes)
  services.tlp.enable = true; # power manamagement profiles
  services.thermald.enable = true; # thermal monitoring

  # control moniters (requires i2c group)
  services.ddccontrol.enable = true;

  # =============================================
  # Filesystems

  fileSystems = {
    # ... typo: ztsd => zstd
    "/".options = [
      "defaults"
      "noatime"
      "compress=zstd"
    ];
    "/nix".options = [
      "defaults"
      "noatime"
    ];
    "/gnu".options = [
      "defaults"
      "noatime"
    ];
    "/var".options = [
      "defaults"
      "noatime"
      "compress=zstd"
    ];
    "/var/log".options = [
      "defaults"
      "noatime"
      "compress=zstd"
    ];
    "/var/cache".options = [
      "defaults"
      "noatime"
      "compress=zstd"
    ];
    "/var/tmp".options = [
      "defaults"
      "noatime"
      "compress=zstd"
    ];
    "/home".options = [
      "defaults"
      "noatime"
      "compress=zstd"
      "discard=async"
      "ssd"
    ];
  };
  services.gvfs.enable = true;

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
  users.defaultUserShell = pkgs.bash;

  # ---------------------------------------------
  # Browsers

  # programs.firefox.enable = true;
  programs.chromium.enable = true;

  # ---------------------------------------------
  # Desktop

  hjem.linker = inputs.hjem.packages.${pkgs.stdenv.hostPlatform.system}.smfh;

  programs.thunar.enable = true;

  # TODO nix: remove once flatpak upgrade service is setup
  services.flatpak.enable = true;

  programs.git = {
    enable = true;
    prompt.enable = true;
    lfs.enable = true;
  };

  programs.bash.interactiveShellInit = "";

  programs.zsh = {
    enable = true;
    enableLsColors = true;
  };
  programs.mtr.enable = true;

  programs.tmux.enable = true;
  programs.tmux = {
    withUtempter = false; # /var/run/utmp (to display connected users)
    secureSocket = true; # socket in /run instead of /tmp (a little to forward)
    # tmuxPlugins.copycat, tmuxPlugins.fpp, tmuxPlugins.cpu, tmuxPlugins.cpu
    plugins = [
      pkgs.tmuxPlugins.sessionist
      pkgs.tmuxPlugins.sysstat
    ];
    # defaults
    shortcut = "b"; # Screen "a"
  };

  # disable speechd
  services.speechd.enable = false;

  # =============================================
  # Home

  # ---------------------------------------------
  # Service Tweaks

  # =============================================
  # Networking

  services.avahi.enable = true;
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

  # needed for store VS Code auth token
  # services.gnome.gnome-keyring.enable = true;

  # =============================================
  # System Packages

  # TODO: nix: lots of duplicate packages end up being installed
  environment.variables.EDITOR = "vim"; # TODO: ends up being doomclient
  environment.systemPackages = with pkgs; [
    git
    vim
    emacs
    wget
    curl

    uwsm
    # ddcutil # TODO: services.i2c.enable, groups, kernel module

    # SYS ADMIN
    htop
    killall
    pstree

    # TOOLS
    file
    lsof
    rng-tools
    diffoscope
    kernel-hardening-checker

    # TOOLS: VM
    qemu

    # TOOLS: NETWORK
    dnsutils
    inetutils
    tunctl
    bridge-utils

    # TOOLS: CRYPTO
    certgraph
    tpm2-tss
    tpm2-tools
    age
    age-plugin-yubikey
    age-plugin-tpm

    # ssh-tpm-agent

    # TOOLS: DISK
    dust
    ioping
    smartmontools
    btrfs-progs

    # TOOLS: FIRMWARE
    fiano
    dmidecode

    # TOOLS: HARDWARE
    sysstat
    usbutils
    hwinfo
    inxi
    mesa-demos
    pciutils
    lm_sensors
    lshw
    v4l-utils
    hw-probe
    brightnessctl
  ];

  # =============================================
  # Misc

  system.stateVersion = "25.05";

  # incompatible with pure flakes. doesn't seem to work anyways
  # system.copySystemConfiguration = true;
}
