{
  lib,
  pkgs,
  config,
  ...
}:
{
  # via drduh/YubiKey-Guide, the first nixos system I built
  isoImage = {
    isoName = "nixos-anywhere.iso";
    # As of writing, zstd-based iso is 1542M, takes ~2mins to
    # compress. If you prefer a smaller image and are happy to
    # wait, delete the line below, it will default to a
    # slower-but-smaller xz (1375M in 8mins as of writing).
    squashfsCompression = "zstd";
    makeEfiBootable = true; # EFI booting
    makeUsbBootable = true; # USB booting
  };

  swapDevices = [ ];

  boot = {
    tmp.cleanOnBoot = true;
    kernel.sysctl = {
      "kernel.unprivileged_bpf_disabled" = 1;
    };
  };

  services = {
    pcscd.enable = true;
    udev.packages = [ pkgs.yubikey-personalization ];
    # Automatically log in at the virtual consoles.
    getty.autologinUser = "nixos";
    displayManager = {
      autoLogin = {
        enable = true;
        user = "nixos";
      };
    };
  };

  programs = {
    ssh.startAgent = false;
    gnupg = {
      dirmngr.enable = true;
      agent = {
        enable = true;
        enableSSHSupport = true;
      };
    };
  };

  users.users = {
    nixos = {
      isNormalUser = true;
      extraGroups = [
        "wheel"
        "video"
      ];
      initialHashedPassword = "";
      openssh.authorizedKeys.keys = [
        "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIAb9WIuASHRVkpqpF5tT5AZoOw5lqlY/ycx1UqsWQH7W openpgp:0xBECF991C"
      ];
    };
    root.initialHashedPassword = "";
  };
  nix.settings = {
    allowed-users = [ "nixos" ];
    trusted-users = [ "nixos" ];
  };

  security = {
    pam.services.lightdm.text = ''
      auth sufficient pam_succeed_if.so user ingroup wheel
    '';
    sudo = {
      enable = true;
      wheelNeedsPassword = false;
    };
  };

  environment.systemPackages = with pkgs; [
    # Tools for backing up keys
    paperkey
    pgpdump
    parted
    cryptsetup

    # Yubico's official tools
    yubikey-manager
    # yubikey-manager-qt
    yubikey-personalization
    # yubikey-personalization-gui
    yubico-piv-tool
    # yubioath-flutter

    # Testing
    ent

    # Password generation tools
    pwgen
    rng-tools

    # Might be useful beyond the scope of the guide
    cfssl
    pcsc-tools
    tmux
    htop

    btrfs-progs
    btrfs-list

    # extra CLI packages
    tree
    # nmcli
    # nmtui

    emacs
    # emacsPackages.nix-mode # probably requires emacsPackagesFor
    nixd
    nh
    nixfmt
    vim
    jq
    yq
    jc
    sqlite
    gnumake
    tmux
    screen
    ripgrep
    fd
    pass
    age
    age-plugin-yubikey
    age-plugin-tpm
    agenix-cli
    sops
    p7zip
    unzip
    socat
  ];

  # Disable networking so the system is air-gapped
  # Comment all of these lines out if you'll need internet access
  # boot.initrd.network.enable = false;
  # networking = {
  #   resolvconf.enable = false;
  #   dhcpcd.enable = false;
  #   dhcpcd.allowInterfaces = [ ];
  #   interfaces = { };
  #   firewall.enable = true;
  #   useDHCP = false;
  #   useNetworkd = false;
  #   wireless.enable = false;
  #   networkmanager.enable = lib.mkForce false;
  # };

  # this gives you nmcli and nmtui
  networking.networkmanager.enable = true;

  # Unset history so it's never stored Set GNUPGHOME to an
  # ephemeral location and configure GPG with the guide

  environment.interactiveShellInit = ''
    unset HISTFILE
    export GNUPGHOME="/run/user/$(id -u)/gnupg"
    if [ ! -d "$GNUPGHOME" ]; then
      echo "Creating \$GNUPGHOME…"
      install --verbose -m=0700 --directory="$GNUPGHOME"
    fi
    echo "\$GNUPGHOME is \"$GNUPGHOME\""
  '';

  # [ ! -f "$GNUPGHOME/gpg.conf" ] && cp --verbose "${self}/../config/gpg.conf" "$GNUPGHOME/gpg.conf"
  # [ ! -f "$GNUPGHOME/gpg-agent.conf" ] && cp --verbose ${gpgAgentConf} "$GNUPGHOME/gpg-agent.conf"

  # Copy the contents of contrib to the home directory, add a
  # shortcut to the guide on the desktop, and link to the whole
  # repo in the documents folder.
  system.activationScripts.yubikeyGuide =
    let
      homeDir = "/home/nixos/";
      desktopDir = homeDir + "Desktop/";
      documentsDir = homeDir + "Documents/";
    in
    ''
      mkdir -p ${desktopDir} ${documentsDir}
      chown nixos ${homeDir} ${desktopDir} ${documentsDir}
    '';

  # cp -R ${self}/contrib/* ${homeDir}
  # ln -sf ${yubikeyGuide}/share/applications/yubikey-guide.desktop ${desktopDir}
  # ln -sf ${dicewareWebApp}/share/applications/${dicewareWebApp.name} ${desktopDir}
  # ln -sfT ${self} ${documentsDir}/YubiKey-Guide

  system.stateVersion = "25.05";
}
