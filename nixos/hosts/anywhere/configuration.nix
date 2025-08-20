{ lib, pkgs, config, ... }: {
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
    kernel.sysctl = { "kernel.unprivileged_bpf_disabled" = 1; };
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
      extraGroups = [ "wheel" "video" ];
      initialHashedPassword = "";
      openssh.authorizedKeys.keys = [
        "ecdsa-sha2-nistp384 AAAAE2VjZHNhLXNoYTItbmlzdHAzODQAAAAIbmlzdHAzODQAAABhBGE6wqFapBOKBA2wCTB22nG+GANmh9JXNG54tBajKNu/Fh61ywzilEI6MYLpvolCuS0YWGAgv4h5MHzk45KnWXKJ1NSNTLJ4koa+NvAAHIVXKA19IZ+s6UyX7eyCWLx58w== cardno:19294239"
        "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAACAQDS6PQ1NqAptAIzcfJLNRxy81yqaF4gc/RAXa6e7lQw2qD4UydYIDgGoD/EYIvVq6qH7g8VXncB7RHLUfnH96Ctod3wd8nb/d8HmFNS7J1PGhvFPjS2/MIT+eZQN1cqQWyohcbJpxT1d0ynsPYrtyEREmutdpJfBg8RDSViWh7gsfXyJlVir7IIIPokJPE0KY0vNyn/sZw6dnTGFyrigCsq9TzAMvnf8ToX1neYQ0ZBS1HNQucVQ7+Xq5ehDClJ5OeeP95uH4DXe22SlZBEGEo2W4ClaXrXVgiUneF46SnfQiEORJnqKWwcr3O1Zdof+oJWIROk9CiYET9yhV58aw2uC5RoxkxE8+2TlpDaVLsi9rHYDnq9Ky2J0vRFXJJCb5PMPKiHbN2elx77rhHckVMpYl4LYA1fLEV6p2oSVoG7Rtqc8MmTf9PZCANNozaO7Y+k/XuzxjJOO4gk33lT8vhopCGvzl7hiYXBkQSolaJmh4jKYJsKngCYBzDgGUx6LSyGUCQSyujBxdlJaoH/wgAhFB/CZYQv7ISA5cmz9jiEQ2/8KuNvkcZzVcUdcfwX2wbItZ5gwnxpQar0Ea2qbE6q7iYY+zxbw7EpLtGIUHjft0I5dFqYv53ADinFyvmN1mRZ34S6+LITall3JHOA0uJ7l1iHDjBnuvPDKej8PJhc0w== cardno:7699336"
        "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAACAQDS6PQ1NqAptAIzcfJLNRxy81yqaF4gc/RAXa6e7lQw2qD4UydYIDgGoD/EYIvVq6qH7g8VXncB7RHLUfnH96Ctod3wd8nb/d8HmFNS7J1PGhvFPjS2/MIT+eZQN1cqQWyohcbJpxT1d0ynsPYrtyEREmutdpJfBg8RDSViWh7gsfXyJlVir7IIIPokJPE0KY0vNyn/sZw6dnTGFyrigCsq9TzAMvnf8ToX1neYQ0ZBS1HNQucVQ7+Xq5ehDClJ5OeeP95uH4DXe22SlZBEGEo2W4ClaXrXVgiUneF46SnfQiEORJnqKWwcr3O1Zdof+oJWIROk9CiYET9yhV58aw2uC5RoxkxE8+2TlpDaVLsi9rHYDnq9Ky2J0vRFXJJCb5PMPKiHbN2elx77rhHckVMpYl4LYA1fLEV6p2oSVoG7Rtqc8MmTf9PZCANNozaO7Y+k/XuzxjJOO4gk33lT8vhopCGvzl7hiYXBkQSolaJmh4jKYJsKngCYBzDgGUx6LSyGUCQSyujBxdlJaoH/wgAhFB/CZYQv7ISA5cmz9jiEQ2/8KuNvkcZzVcUdcfwX2wbItZ5gwnxpQar0Ea2qbE6q7iYY+zxbw7EpLtGIUHjft0I5dFqYv53ADinFyvmN1mRZ34S6+LITall3JHOA0uJ7l1iHDjBnuvPDKej8PJhc0w== cardno:19294239"
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
    pcsctools
    tmux
    htop

    # extra CLI packages
    tree

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
  system.activationScripts.yubikeyGuide = let
    homeDir = "/home/nixos/";
    desktopDir = homeDir + "Desktop/";
    documentsDir = homeDir + "Documents/";
  in ''
    mkdir -p ${desktopDir} ${documentsDir}
    chown nixos ${homeDir} ${desktopDir} ${documentsDir}
  '';

  # cp -R ${self}/contrib/* ${homeDir}
  # ln -sf ${yubikeyGuide}/share/applications/yubikey-guide.desktop ${desktopDir}
  # ln -sf ${dicewareWebApp}/share/applications/${dicewareWebApp.name} ${desktopDir}
  # ln -sfT ${self} ${documentsDir}/YubiKey-Guide

  system.stateVersion = "25.05";
}
