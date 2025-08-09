{ pkgs, ... }: {

  services.pcscd.enable = true;
  # services.pscsd.plugins = [pkgs.ccid] # default

  # TODO: move some of these to the users.users.<username>.packages?
  environment.systemPackages = [ pkgs.yubikey-manager pkgs.yubico-piv-tool ];

  # https://github.com/maximbaz/yubikey-touch-detector
  # ./notifier/unix_socket.go
  programs.yubikey-touch-detector = {
    enable = true;
    unixSocket = true; # in v1.13.0, only writes 3 booleans
    verbose = false;
  };
}
