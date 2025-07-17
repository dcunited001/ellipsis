{ pkgs, ... }:
{

  services.pcscd.enable = true;
  # services.pscsd.plugins = [pkgs.ccid] # default

  # TODO: move some of these to the users.users.<username>.packages?
  environment.systemPackages = [
    pkgs.yubikey-manager
    pkgs.yubico-piv-tool
  ];
}
