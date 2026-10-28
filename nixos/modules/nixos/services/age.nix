{ pkgs, ... }:
{
  environment.systemPackages = [
    pkgs.age
    pkgs.age-plugin-openpgp-card
    pkgs.age-plugin-yubikey
    pkgs.age-plugin-tpm
  ];
}
