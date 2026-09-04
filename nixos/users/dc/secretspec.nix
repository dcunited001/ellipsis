{ config, pkgs, ... }:
{
  users.users.dc.packages = [
    pkgs.sops
    pkgs.secretspec
    pkgs.age
    pkgs.age-plugin-openpgp-card
    pkgs.age-plugin-yubikey
    pkgs.age-plugin-tpm
    pkgs.agenix-cli
    pkgs.pass
    pkgs.sshpass
  ];
}
