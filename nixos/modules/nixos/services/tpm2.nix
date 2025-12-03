{
  config,
  lib,
  pkgs,
  ...
}:
{
  # environment.systemPackages = [
  #   pkgs.tpm2-tss
  #   pkgs.tpm2-tools
  # ];
  services.security.tpm2 = {
    enable = true;
    pkcs11.enable = true;
    applyUdevRules = true; # makes tpm(rm)?[0-9] availabe to tss user/group
  };
}
