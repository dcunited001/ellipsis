{
  config,
  lib,
  inputs,
  pkgs,
  ...
}:
let
  ai-jail = inputs.ai-jail.packages.${pkgs.stdenv.system}.default;
  ai-jail-fix = ai-jail.overrideAttrs (_: {
    doCheck = false;
  });

  # # can skip module-specific checks with:
  # (oldAttrs: {
  # checkFlags = (oldAttrs.checkFlags or [ ]) ++ [ "--skip=module_name::test_name" ];
  # })

in
{
  users.users.dc.packages = [
    pkgs.opencode-desktop
    ai-jail-fix
  ];
}
