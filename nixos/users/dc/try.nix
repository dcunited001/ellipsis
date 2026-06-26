{
  inputs,
  pkgs,
  config,
  ...
}:
let
  tryPkg = inputs.try.packages.${pkgs.stdenv.system}.try;
in
{
  # set TRY_PROJECTS in ~/Work/.mise.toml or elsewhere
  environment.sessionVariables.TRY_PATH = [ "$HOME/Work/tries" ];

  users.users.dc = {
    packages = [
      tryPkg
      pkgs.ruby
    ];
  };
}
