{
  config,
  lib,
  pkgs,
  ...
}:
let
  dc = "dc";
  # https://github.com/SquirrelModeller/squirrel-nixos/blob/0e52526186fa984e51ea3328174c0bca0ca4b366/flake.nix#L28
  #
  # - in flake.nix: makeNixosConfiguration, userEntries, userNames, getUserPrograms
  # - in ./modules/users.nix: mkMerge merges basic user attrs in
  # - users have misc.nix, services.nix, programs/default.nix (the main entry point)
  #
  # builtins.readFile (./. + "/hosts/${host}/system")
  # builtins.readFile (./. + "/.envrc") # hmmm
  #
in
{
  users.users.${dc}.packages = [
    # DESKTOP: NOTIFICATIONS
    pkgs.swaynotificationcenter
    # pkgs.mako
    # pkgs.libnotify
  ];
}
