# https://github.com/feel-co/hjem/blob/3c01274451544d3f5ebceec382447c46cb9ca83c/flake.nix#L45
let
  userHome = "/home/alice";
in
(import ./lib) {
  name = "checkonetwo";
  nodes = {
    node1 =
      {
        self,
        lib,
        pkgs,
        ...
      }:
      {
        users.groups.alice = { };
        users.users.alice = {
          isNormalUser = true;
          home = userHome;
          password = "";
        };
      };
  };
  testScript = ''
    machine.succeed("loginctl enable-linger alice")
  '';
}
