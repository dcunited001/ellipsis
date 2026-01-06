{
  inputs,
  config,
  lib,
  pkgs,
  ...
}:
let
  hjemFiles = ./. + "../../../../gh/f";
in
{
  hjem.users.dc = {

    files = {
      "bin/alatheme.jq" = {
        executable = true;
        source = (hjemFiles + "/bin/alatheme.jq");
      };
      # ipc_socket = true # required for alacritty msg config $options
      # ... but it still doesn't seem to work
      "bin/alatheme" = {
        executable = true;
        source = (hjemFiles + "/bin/alatheme");
      };
    };
  };
}
