{ config, lib, pkgs, ... }:

{
  programs.walker = {
    enable = true;
    # style = lines
    # https://github.com/abenz1267/walker/tree/master/resources/themes/default/style.css

    # themes = attrsOf str
    # https://github.com/abenz1267/walker/tree/master/resources/themes/default
  };
}
