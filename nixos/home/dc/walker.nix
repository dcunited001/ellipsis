{
  inputs,
  config,
  lib,
  pkgs,
  importTOML,
  ...
}:
{
  programs.walker = {
    enable = true;
  };

  # the default theme is just in
  # ~/.config/walker/themes/default/{style.css,*.xml}
  # environment.etc = {
  #   "xdg/walker/config.toml" = importTOML "./walker.toml";
  # };

}
