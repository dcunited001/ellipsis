{ pkgs, ... }:
{
  # NOTE .. do not use

  services.regreet = {
    enable = true;
    vt = 1;
    # settings = {
    #   # runs the regreet session
    #   default_session = {
    #     command = "env GTK_USE_PORTAL=0 GDK_DEBUG=no-portals ..."
    #   }
    # }

    # wayland sessions needed in /usr/share/wayland-sessions
    #
    # but services.displayManager is only compat with SDDM, GDM, etc


  };

  programs.regreet = {
    enable = true;

    # package = pkgs.greetd.regreet
    # settings = {toml-like};

    # requires environment setup
    # GTK_USE_PORTAL=0 GDK_DEBUG=no-portals
    # https://github.com/rharish101/ReGreet/blob/main/regreet.sample.toml

    # customize background/etc
    # extraCss = ""

    # cage locks the regreet program
    # cageArgs = [ "-s" ]; # -mlast: use the last connected monitor

    # defaults
    theme.name = "Adwaita";
    theme.package = pkgs.gnome-themes-extra;
    font.name = "Cantarell";
    font.size = 16;
    font.package = pkgs.cantarell-fonts;
    iconTheme.name = "Adwaita";
    iconTheme.package = pkgs.adwaita-icon-theme;
    cursorTheme.package = pkgs.adwaita-icon-theme;
  }
}
