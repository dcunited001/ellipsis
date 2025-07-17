{pkgs, ...}: {
  # fontconfig.defaultFonts:
  # + emoji :: Noto Color Emoji
  # + monospace :: DejaVu Sans Mono
  # + sansSerif :: DejaVu Sans
  # + serif :: DejaVu Serif

  fonts.fontconfig.enable = true;
  # confPackages = [pkgs.fontconfig-conf];
  # hinting = {autohint = false; enable = true; style = "slight";};
  # subpixel = {lcdfilter = "default"; rgba = "none"};

  fonts.packages = [
    pkgs.dejavu_fonts
    pkgs.fira-code
    pkgs.fira-code-symbols
    pkgs.font-awesome
    # pkgs.hackgen-nf-font
    # pkgs.ibm-plex
    pkgs.inter
    pkgs.jetbrains-mono
    # pkgs.maple-mono.NF
    # pkgs.nerd-fonts.im-writing
    # pkgs.nerd-fonts.blex-mono
    pkgs.noto-fonts
    pkgs.noto-fonts-emoji
    pkgs.noto-fonts-cjk-sans
    pkgs.noto-fonts-cjk-serif
    pkgs.noto-fonts-monochrome-emoji
    pkgs.powerline-fonts
    pkgs.roboto
    pkgs.roboto-mono
    # pkgs.symbola
    pkgs.terminus_font

    pkgs.source-code-pro   # mono
    pkgs.source-sans-pro   # sans
    pkgs.source-serif-pro  # serif

    # pkgs.overpass
    pkgs.iosevka
    pkgs.nerd-fonts.iosevka
    # pkgs... nerd-fonts.iosevka-aile
    pkgs.cantarell-fonts
    # pkgs... font-ghostscript

    # asian fonts
    pkgs.source-han-sans
    pkgs.source-han-serif
    pkgs.wqy_zenhei

    pkgs.material-design-icons
    pkgs.material-icons
  ];
}
