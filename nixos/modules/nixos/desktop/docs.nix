{ pkgs, ... }:
{
  # if it's not a server, add manix (probably requires nixos docs)
  # environment.systemPackages = with pkgs; [ manix ];

  # man-cache ⏱ 24s (first run ... each run?)

  documentation.man = {
    enable = true;
    generateCaches = true; # default: false
    # mandoc.enable = true; # default: false
  };

  documentation.info.enable = true; # default: false   #
  documentation.doc.enable = false; # default: false   # HTML docs
  documentation.dev.enable = false; # default: false   # Docs 4 devs
  documentation.nixos.enable = true; # default: true   # Nixos HTML Docs

  # by default, info/doc/nixos are produced, generateCaches/dev are not

  # https://wiki.archlinux.org/title/Man_page#Reading_local_man_pages

}
