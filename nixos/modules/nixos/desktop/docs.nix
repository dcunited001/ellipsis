{ pkgs, ... }: {
  # if it's not a server, add manix (probably requires nixos docs)
  # environment.systemPackages = with pkgs; [ manix ];

  # defaults
  documentation.man.enable = true;
  documentation.man.generateCaches = false;

  # by default, info/doc/dev/nixos are produced, mandoc is not
}
