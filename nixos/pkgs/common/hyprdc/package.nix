{ lib, stdenv, fetchgit, }:
let
  pname = "hyprdc";
  # install_root = ".";
  # scripts unused for how
  scripts = [ "hjbinds" "hjinspect.jq" ];
  # install_path = "share/fdsa/${pname}";
  # url = "https://github.com/dcunited001/ellipsis";
in stdenv.mkDerivation {
  inherit pname; # pname wasn't sufficient here.
  name = pname;
  src = ./src;
  # strictDeps = true;
  dontBuild = true;

  # NOTE if any scripts need patching, another derivation is needed as input
  dontPatchShebangs = true;

  # dontUnpack = true;
  installPhase = ''
    install -m755 -D $src/bin/hjbinds $out/bin/hjbinds
    install -m755 -D $src/bin/hjinspect.jq $out/bin/hjinspect.jq
  '';
  meta = {
    license = lib.licenses.mit;
    description =
      "If it supports or depends on hyprland, it goes here for now.";
  };
}
