{ lib, stdenv, fetchgit, ... }:
let
  pname = "dcstaticdots";
  scripts = [ "bash/colors.sh" ]; # "bash/alias.sh"

in stdenv.mkDerivation {
  inherit pname; # pname wasn't sufficient here.
  name = pname;

  # TODO: this works with callPackage, but lib.custom isn't recognized after that
  src = (lib.custom.relativeToRoot "../gh/f");

  # strictDeps = true;
  dontBuild = true;

  # NOTE if any scripts need patching, another derivation is needed as input
  dontPatchShebangs = true;

  # dontUnpack = true;
  installPhase = ''
    for f in ${lib.concatStringsSep "\n" scripts}; do
      install -m644 -D "$src/$f" "$out/share/gh/f/$f"
    done
  '';
  meta = {
    license = lib.licenses.mit;
    description = "Static dots for hjem that don't need hjem-impure.";
  };
}
