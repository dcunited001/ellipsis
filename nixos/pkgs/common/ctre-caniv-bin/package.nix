{
  lib,
  stdenv,
  fetchurl,
  autoPatchelfHook,
}:

let
  platform = if stdenv.hostPlatform.isAarch64 then "linuxarm64" else "linuxx86-64";
  pname = "ctre-caniv-bin";
  version = "1.1.1.0";

  # NOTE: not yet tested. need an arm system (don't want to change my flake yet)
  sources = {
    "x86_64-linux" = {
      url = "https://redist.ctr-electronics.com/tools/caniv/${version}/caniv-${version}-${platform}";
      hash = "sha256-mjan4/WUeF97HOZDbqM+rCoGt0owaTkl+vpeT4lYIo4=";
    };
    "aarch64-linux" = {
      url = "https://redist.ctr-electronics.com/tools/caniv/${version}/caniv-${version}-${platform}";
      hash = lib.fakeHash;
    };
  };

  srcInfo =
    sources.${stdenv.hostPlatform.system}
      or (throw "Unsupported system: ${stdenv.hostPlatform.system}");
in
stdenv.mkDerivation {
  inherit pname version;

  src = fetchurl {
    inherit (srcInfo) url hash;
  };

  dontUnpack = true;

  nativeBuildInputs = [
    autoPatchelfHook
  ];

  buildInputs = [
    stdenv.cc.cc.lib
  ];

  installPhase = ''
    runHook preInstall

    mkdir -p "$out/bin"
    cp "$src" "$out/bin/caniv"
    chmod 0755 "$out/bin/caniv"

    runHook postInstall
  '';

  meta = {
    description = "CLI Utility to enumerate, retrieve info and configure CANivore devices";
    homepage = "https://docs.ctr-electronics.com/cli-tools";
    license = lib.licenses.unfree;
    mainProgram = "caniv";
    platforms = [
      "x86_64-linux"
      "aarch64-linux"
    ];
  };
}
