{
  lib,
  stdenv,
  fetchurl,
  autoPatchelfHook,
  writeShellApplication,
  curl,
  jq,
  nix,
  gnused,
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

  # update with $(nix build --no-link --print-out-paths .#ctre-caniv-bin.updateScript)/bin/update-ctre-caniv-bin
  # passthru.updateScript = lib.getExe (writeShellApplication {
  passthru.updateScript = writeShellApplication {
    name = "update-${pname}";
    runtimeInputs = [
      curl
      jq
      nix
      gnused
    ];
    text = ''
      set -euo pipefail

      tool_name="caniv"
      nix_file="./ctre-caniv-bin.nix"

      release_json="$(
        curl -fsSL https://redist.ctr-electronics.com/index.json | jq -e --arg tool "$tool_name" '
          .Tools[]
          | select(.Name == $tool)
          | .Items | last
          | {
              version: .Version,
              x86_64_url: .Urls."linuxx86-64",
              x86_64_sha1: .Urls."linuxx86-64-sha1",
              aarch64_url: .Urls."linuxarm64",
              aarch64_sha1: .Urls."linuxarm64-sha1"
            }
        '
      )"

      latest_version="$(printf '%s' "$release_json" | jq -r '.version')"

      # evaluate before checking version so it fails fast
      x86_64_url="$(printf '%s' "$release_json" | jq -r '.x86_64_url')"
      aarch64_url="$(printf '%s' "$release_json" | jq -r '.aarch64_url')"
      x86_64_sha256="$(nix store prefetch-file --json "$x86_64_url" | jq -r '.hash')"
      aarch64_sha256="$(nix store prefetch-file --json "$aarch64_url" | jq -r '.hash')"

      # already sri
      # 
      # nix hash convert --hash-algo sha256 --to sri "$x86_64_sha256"
      # nix hash convert --hash-algo sha256 --to sri "$aarch64_sha256"

      if [ "$latest_version" = "${version}" ]; then
        echo "${pname} is already up to date at ${version}"
        exit 0
      fi

      # Converts the upstream SHA-1 into Nix SRI format; swap for `nix store prefetch-file`
      # if you prefer Nix to download the binaries and calculate sha256 hashes.
      # TODO: swap with nix store prefetch-file --json
      # x86_64_sri="$(nix hash convert --hash-algo sha256 --to sri "$x86_64_sha256")"
      # aarch64_sri="$(nix hash convert --hash-algo sha256 --to sri "$aarch64_sha256")"

      sed -i \
        -e "s/version = \"${version}\";/version = $latest_version;/" \
        -e "s|REPLACE_X86_64_HASH|$x86_64_sha256|" \
        -e "s|REPLACE_AARCH64_HASH|$aarch64_sha256|" \
        "$nix_file"

      echo "Updated ${pname}: ${version} -> $latest_version"
    '';
  };

  meta = {
    description = "CLI Utility to enumerate, retrieve info and configure CANivore devices";
    homepage = "https://docs.ctr-electronics.com/cli-tools";
    # license = lib.licenses.unfree;
    mainProgram = "caniv";
    platforms = [
      "x86_64-linux"
      "aarch64-linux"
    ];
  };

}
