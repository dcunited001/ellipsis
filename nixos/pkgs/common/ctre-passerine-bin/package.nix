{
  lib,
  stdenv,
  fetchurl,
  autoPatchelfHook,
  writeShellApplication,
  curl,
  jq,
  # nix,
  gnused,
  # makeWrapper,
  common-updater-scripts,
}:

stdenv.mkDerivation (finalAttrs: {
  pname = "ctre-passerine-bin";
  version = "0.0.1";

  src =
    finalAttrs.passthru.sources.${stdenv.hostPlatform.system}
      or (throw "Unsupported system: ${stdenv.hostPlatform.system}");

  strictDeps = true;
  __structuredAttrs = true;

  dontUnpack = true;
  dontConfigure = true;
  dontBuild = true;

  # [ makeWrapper ] # may be needed here (and wrapProgram, see codegraph/package.nix)
  nativeBuildInputs = lib.optionals stdenv.hostPlatform.isLinux [
    autoPatchelfHook
  ];

  installPhase = ''
    runHook preInstall

    mkdir -p "$out/bin"
    cp "$src" "$out/bin/passerine"
    chmod 0755 "$out/bin/passerine"

    runHook postInstall
  '';

  # update with $(nix build --no-link --print-out-paths .#ctre-passerine-bin.updateScript)/bin/update-ctre-passerine-bin
  # passthru.updateScript = lib.getExe (writeShellApplication {
  passthru = {
    sources = {
      "x86_64-linux" = fetchurl {
        url = "https://redist.ctr-electronics.com/tools/passerine/${finalAttrs.version}/passerine-${finalAttrs.version}-linuxx86-64";
        hash = "sha256-TrK3YxY9De+RkdaELI9z1NnLfXsky9bFqrqeL/67L0Q=";
      };
      "aarch64-linux" = fetchurl {
        url = "https://redist.ctr-electronics.com/tools/passerine/${finalAttrs.version}/passerine-${finalAttrs.version}-linuxarm64";
        hash = "sha256-gqoeR2rmWwVpvMDiHO8k+3Nd7rntdMEtFg8iRRsZFEA=";
      };
    };
    updateScript = writeShellApplication {
      name = "update-${finalAttrs.pname}";
      runtimeInputs = [
        curl
        jq
        # nix
        gnused
        common-updater-scripts
      ];
      text = ''
        set -euo pipefail

        # curl --silent -fsSL -o /dev/null -w '%{url_effective}' $url # just follows redirects and extracts from github's ./releases/latest URL
        tool_name="passerine"
        release_json="$(
          curl --silent -fsSL https://redist.ctr-electronics.com/index.json | jq -e --arg tool "$tool_name" '
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

        echo "$release_json"
        NEW_VERSION="$(printf '%s' "$release_json" | jq -r '.version')"


        if [ "$NEW_VERSION" = "${finalAttrs.version}" ]; then
          echo "${finalAttrs.pname} is already up to date at ${finalAttrs.version}"
          exit 0
        fi

        for platform in ${lib.escapeShellArgs finalAttrs.meta.platforms}; do
          update-source-version "ctre-$tool_name-bin" "$NEW_VERSION" --file=./pkgs/common/ctre-passerine-bin/package.nix --ignore-same-version --source-key="sources.$platform"
        done

        echo "Updated ${finalAttrs.pname}: ${finalAttrs.version} -> $NEW_VERSION"
      '';
    };

  };
  meta = {
    description = "CLI utility to turn MIDI (.mid and .midi) files into CTR Electronics CHRP .chrp files";
    homepage = "https://docs.ctr-electronics.com/cli-tools";
    # license = lib.licenses.unfree;
    mainProgram = "passerine";
    platforms = builtins.attrNames finalAttrs.passthru.sources;
    # sourceProvenance = with lib.sourceTypes; [ binaryNativeCode ];
  };
})
