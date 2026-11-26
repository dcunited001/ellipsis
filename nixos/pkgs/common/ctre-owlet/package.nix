{
  lib,
  stdenv,
  fetchurl,
  autoPatchelfHook,
  writeShellApplication,
  curl,
  jq,
  nix-update,
}:
stdenv.mkDerivation (finalAttrs: {
  pname = "ctre-owlet";
  version = "26.70.0";

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

  buildInputs = [
    (lib.getLib stdenv.cc.cc)
  ];

  installPhase = ''
    runHook preInstall

    mkdir -p "$out/bin"
    cp "$src" "$out/bin/owlet"
    chmod 0755 "$out/bin/owlet"

    runHook postInstall
  '';

  # update with $(nix build --no-link --print-out-paths .#ctre-owlet.updateScript)/bin/update-ctre-owlet
  #
  # for now, update with nix run github:mic92/nix-update/v1.17.0#nix-update -- --flake "ctre-owlet" --version=26.70.0
  #
  # https://github.com/Mic92/nix-update/issues/534
  passthru = {
    sources = {
      "x86_64-linux" = fetchurl {
        url = "https://redist.ctr-electronics.com/tools/${finalAttrs.meta.mainProgram}/${finalAttrs.version}/${finalAttrs.meta.mainProgram}-${finalAttrs.version}-linuxx86-64";
        hash = "sha256-CBsKEiKW/RLoRVU/FhRG/v4WiR8L3ETYZ57bR4Wpm6o=";
      };
      "aarch64-linux" = fetchurl {
        url = "https://redist.ctr-electronics.com/tools/${finalAttrs.meta.mainProgram}/${finalAttrs.version}/${finalAttrs.meta.mainProgram}-${finalAttrs.version}-linuxarm64";
        hash = "sha256-AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA=";
      };
    };
    updateScript = writeShellApplication {
      name = "update-${finalAttrs.pname}";
      runtimeInputs = [
        curl
        jq
        nix-update
      ];
      text = ''
        set -euo pipefail

        tool_name="owlet"
        NEW_VERSION="$(
          curl --silent -fsSL https://redist.ctr-electronics.com/index.json | jq -er --arg tool "$tool_name" '
            .Tools[]
            | select(.Name == $tool)
            | .Items | last | .Version
          '
        )"

        if [ "$NEW_VERSION" = "${finalAttrs.version}" ]; then
          echo "${finalAttrs.pname} is already up to date at ${finalAttrs.version}"
          exit 0
        fi

        for platform in ${lib.escapeShellArgs finalAttrs.meta.platforms}; do
          nix-update --flake --version="$NEW_VERSION" --system="$platform" "${finalAttrs.pname}"
          # update-source-version "ctre-$tool_name" "$NEW_VERSION" --ignore-same-version \
          #   --file=./pkgs/common/ctre-owlet/package.nix --source-key="sources.$platform"
        done

        echo "Updated ${finalAttrs.pname}: ${finalAttrs.version} -> $NEW_VERSION"
      '';
    };

  };
  meta = {
    description = "CLI Utility to convert CTR Electronics hoot .hoot files into other logging formats";
    homepage = "https://docs.ctr-electronics.com/cli-tools";
    license = lib.licenses.unfree;
    mainProgram = "owlet";
    platforms = builtins.attrNames finalAttrs.passthru.sources;
    # sourceProvenance = with lib.sourceTypes; [ binaryNativeCode ];
  };
})
