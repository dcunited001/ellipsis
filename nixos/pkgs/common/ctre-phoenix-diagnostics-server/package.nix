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
  pname = "ctre-phoenix-diagnostics-server";
  version = "26.3.0";

  # NOTE: this is probably wrong.
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

  # case "${stdenv.hostPlatform.system}" in
  #    x86_64-linux) _arch="linuxx86-64" ;;
  #    aarch64-linux) _arch="linuxarm64" ;;
  # esac
  # _binfile="$_binname-${finalAttrs.version}-$_arch"

  installPhase = ''
    runHook preInstall

    mkdir -p "$out/bin"
    _binname=PhoenixDiagnosticsProgram
    cp "$src" "$out/bin/$_binname"
    chmod 0755 "$out/bin/$_binname"

    runHook postInstall
  '';

  # update with $(nix build --no-link --print-out-paths .#ctre-phoenix-diagnostics-server.updateScript)/bin/update-ctre-phoenix-diagnostics-server
  #
  # for now, update with nix run github:mic92/nix-update/v1.17.0#nix-update -- --flake "ctre-phoenix-diagnostics-server" --version=26.3.0
  #
  # https://github.com/Mic92/nix-update/issues/534
  passthru = {
    sources = {
      "x86_64-linux" = fetchurl {
        url = "https://redist.ctr-electronics.com/tools/${finalAttrs.meta.mainProgram}/${finalAttrs.version}/${finalAttrs.meta.mainProgram}-${finalAttrs.version}-linuxx86-64";
        hash = "sha256-xQb7CsxpQBWsb1L4OFrjaWXAEboZ+01dYHIYFiTqhGo=";
      };
      "aarch64-linux" = fetchurl {
        url = "https://redist.ctr-electronics.com/tools/${finalAttrs.meta.mainProgram}/${finalAttrs.version}/${finalAttrs.meta.mainProgram}-${finalAttrs.version}-linuxarm64";
        # hash = "sha256-AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA=";
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

        tool_name="PhoenixDiagnosticsProgram"
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
          #   --file=./pkgs/common/ctre-phoenix-diagnostics-server/package.nix --source-key="sources.$platform"
        done

        echo "Updated ${finalAttrs.pname}: ${finalAttrs.version} -> $NEW_VERSION"
      '';
    };

  };
  meta = {
    description = "CLI Utility that allows Phoenix Tuner to manage CTR Electronics devices";
    homepage = "https://docs.ctr-electronics.com/cli-tools";
    # license = lib.licenses.unfree;
    mainProgram = "PhoenixDiagnosticsProgram";
    platforms = builtins.attrNames finalAttrs.passthru.sources;
    # sourceProvenance = with lib.sourceTypes; [ binaryNativeCode ];
  };
})
