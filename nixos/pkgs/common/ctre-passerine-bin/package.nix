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
  pname = "ctre-passerine-bin";
  version = "1.0.0";

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

  installPhase = ''
    runHook preInstall

    mkdir -p "$out/bin"
    cp "$src" "$out/bin/passerine"
    chmod 0755 "$out/bin/passerine"

    runHook postInstall
  '';

  # update with $(nix build --no-link --print-out-paths .#ctre-passerine-bin.updateScript)/bin/update-ctre-passerine-bin
  #
  # for now, update with nix run github:mic92/nix-update/v1.17.0#nix-update -- --flake "ctre-passerine-bin" --version=1.0.0
  #
  # https://github.com/Mic92/nix-update/issues/534 fixes non-root flakes (I always wondered whether this would bite me in the ass)
  passthru = {
    sources = {
      "x86_64-linux" = fetchurl {
        url = "https://redist.ctr-electronics.com/tools/passerine/${finalAttrs.version}/passerine-${finalAttrs.version}-linuxx86-64";
        hash = "sha256-FvhUbLVHmLQL4+mF5hNPY1XNlitZCehpnVs4ijpRuMQ=";
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
        nix-update
      ];
      text = ''
        set -euo pipefail

        tool_name="passerine"
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

        # this apparently doesn't work if you're flake isn't at the root of the repo
        # nix-update --flake --version="$NEW_VERSION" "${finalAttrs.pname}"

        # nix flake metadata --json
        # nix-update --flake -f pkgs/common/ctre-passerine-bin/package.nix --version="$NEW_VERSION" "${finalAttrs.pname}"

        for platform in ${lib.escapeShellArgs finalAttrs.meta.platforms}; do
          nix-update --flake --version="$NEW_VERSION" --system="$platform" "${finalAttrs.pname}"
          # update-source-version "ctre-$tool_name-bin" "$NEW_VERSION" --ignore-same-version \
          #   --file=./pkgs/common/ctre-passerine-bin/package.nix --source-key="sources.$platform"
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
