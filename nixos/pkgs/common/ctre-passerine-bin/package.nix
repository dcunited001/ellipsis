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
# makeWrapper,

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
        nix-update --flake --version="$NEW_VERSION" "${finalAttrs.pname}"

        # nix flake metadata --json
        # nix-update --flake -f pkgs/common/ctre-passerine-bin/package.nix --version="$NEW_VERSION" "${finalAttrs.pname}"

        # for platform in ${lib.escapeShellArgs finalAttrs.meta.platforms}; do
        #   update-source-version "ctre-$tool_name-bin" "$NEW_VERSION" --ignore-same-version \
        #     --file=./pkgs/common/ctre-passerine-bin/package.nix --source-key="sources.$platform"
        # done

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
