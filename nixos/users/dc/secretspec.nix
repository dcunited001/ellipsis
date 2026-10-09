{
  config,
  pkgs,
  rustPlatform,
  lib,
  ...
}:
let
  secretspecX509 = pkgs.secretspec.overrideAttrs (

    final: prev: {
      version = "0.21.1.0-x509";

      src = pkgs.fetchFromGitHub {
        repo = "secretspec";
        owner = "dcunited001";
        rev = "cf3f252d971ee402ad2608e497bfca3a7f861efd";
        sha256 = "sha256-u89NMvrkKniwEo1TDWHcKf8RpfyFq22eVrWXOWl69j8=";
        # owner = "cachix";
        # rev = "eba5d53018f9263dc672cd4487492c305aaede2b";
        # sha256 = "sha256-1234SNfhyC9Y56wOMUQqSA14dO64Gu11yB1k4j6xLH4=";
      };

      cargoHash = null;
      cargoDeps = pkgs.rustPlatform.importCargoLock {
        lockFile = "${final.src}/Cargo.lock";
      };

      nativeBuildInputs = (prev.nativeBuildInputs or [ ]) ++ [
        pkgs.pkg-config
        # pkgs.php.unwrapped.dev
      ];

      buildInputs = (prev.buildInputs or [ ]) ++ [
        pkgs.openssl
        pkgs.yyjson
        # pkgs.php.unwrapped.dev
      ];

      env = {
        # Get openssl-sys to use pkg-config
        OPENSSL_NO_VENDOR = 1;
      };

      # =============================================
      # can't whitelist these because --profile release pulls in php anyways
      #
      # cargoBuildFlags = (prev.cargoBuildFlags or [ ]) ++ [
      #   "-p"
      #   "secretspec"
      #   "-p"
      #   "secretspec-derive"
      #   "-p"
      #   "secretspec-ipc"
      #   "-p"
      #   "libsecretspec"
      #   "-p"
      #   "secretspec-ipc"
      #   # "-p"
      #   # "conformance/ipc/runner" # needs a file path
      #   "-p"
      #   "secretspec-py-native"

      #   # can't use exclude here. it's a "--profile release" not a workspace...
      #   # "--exclude"
      #   # "secretspec-php-native"
      # ];

      # =============================================
      # Cargo.toml workspace.members != features...

      # cargoBuildNoDefaultFeatures = true;
      # cargoBuildFeatures = [
      #   "secretspec"
      #   "secretspec-derive"
      #   "libsecretspec"
      #   "secretspec-ipc"
      #   # "conformance/ipc/runner"
      #   # "secretspec-node"
      #   # "secretspec-php"
      #   "secretspec-py"
      # ];

      # cargoHash = "sha256-ewrfnCi4jS/Ou8+3LKp/527vccJgOidvlE83eFk4/0c=";
    }
  );
in
{
  users.users.dc.packages = [
    pkgs.sops
    secretspecX509
    pkgs.secretspec
    pkgs.age
    pkgs.age-plugin-openpgp-card
    pkgs.age-plugin-yubikey
    pkgs.age-plugin-tpm
    pkgs.agenix-cli
    pkgs.pass
    pkgs.sshpass
  ];
}

# cargoDeps = oldAttrs.cargoDeps.overrideAttrs (
# cargoHash = "sha256-BBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBB=";
# );

# cargoDeps = rustPlatform.importCargoLock {
#   lockFile = "${src}/Cargo.lock";
# };

# cargoDeps = prev.cargoDeps.overrideAttrs (
#   let
#     hash = "sha256-ewrfnCi4jS/Ou8+3LKp/527vccJgOidvlE83eFk4/0c=";
#   in
#   _final: _prev: {
#     inherit hash;
#     # Handle older Nixpkgs versions that use outputHash
#     outputHash = hash;
#   }
# );
# cargoHash = "sha256-ewrfnCi4jS/Ou8+3LKp/527vccJgOidvlE83eFk4/0c=";

#   # 1. This forces the final build to expect a new dependency derivation
#   cargoHash = lib.fakeHash;

#   # 2. This instructs the underlying dependency derivation to use the new source and fake hash
#   cargoDeps = prev.cargoDeps.overrideAttrs (
#     # lib.upstreamAtoms (
#     finalAttrs: prevAttrs: {
#       inherit (final) src;
#       outputHash = lib.fakeHash;
#     }
#     # )
#   );

# cargodeps = prev.cargoDeps.overrideAttrs {
#   # name = iamb;
#   inherit (final) src;
#   lockFile = final.src + "/Cargo.lock";
#   outputHash = "sha256-BBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBB=";
# };
# cargoHash = lib.fakeHash; # "sha256-ewrfnCi4jS/Ou8+3LKp/527vccJgOidvlE83eFk4/0c=";

# cargoDeps = prev.cargoDeps.overrideAttrs (
#   let
#     hash = "sha256-ewrfnCi4jS/Ou8+3LKp/527vccJgOidvlE83eFk4/0c=";
#   in
#   _final: _prev: {
#     inherit hash;
#     outputHash = hash;
#   }
# );
