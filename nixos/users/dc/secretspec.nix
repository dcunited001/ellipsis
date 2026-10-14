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
        rev = "sha256-h4r+2zaJHBrJTuwC3EWDkrzg24du/rOSfn7WNL/YB0E=";
        sha256 = "sha256-h4r+2zaJHBrJTuwC3EWDkrzg24du/rOSfn7WNL/YB0E=";
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
      ];

      buildInputs = (prev.buildInputs or [ ]) ++ [
        pkgs.openssl
        pkgs.yyjson
      ];

      env = {
        # Get openssl-sys to use pkg-config
        OPENSSL_NO_VENDOR = 1;
        RUSTFLAGS = "-l yyjson";
      };

      postPatch = ''
        patchShebangs secretspec/tests/fixtures/bw-shim.sh
      '';

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
    # secretspecX509
    # pkgs.devenv
    pkgs.secretspec

    pkgs.age
    pkgs.age-plugin-openpgp-card
    pkgs.age-plugin-yubikey
    pkgs.age-plugin-tpm
    pkgs.agenix-cli
    pkgs.pass
    pkgs.sshpass

    pkgs.net-snmp # snmptranslate describes OIDs like "1.3.6.1.5.5.7.3.2"
  ];
}
