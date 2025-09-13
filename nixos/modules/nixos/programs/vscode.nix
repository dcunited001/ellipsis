{ inputs, config, lib, pkgs, ... }:
let
  frcPkgs = inputs.frc-nix.packages.${pkgs.system};
  fhsPkgs = with pkgs;
    [ stdenv.cc.cc.lib zlib openssl.dev pkg-config jdk17 jdt-language-server ]
    ++ [
      build-bin-tool
      build-java-tool
      datalog-tool
      glass
      outlineviewer
      pathweaver
      roborioteamnumbersetter
      robotbuilder
      shuffboard
      smartdashboard
      sysid
      utility
      wpical
    ];
  vscFhs = pkgs.vscode.fhsWithPackages (ps: with ps; fhsPkgs);
  vscExtensions = with pkgs.vscode-extensions;
    [
      bbenoist.nix
      golang.go
      twxs.cmake
      ms-vscode.cpptools
      redhat.java
      redhat.vscode-yaml
      ms-python.python
      ms-python.debugpy
      ms-python.vscode-pylance
      ms-python.isort
      ms-python.black-formatter
      vscjava.vscode-java-pack
      vscjava.vscode-gradle
    ] ++ [ inputs.frc-nix.packages.${pkgs.system}.vscode-wpilib ];

  vscFinal = pkgs.vscode-with-extensions.override {
    vscode = vscFhs;
    vscodeExtensions = vscExtensions;
  };
in {
  environment.systemPackages = [ vscFinal ];
}

# If you're in a hurry, just export these from the ./gradlew script (only for testing)
#
# export HALSIM_EXTENSIONS="$PWD"/build/jni/release/libhalsim_gui.so  # if not setting up build.gradle
# export JAVA_HOME="$HOME"/wpilib/2025/jdk/

# The logs print to ~/wpilib/2025/logs/wpilibtoollog.txt
