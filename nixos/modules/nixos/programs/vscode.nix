{ inputs, config, lib, pkgs, ... }:
let
  frcPkgs = inputs.frc-nix.packages.${pkgs.system};
  # these fhsPkgs can be set in programs.nix-ld.libraries
  fhsPkgs = with pkgs;
    [ stdenv.cc.cc.lib zlib openssl.dev pkg-config jdt-language-server ] # jdk17
    ++ [
      # some, but not all wpilib apps should go here. particularly, those that
      # need to link consistently into the FHS.
      #
      # - this avoids GLW issues (which you can usually circumvent on nix/guix
      #   by wrapping in a shell with chromium dev dependencies).
      #
      # - however, these packages aren't added to the top-level environment
      #   (and those won't have the FHS corrections).
      #
      # - This works for some apps, but not for those which need FHS
      #   constraints on the dynamic library dependencies.
      #
      # - thus, it's safer to find a way to spawn these from within VS Code.

      frcPkgs.datalogtool
      frcPkgs.glass
      frcPkgs.outlineviewer
      frcPkgs.pathweaver
      frcPkgs.roborioteamnumbersetter
      frcPkgs.robotbuilder
      frcPkgs.shuffleboard
      frcPkgs.smartdashboard
      frcPkgs.sysid
      frcPkgs.wpilib-utility
      frcPkgs.wpical
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
    ] ++ [ frcPkgs.vscode-wpilib ];

  vscFinal = pkgs.vscode-with-extensions.override {
    vscode = vscFhs;
    vscodeExtensions = vscExtensions;
  };
in {
  environment.systemPackages = [
    vscFinal
    frcPkgs.advantagescope
    frcPkgs.choreo
    frcPkgs.elastic-dashboard
    frcPkgs.pathplanner
  ];

  programs.nix-ld.enable = true;
}
