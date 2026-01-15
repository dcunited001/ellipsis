{
  inputs,
  pkgs,
  ...
}:
let
  frcPkgs = inputs.frc-nix.packages.${pkgs.stdenv.hostPlatform.system};
  vscMarketplace = pkgs.vscode-utils.extensionsFromVscodeMarketplace [
    # vscode-lombok is deprecated
    # {
    #   # "created 12,000 symlinks..."
    #   name = "vscode-lombok";
    #   publisher = "vscjava";
    #   version = "1.1.2024071804";
    #   sha256 = "10ppk8s4ppaac91r20hdb2m7kvmsxp15dgisd7f2raqbblk7d9sm";
    # }
    {
      name = "vscode-spotless-gradle";
      publisher = "richardwillis";
      version = "1.2.1";
      sha256 = "0sdlg3w5g5v1jcx3qf8lljm2qavj3jas8dgr5gxb3l2yyk8knj1l";
    }
  ];
  # these fhsPkgs can be set in programs.nix-ld.libraries
  fhsPkgs =
    with pkgs;
    [
      stdenv.cc.cc.lib
      zlib
      openssl.dev
      pkg-config
      jdt-language-server
      libglvnd
      libGL
      glfw
      xorg.libX11
      xorg.libXScrnSaver
      xorg.libXcomposite
      xorg.libXcursor
      xorg.libXdamage
      xorg.libXext
      xorg.libXfixes
      xorg.libXi
      xorg.libXrandr
      xorg.libXrender
      xorg.libXtst
      xorg.libxcb
      xorg.libxkbfile
      xorg.libxshmfence
      # had added these earlier, but unsure of whether they're necessary
      # xorg.libXinerama
      # xorg.libXt
      wayland
    ] # jdk17
    ++ [
      frcPkgs.datalogtool
      frcPkgs.glass
      frcPkgs.outlineviewer
      frcPkgs.pathweaver
      frcPkgs.roborioteamnumbersetter
      frcPkgs.robotbuilder
      frcPkgs.shuffleboard
      # frcPkgs.smartdashboard
      frcPkgs.sysid
      frcPkgs.wpilib-utility
      frcPkgs.wpical
    ];
  vscFhs = pkgs.vscode.fhsWithPackages (ps: with ps; fhsPkgs);
  vscExtensions =
    with pkgs.vscode-extensions;
    [
      bbenoist.nix
      golang.go

      redhat.vscode-yaml
      drblury.protobuf-vsc # zxh404.vscode-proto3
      vscjava.vscode-gradle

      # the extension pack won't work with java
      # vscjava.vscode-java-pack

      # wpilib vscode installs these
      ms-vscode.cpptools
      redhat.java
      vscjava.vscode-java-debug
      vscjava.vscode-java-dependency
      ms-python.python
      ms-python.debugpy
      ms-python.vscode-pylance
      ms-python.isort
      ms-python.black-formatter

      # stkb.rewrap ... word wrap? (but not mode-specific?)

      # remote/ssh has telemetry:
      # ensure telemetry.enableTelemetry=false
      # ms-vscode.remote-explorer
      ms-vscode-remote.remote-ssh
      ms-vscode-remote.remote-containers
      # ms-vscode.remote-server
      ms-toolsai.jupyter-renderers
      ms-toolsai.vscode-jupyter-cell-tags

      ms-vscode.cmake-tools # twxs.cmake

      # to debug the extension: ensure vscode-wpilib isn't included
      #
      # - this VSCode should be able to run the two "Extension.*" targets
    ]
    ++ vscMarketplace
    ++ [ frcPkgs.vscode-wpilib ];

  vscFinal = pkgs.vscode-with-extensions.override {
    vscode = vscFhs;
    vscodeExtensions = vscExtensions;
  };

  # webkitSoup = pkgs.webkitgtk_4_1.overrideAttrs # ... already has it
  #   (finalAttrs: previousAttrs: { buildInputs = libsoup_3 });
  choreoFix = frcPkgs.choreo.overrideAttrs (
    finalAttrs: previousAttrs: { buildInputs = [ pkgs.webkitgtk_4_1 ]; }
  );
in
{
  environment.systemPackages = [
    vscFinal
    frcPkgs.advantagescope
    # choreoFix
    frcPkgs.elastic-dashboard
    # frcPkgs.pathplanner
  ];

  programs.nix-ld.enable = true;
}
