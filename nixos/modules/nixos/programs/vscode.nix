{
  inputs,
  pkgs,
  ...
}:
let
  frcPkgs = inputs.frc-nix.packages.${pkgs.stdenv.hostPlatform.system};
  vscMarketplace = pkgs.vscode-utils.extensionsFromVscodeMarketplace [
    {
      # drblury.protobuf-vsc # replaces zxh404.vscode-proto3
      name = "protobuf-vsc";
      publisher = "drblury";
      version = "1.4.28";
      sha256 = "08r1r16xswa8ignsa7zy5j404y677ssiw6wjvjq071wmzblq1pf9";
    }
    {
      name = "vscode-spotless-gradle";
      publisher = "richardwillis";
      version = "1.2.1";
      sha256 = "0sdlg3w5g5v1jcx3qf8lljm2qavj3jas8dgr5gxb3l2yyk8knj1l";
    }
  ];

  # deps to run avalonia WPILIB installer (don't do this: waste of time)
  #
  # - installing in this way is probably a bad idea...
  #   - See WPILibInstaller-Avalonia/ViewModels/InstallPageViewModel.cs#L222
  #     for installation procedure
  # - install these, run from the VSCode FHS container,
  #   - "Install everything", but skip VSCode, remove these avaoloniaDeps afterwards.
  # - again, probably a bad idea (i didn't validate this thoroughly)
  #   - you would need to deeply profile the app, its crossbuilds and deployed
  #     artifacts to know...
  #
  # https://github.com/wpilibsuite/WPILibInstaller-Avalonia/blob/2b61eb0bc954ebb1d1f1bd186a079298a9f77c2f/WPILibInstaller-Avalonia/ViewModels/InstallPageViewModel.cs#L222
  # avaloniaDeps = with pkgs; [
  #   icu
  #   font-config-info
  #   fontconfig.lib
  #   libice
  #   libsm
  # ];

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

      # wayland
      wayland
      # also: jdk17
    ]
    ++ [
      # frcPkgs.datalogtool
      frcPkgs.glass
      frcPkgs.outlineviewer
      # frcPkgs.pathweaver
      frcPkgs.roborioteamnumbersetter
      # frcPkgs.robotbuilder
      # frcPkgs.shuffleboard
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
    frcPkgs.pathplanner
  ];

  programs.nix-ld.enable = true;
}
