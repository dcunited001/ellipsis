{
  copyDesktopItems,
  cmake,
  fetchFromGitHub,
  installShellFiles,
  lib,
  pkg-config,
  qt6Packages,
  stdenv,
  libjpeg_turbo,
  libpulseaudio,
}:
stdenv.mkDerivation (finalAttrs: {
  pname = "monologue";
  version = "0.4.0";

  # note: some kinda multimedia issue when running. it builds fine.

  src = fetchFromGitHub {
    owner = "omacom";
    repo = "monologue";
    rev = "v${finalAttrs.version}";
    hash = "sha256-ItL5WqPYJo7gr6uSd9FHVFbvtnJE51tUR4EXVCFeLk4=";
  };

  nativeBuildInputs = [
    cmake
    pkg-config
    qt6Packages.wrapQtAppsHook
    copyDesktopItems
    installShellFiles
  ];

  buildInputs = [
    libjpeg_turbo
    libpulseaudio
    qt6Packages.qtbase
    qt6Packages.qtdeclarative
    qt6Packages.qtimageformats
    qt6Packages.qtkeychain
    qt6Packages.qtmultimedia
    qt6Packages.qtsvg
    qt6Packages.qttools
  ];

  installPhase = ''
    # pwd
    # ls -al 
    install -m0755 -Dt $out/bin/ monologue
  '';

  desktopItems = [ "monologue.desktop" ];

  meta = {
    description = "A simple webcam recorder for Omarchy";
    homepage = "https://github.com/omacom/monologue";
    license = lib.licenses.mit;
    # maintainers = with lib.maintainers; [ ];
    mainProgram = "monologue";
    platforms = lib.platforms.linux;
  };
})
