{
  copyDesktopItems,
  cmake,
  fetchFromGitHub,
  installShellFiles,
  lib,
  pkg-config,
  qt6Packages,
  stdenv,
  kdePackages,
  wayland-protocols,
  wayland-scanner,
  libdeflate,
}:
stdenv.mkDerivation (finalAttrs: {
  pname = "omasnap";
  version = "1.26.2";

  # note: some kinda multimedia issue when running. it builds fine.

  src = fetchFromGitHub {
    owner = "omacom";
    repo = "omasnap";
    rev = "v${finalAttrs.version}";
    hash = "sha256-4mNe+ZOaBV/nrSd9Wos+eBR+vb/1HQOrD5J2mlJ2uck=";
  };

  nativeBuildInputs = [
    cmake
    pkg-config
    qt6Packages.wrapQtAppsHook
    copyDesktopItems
    installShellFiles
    wayland-scanner
  ];

  buildInputs = [
    qt6Packages.qtbase
    qt6Packages.qtdeclarative
    qt6Packages.qtwayland
    kdePackages.layer-shell-qt
    libdeflate
  ];

  postPatch = ''
    substituteInPlace "CMakeLists.txt" \
      --replace-fail "/usr/share/wayland-protocols" "${wayland-protocols}/share/wayland-protocols"
  '';

  desktopItems = [ "omasnap.desktop" ];

  meta = {
    description = "A simple webcam recorder for Omarchy";
    homepage = "https://github.com/omacom/omasnap";
    license = lib.licenses.mit;
    mainProgram = "omasnap";
    platforms = lib.platforms.linux;
  };
})
