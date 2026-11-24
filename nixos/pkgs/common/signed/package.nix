{
  copyDesktopItems,
  cmake,
  fetchFromGitHub,
  installShellFiles,
  lib,
  pkg-config,
  qt6Packages,
  stdenv,
  poppler,
  qpdf,
}:
stdenv.mkDerivation (finalAttrs: {
  pname = "signed";
  version = "0.3.2";

  # note: some kinda multimedia issue when running. it builds fine.

  src = fetchFromGitHub {
    owner = "omacom";
    repo = "signed";
    rev = "v${finalAttrs.version}";
    hash = "sha256-gTaHcPbmHrXbg8qWuWrSgprEyy34imRrQUFPK4uBh0A=";
  };

  nativeBuildInputs = [
    cmake
    pkg-config
    qt6Packages.wrapQtAppsHook
    copyDesktopItems
    installShellFiles
  ];

  buildInputs = [
    poppler
    qpdf
    qt6Packages.qtbase
    qt6Packages.poppler
    qt6Packages.qtdeclarative
    qt6Packages.qtmultimedia
    qt6Packages.qttools
  ];

  installPhase = ''
    # pwd
    # ls -al 
    install -m0755 -Dt $out/bin/ signed
  '';

  # checks assume hardware access, i think.
  # doCheck = true;

  desktopItems = [ "signed.desktop" ];

  meta = {
    description = "A simple webcam recorder for Omarchy";
    homepage = "https://github.com/omacom/signed";
    license = lib.licenses.mit;
    # maintainers = with lib.maintainers; [ ];
    mainProgram = "signed";
    platforms = lib.platforms.linux;
  };
})
