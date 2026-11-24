{
  cmake,
  fetchFromGitHub,
  lib,
  qt6Packages,
  stdenv,
}:
stdenv.mkDerivation (finalAttrs: {
  pname = "plain";
  version = "0.4.2";

  # note: some kinda multimedia issue when running. it builds fine.

  src = fetchFromGitHub {
    owner = "omacom";
    repo = "plain";
    rev = "v${finalAttrs.version}";
    hash = "sha256-q6CgaS0pO2Hx7jcET9+cePSpaemlzD8c89wQekZfqno=";
  };

  nativeBuildInputs = [
    cmake
    qt6Packages.wrapQtAppsHook
  ];

  buildInputs = [
    qt6Packages.qtbase
    qt6Packages.qtdeclarative
    qt6Packages.qtwayland
  ];

  installPhase = ''
    install -m0755 -Dt $out/bin/ plain
    install -m0644 -Dt $out/share/applications ../pkgbuild/plain.desktop
    install -m0644 -Dt $out/share/icons/hicolor/scalable/apps ../pkgbuild/plain.svg
  '';

  meta = {
    description = "Omarchy's text editor for config files";
    homepage = "https://github.com/omacom/plain";
    license = lib.licenses.mit;
    mainProgram = "plain";
    platforms = lib.platforms.linux;
  };
})
