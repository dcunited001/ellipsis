{
  lib,
  rustPlatform,
  fetchFromGitHub,
  pkg-config,
  wrapGAppsHook4,
  gdk-pixbuf,
  gtk4-layer-shell,
  glib,
  gtk4,
  libadwaita,
  libepoxy,
  libGL,
  copyDesktopItems,
  installShellFiles,
  libxkbcommon,
}:

rustPlatform.buildRustPackage (finalAttrs: {

  pname = "tensaku";
  version = "0.28.0";

  # when bumping the version above, unless these hashes change, it doesn't
  # bump the version used for build (for 26.2 -> 29.0 anyways)
  src = fetchFromGitHub {
    owner = "jondkinney";
    repo = "tensaku";
    rev = "v${finalAttrs.version}";
    hash = "sha256-rkLDfzGFonNghDspDDH6sLikOC/5TZtUCvIPHWtdLXI=";
  };

  cargoHash = "sha256-eFG6MhSnoPzwSX8FkK+qFOSCFsCJay8jiFAMeXgNrds=";

  # 0.29.0 fails: test scroll_capture::auto_scroll::tests::capture_loop_stops_after_two_probe_scrolls_without_terminal_ack
  # hash = "sha256-IAjvMaN0R+dPtdOR26uLYRT+yjpIz/ZA3V4pKa6nue4=";
  # cargoHash = "sha256-q+jS+NX/AKWwaidEQrJMF2hMW+UCb1XJ9zA9Tc5iH5A=";

  # Generate shell completions and man file
  buildFeatures = [ "ci-release" ];

  nativeBuildInputs = [
    copyDesktopItems
    pkg-config
    wrapGAppsHook4
    installShellFiles
  ];

  buildInputs = [
    gdk-pixbuf
    gtk4-layer-shell
    glib
    gtk4
    libadwaita
    libepoxy
    libGL
    libxkbcommon
  ];

  # - installing tensaku-edit required chmod -> patchShebanges -> install
  # - instead of install -> patch

  # https://github.com/omacom/omarchy-pkgs/blob/master/pkgbuilds/tensaku/PKGBUILD#L38-L50
  postInstall = ''
    chmod 755 assets/tensaku-edit
    patchShebangs assets/tensaku-edit
    install -m755 -Dt $out/bin/ assets/tensaku-edit

    install -m644 -Dt $out/share/icons/hicolor/scalable/apps/ assets/tensaku.svg
    install -m644 -Dt $out/share/applications/ dev.tensaku.Tensaku.desktop
    install -m644 -Dt $out/share/man/man1/ man/tensaku.1
    install -m644 -Dt $out/share/licenses/tensaku/ LICENSE
    install -m644 -Dt $out/share/licenses/tensaku/ NOTICE

    installShellCompletion --cmd tensaku \
      --bash completions/tensaku.bash \
      --fish completions/tensaku.fish \
      --zsh completions/_tensaku
  '';

  desktopItems = [ "tensaku.desktop" ];

  meta = {
    description = "Screenshot annotation tool inspired by Swappy and Flameshot";
    homepage = "https://github.com/Tensaku-org/jondkinney";
    license = lib.licenses.mpl20;
    # maintainers = with lib.maintainers; [ ];
    mainProgram = "tensaku";
    platforms = lib.platforms.linux;
  };
})
