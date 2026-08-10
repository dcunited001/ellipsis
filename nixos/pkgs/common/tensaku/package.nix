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
}:

rustPlatform.buildRustPackage (finalAttrs: {

  pname = "tensaku";
  version = "0.26.2";

  src = fetchFromGitHub {
    owner = "jondkinney";
    repo = "tensaku";
    rev = "v${finalAttrs.version}";
    hash = "sha256-ZzGX+B4ZHfTHySO+a8ZnDpHzjztamrXYDWys82R++/c=";
  };

  cargoHash = "sha256-mm38TdShT8HN4EMzDGFfdMeMyIx4n3inaVvXzJrp8N4=";

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
  ];

  postInstall = ''
    install -Dt $out/share/icons/hicolor/scalable/apps/ assets/tensaku.svg

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
