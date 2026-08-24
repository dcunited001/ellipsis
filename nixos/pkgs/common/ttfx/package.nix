{
  lib,
  rustPlatform,
  fetchFromGitHub,
  glib,
}:

rustPlatform.buildRustPackage (finalAttrs: {
  pname = "ttfx";
  version = "0.3.2";

  src = fetchFromGitHub {
    owner = "omacom-io";
    repo = "ttfx";
    rev = "v${finalAttrs.version}";
    hash = "sha256-bwFjC6ZkZibkgXjoYVH2VuqqeXklGR9kmRl2fTitWBU=";
  };

  cargoHash = "sha256-DNrg12MNqBcQi6yvoJObM1gtE90iGBCxeQ3RwueYCE4=";

  # nativeBuildInputs = [
  # ];

  buildInputs = [
    glib
  ];

  # postInstall = ''

  # '';

  # pkgname="ttfx"
  # install -Dm755 "target/release/ttfx" "$out/usr/bin/ttfx"

  # install -Dm644 README.md "$out/usr/share/doc/$pkgname/README.md"
  # ./target/release/ttfx --print-completion bash \
  #   | install -Dm644 /dev/stdin "$out/usr/share/bash-completion/completions/$pkgname"
  # ./target/release/ttfx --print-completion zsh \
  #   | install -Dm644 /dev/stdin "$out/usr/share/zsh/site-functions/_$pkgname"

  # install -Dt $out/share/icons/hicolor/scalable/apps/ assets/tensaku.svg

  # installShellCompletion --cmd ttfx \
  #   --bash completions/tensaku.bash \
  #   --fish completions/tensaku.fish \
  #   --zsh completions/_tensaku

  desktopItems = [ "tensaku.desktop" ];

  meta = {
    description = "Terminal text effects — single-binary Rust port of terminaltexteffects";
    homepage = "https://github.com/omacom-io/ttfx";
    license = lib.licenses.mpl20;
    # maintainers = with lib.maintainers; [ ];
    mainProgram = "tensaku";
    platforms = lib.platforms.linux;
  };
})
