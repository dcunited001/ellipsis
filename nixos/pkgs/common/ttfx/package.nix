{
  lib,
  rustPlatform,
  fetchFromGitHub,
  glib,
}:

rustPlatform.buildRustPackage (finalAttrs: {
  pname = "ttfx";
  version = "0.3.3";

  src = fetchFromGitHub {
    owner = "omacom-io";
    repo = "ttfx";
    rev = "v${finalAttrs.version}";
    hash = "sha256-N28CYWQ71hfMm2dEcfLFf1qsLXWRbCiGd+ygVzCVbU0=";
  };

  cargoHash = "sha256-JKfEgISmX8iIw5Bcr0u7pb5J5TsKCvNSQn3E9tH7Wes=";

  # nativeBuildInputs = [
  #
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
