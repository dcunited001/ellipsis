{ stdenv, lib, fetchFromGitHub }:

stdenv.mkDerivation rec {
  name = "omarchy-scripts-${version}";
  srcVersion = "3.0.2";
  version = "3.0.2-revision0"; # increment revision number when package changes

  src = fetchFromGitHub {
    owner = "basecamp";
    repo = "omarchy";
    rev = "v${srcVersion}";
    sha256 = "sha256-1QJBoMe6MzaD/dcOcqC8QpRxG0Z2c1p+WYqtNFlsTOA=";
  };

  # from android-udev-rules
  installPhase = ''
    runHook preInstall
    install -D bin/omarchy-cmd-terminal-cwd $out/bin/omarchy-cmd-terminal-cwd
    runHook postInstall
  '';

  doInstallCheck = true;

  meta = {
    description =
      "A collection of scripts from DHH's opinionated arch/hyprland setup.";
    homepage = "https://github.com/basecamp/omarchy";

    license = lib.licenses.mit;
    platforms = lib.platforms.linux;
  };
}
