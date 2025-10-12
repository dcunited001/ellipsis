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

  # TODO: find a way to quickly test these omarchy scripts (bats?)

  # NOTE: I'm just manually managing the =~/.nix-profile= link with the command below.
  # I'm not using =nix-env= or =nix profile=.
  #
  # ln -fs /etc/profiles/per-user/dc ~/.nix-profile

  installPhase = ''
    runHook preInstall
    for f in omarchy-cmd-terminal-cwd\
      omarchy-{refresh,toggle,restart}-waybar\
      omarchy-webapp-{install,remove}\
      omarchy-launch-{,or-focus-}webapp\
      omarchy-webapp-handler-zoom\
      omarchy-refresh-config; do
      install -D "bin/$f" "$out/bin/$f"
    done
    runHook postInstall
  '';

  # omarchy-webapp-handler-zoom: only called if zoom is installed via
  # `omarchy-webapp-install "Zoom" ...` which is called in
  # ./install/packaging/webapps.sh. I'd rather have the native zoom (i think?)

  # strace xdg-settings set default-web-browser chromium-browser.desktop
  #
  # thanks strace (where is this stored? it gets pre-empted by $BROWSER)

  doInstallCheck = true;

  meta = {
    description =
      "A collection of scripts from DHH's opinionated arch/hyprland setup.";
    homepage = "https://github.com/basecamp/omarchy";

    license = lib.licenses.mit;
    platforms = lib.platforms.linux;
  };
}
