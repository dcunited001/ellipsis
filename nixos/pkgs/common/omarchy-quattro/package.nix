{
  stdenv,
  lib,
  fetchFromGitHub,
}:

# this is probably a terrible idea. also, nothing in ./share/omarchy shows up
# in the user's profile when added to users.users.dc.packages...
#
# test with `nix shell .#omarchy-quattro` or with nix build
stdenv.mkDerivation rec {
  name = "omarchy-quattro-${version}";
  commit = "0a8359072c4ae42c31b787b7b3f47d8720255d54";
  version = "3.9.9-revision0"; # increment revision number when package changes

  src = fetchFromGitHub {
    owner = "basecamp";
    repo = "omarchy";
    rev = commit;
    sha256 = "sha256-mb3xGJ9Pb4Qha2nQFthaMONP/3iou+oAM3PsjqB5420=";
  };

  # can't be known in advance: set in the user's environment...
  # OMARCHY_PATH = "/run/current-system/sw/share/omarchy";
  # OMARCHY_PATH = "$HOME/.nix-profile/share/omarchy";

  installPhase = ''
    runHook preInstall
    install -d "$out/share/omarchy"
    cp -r "applications" "$out/share/omarchy" 
    cp -r "bin" "$out/share/omarchy" 
    cp -r "config" "$out/share/omarchy" 
    cp -r "default" "$out/share/omarchy" 
    cp -r "install" "$out/share/omarchy" 
    cp -r "shell" "$out/share/omarchy" 
    cp -r "themes" "$out/share/omarchy" 
    install -t "$out/share/omarchy" "icon.txt" 
    install -t "$out/share/omarchy" "icon.png" 
    install -t "$out/share/omarchy" "logo.txt" 
    install -t "$out/share/omarchy" "logo.svg" 
    install -t "$out/share/omarchy" "version" 

    runHook postInstall
  '';

  doInstallCheck = true;

  meta = {
    description = "Beautiful, Modern & Opinionated Linux.";
    homepage = "https://github.com/basecamp/omarchy";

    license = lib.licenses.mit;
    platforms = lib.platforms.linux;
  };
}
