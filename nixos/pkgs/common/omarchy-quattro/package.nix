{
  stdenv,
  lib,
  fetchFromGitHub,
  installShellFiles,
}:

# this is probably a terrible idea. also, nothing in ./share/omarchy shows up
# in the user's profile when added to users.users.dc.packages...
#
# test with `nix shell .#omarchy-quattro` or with nix build
stdenv.mkDerivation rec {
  pname = "omarchy-quattro";
  name = "omarchy-quattro-${version}";
  version = "4.0.0";

  src = fetchFromGitHub {
    owner = "basecamp";
    repo = "omarchy";
    rev = "v${version}";
    hash = "sha256-tge1Sp/Gn6ZNk/I0i4QjIXp+hNuBN2nZCPvcqVLum5Q=";
  };

  nativeBuildInputs = [ installShellFiles ];

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

  postInstall = ''
    installShellCompletion --bash --name omarchy.bash $out/share/omarchy/default/bash/completions
  '';

  doInstallCheck = true;

  meta = {
    description = "Beautiful, Modern & Opinionated Linux.";
    homepage = "https://github.com/basecamp/omarchy";

    license = lib.licenses.mit;
    platforms = lib.platforms.linux;
  };
}
