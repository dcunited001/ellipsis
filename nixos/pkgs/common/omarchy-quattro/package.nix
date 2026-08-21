{
  stdenv,
  lib,
  fetchFromGitHub,
  installShellFiles,
  makeWrapper,
  python3,
  gobject-introspection,
}:
let
  pythonEnv = python3.withPackages (ps: [
    ps.pygobject3
  ]);
in

# this is probably a terrible idea. also, nothing in ./share/omarchy shows up
# in the user's profile when added to users.users.dc.packages...
#
# test with `nix shell .#omarchy-quattro` or with nix build
stdenv.mkDerivation rec {
  pname = "omarchy-quattro";
  name = "omarchy-quattro-${version}";
  commit = "ed7bae4ac5a570e9df307486e0202fdafcc6ee24";
  version = "4.0.0-me1"; # increment revision number when package changes

  # version = "4.0.0";

  src = fetchFromGitHub {
    owner = "basecamp";
    repo = "omarchy";
    # rev = "v${version}";
    rev = "${commit}";
    hash = "sha256-0f9BAeRUp69o4OxyOqPV+ytFtOkpZA9MCWPL2Z5F/I0=";
  };

  # TODO: check uwsm setup (omarchy expects mise in uwsm environment)
  # TODO: compare environments
  # TODO: (hopefully) fix whatever confusing issues occur with hyprlock & omarchy shell locking

  nativeBuildInputs = [
    installShellFiles
    makeWrapper
    pythonEnv
  ];

  buildInputs = [
    gobject-introspection
  ];

  # can't be known in advance: set in the user's environment...
  # OMARCHY_PATH = "/run/current-system/sw/share/omarchy";
  # OMARCHY_PATH = "$HOME/.nix-profile/share/omarchy";

  installPhase = ''
    runHook preInstall

    substituteInPlace "default/hypr/omarchy.lua" \
      --replace "require(\"default.hypr.autostart\")" "-- require(\"default.hypr.autostart\")"

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
    patchShebangs $out/share/omarchy/bin

    for prog in $out/share/omarchy/bin/*; do
      if [ -f "$prog" ] && head -n1 "$prog" | grep -q "python"; then
        wrapProgram "$prog" \
          --prefix GI_TYPELIB_PATH : "$GI_TYPELIB_PATH"
      fi
    done

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
