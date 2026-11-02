{
  lib,
  stdenv,
  fetchFromGitHub,
  pkg-config,
  glib,
  glibc,
  cmake,
  h3,
  gdal,
  shapelib,
}:

stdenv.mkDerivation (finalAttrs: {

  pname = "dggrid";
  version = "9.1";

  # when bumping the version above, unless these hashes change, it doesn't
  # bump the version used for build (for 26.2 -> 29.0 anyways)
  src = fetchFromGitHub {
    owner = "sahrk";
    repo = "dggrid";
    rev = "v${finalAttrs.version}";
    hash = "sha256-Fer/A5SR5yVouiXlpJdCvXyEN+FLt1Vl9GTRJuN02Sg=";
  };

  nativeBuildInputs = [
    cmake
    pkg-config
  ];
  buildInputs = [
    gdal
    shapelib
    h3
  ];
  cmakeFlags = [
    # "-DCMAKE_PREFIX_PATH=${h3}/lib"
    # (lib.cmakeFeature "CMAKE_EXE_LINKER_FLAGS" "-static")
  ];

  # logs show H3 must be >= 4.4.1
  # H3_VERSION_OK

  # -DWITH_EXT_SHAPELIB=ON
  # -DSHAPELIB_ROOT_DIR=/..

  installPhase = ''
    runHook preInstall

    # Create the destination directory
    mkdir -p $out/bin

    # ls -al $out
    # ls -al src/apps
    # ls -al src/apps/dggrid

    # Locate the compiled binary inside the build directory and copy it
    cp src/apps/dggrid/dggrid $out/bin/

    runHook postInstall
  '';

  # TODO: tests need to patch shebangs & bin name in ./examples/doexamples
  #
  # some of the generated data needs cleanup to prevent polygons from wrapping
  # around the antimeridian

  meta = {
    description = "A command-line application that generates and manipulates icosahedral discrete global grids.";
    homepage = "https://github.com/sahrk/DGGRID";
    license = lib.licenses.agpl3Only;
    # maintainers = with lib.maintainers; [ ];
    mainProgram = "dggrid";
    platforms = lib.platforms.linux;
  };
})
