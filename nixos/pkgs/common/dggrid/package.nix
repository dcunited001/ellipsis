{
  lib,
  stdenv,
  fetchFromGitHub,
  pkg-config,
  glib,
  glibc,
  cmake,
  h3_4,
  gdal,
  shapelib,
}:
let

  # logs show H3 must be >= 4.4.1
  h3_static = h3_4.overrideAttrs (
    final: prev: {
      outputs = [ "out" ];
      setOutputFlags = false;

      # split outputs for cmake builds sometimes have problems
      #
      # postInstall = ''
      #   mkdir -p $out/include
      #   pwd
      #   ls -al ..
      #   cp -r ../src/h3lib/include/* $out/include/
      #   cp ./src/h3lib/include/*.h $out/include/ 2>/dev/null || true
      # '';
      cmakeFlags = (prev.cmakeFlags or [ ]) ++ [
        "-DBUILD_SHARED_LIBS=OFF"
      ];
    }
  );
in

stdenv.mkDerivation (finalAttrs: {

  pname = "dggrid";
  version = "9.1";

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
    # h3_4
    h3_static
  ];
  cmakeFlags = [
    # "-DCMAKE_PREFIX_PATH=${h3_4.dev}/lib"
    # (lib.cmakeFeature "CMAKE_EXE_LINKER_FLAGS" "-static")
  ];

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

  # TODO: tests need to patch shebangs & `dggrid` path in
  # ./examples/doexamples
  #
  # Some of the generated data needs cleanup to prevent polygons from wrapping
  # around the antimeridian. The H3 build eliminates this. The poles are still
  # missing. matching up the index IDs is more important to me.

  meta = {
    description = "A command-line application that generates and manipulates icosahedral discrete global grids.";
    homepage = "https://github.com/sahrk/DGGRID";
    license = lib.licenses.agpl3Only;
    # maintainers = with lib.maintainers; [ ];
    mainProgram = "dggrid";
    platforms = lib.platforms.linux;
  };
})
