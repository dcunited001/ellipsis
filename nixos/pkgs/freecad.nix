{
  config,
  lib,
  pkgs,
  ...
}:
let
  # from https://codeberg.org/vandenoever/FreeCAD/src/commit/f19ed71904a82a71d38166a546cf9b5cca454c5d/package/nix/flake.nix
  #
  # - this is about a year old, need to be converted to work 4 flake
  # - ifcopenshell has a python devshell

  # pkgs = import nixpkgs { inherit system; };
  ifcopenshell = pkgs.ifcopenshell.overrideAttrs (
    old: with pkgs; rec {
      version = "231007";
      src = fetchFromGitHub {
        owner = "IfcOpenShell";
        repo = "IfcOpenShell";
        rev = "blenderbim-${version}";
        fetchSubmodules = true;
        sha256 = "sha256-7/N3q5ocrYnjWt+LpOwmhjFW4PuI3sHBp4A3vARGcGc=";
      };

      # nixpkgs has a python ifcopenshell, build is different

      # - https://github.com/NixOS/nixpkgs/blob/eb6fbcf24f82bfef9c8cae5d2d4018fbe972fe8f/pkgs/development/python-modules/ifcopenshell/default.nix#L138
      # - build freecad with ifcSupport to get it's python libs in the same env
      # - it actually builds it in regardless. still a good example of how to do this
      #
      buildInputs = old.buildInputs ++ [
        cgal_5
        gmp
        hdf5-cpp
        mpfr
        nlohmann_json
        opencascade-occt
        opencollada
        pcre
        swig
      ];
      # - nixpkgs uses ${lib.getLib pkgs.aLib} for these, instead of pkgs.aLib.out
      # - nixpkgs also includes a preCheck phase
      cmakeFlags = [
        # Docs/etc
        "-DBUILD_EXAMPLES=OFF"
        "-DBUILD_DOCUMENTATION=OFF"
        # Qt
        "-DBUILD_QTVIEWER=OFF"
        # Python
        "-DUSERSPACE_PYTHON_PREFIX=ON"
        ### "-DBUILD_IFCPYTHON=on" # may need this
        # Geom
        "-DBUILD_GEOMSERVER=OFF"
        # CityJSON (also, related format CityGML) # https://www.ogc.org/standards/citygml/
        # "-DCITYJSON_SUPPORT=OFF"                # https://www.cityjson.org/
        "-DBUILD_SHARED_LIBS=ON"
        "-DGLTF_SUPPORT=ON"
        # C JSON
        "-DJSON_INCLUDE_DIR=${nlohmann_json}/include"
        # OCC
        # nixpkgs: https://github.com/NixOS/nixpkgs/blob/eb6fbcf24f82bfef9c8cae5d2d4018fbe972fe8f/pkgs/by-name/op/opencascade-occt/package.nix#L26
        "-DOCC_INCLUDE_DIR=${opencascade-occt}/include/opencascade"
        "-DOCC_LIBRARY_DIR=${opencascade-occt}/lib"
        # Collada
        # nixpkgs: https://github.com/NixOS/nixpkgs/blob/eb6fbcf24f82bfef9c8cae5d2d4018fbe972fe8f/pkgs/by-name/op/opencollada-blender/package.nix#L52
        "-DOPENCOLLADA_INCLUDE_DIR=${opencollada}/include/opencollada"
        "-DOPENCOLLADA_LIBRARY_DIR=${opencollada}/lib/opencollada"
        # Swig
        # "-DSWIG_EXECUTABLE=${swig}/bin/swig"
        # XML
        "-DLIBXML2_INCLUDE_DIR=${libxml2.dev}/include/libxml2"
        "-DLIBXML2_LIBRARIES=${libxml2.out}/lib/libxml2${stdenv.hostPlatform.extensions.sharedLibrary}"
        # GMP
        "-DGMP_INCLUDE_DIR=${gmp.dev}/include"
        "-DGMP_LIBRARY_DIR=${gmp.out}/lib" # nixpkgs: only /lib
        # MPFR
        "-DMPFR_INCLUDE_DIR=${mpfr.dev}/include"
        "-DMPFR_LIBRARY_DIR=${mpfr.out}/lib" # nixpkgs: only /lib
        # Eigen
        # "-DEIGEN_DIR=${eigen}/include/eigen3"   # nixpkgs includes this
        # CGAL
        # nixpkgs: https://github.com/NixOS/nixpkgs/blob/eb6fbcf24f82bfef9c8cae5d2d4018fbe972fe8f/pkgs/by-name/cg/cgal/package.nix#L20
        "-DCGAL_INCLUDE_DIR=${cgal_5.out}/include"
        # HFC5
        "-DHDF5_SUPPORT=ON"
        "-DHDF5_INCLUDE_DIR=${hdf5-cpp.dev}/include"
        "-DHDF5_LIBRARIES=${hdf5-cpp.out}/lib/libhdf5_cpp${stdenv.hostPlatform.extensions.sharedLibrary}"
      ];
    }
  );
in
rec {
  # TODO use wrapProgram or makeWrapper or mkApp. see `nix app` impl.
  # https://github.com/numtide/flake-utils/blob/main/lib.nix

  # `nix build`
  packages.freecad = pkgs.freecad.overrideAttrs (old: {
    buildInputs = old.buildInputs ++ [
      pkgs.yaml-cpp
      ifcopenshell
    ];
  });
  # defaultPackage = packages.freecad;

  # `nix run`
  # apps.freecad = flake-utils.lib.mkApp { drv = packages.freecad; };
  # defaultApp = apps.freecad;
}
