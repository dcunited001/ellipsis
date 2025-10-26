# This file can by used by Nix to create a development shell with the
# build dependencies:
#     nix develop ./package/nix
# or to compile and run FreeCAD
#     nix run ./package/nix

{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs?ref=nixos-23.11";
    flake-utils.url = "github:numtide/flake-utils";
  };
  outputs =
    {
      self,
      nixpkgs,
      flake-utils,
    }:
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = import nixpkgs { inherit system; };
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
            cmakeFlags = [
              "-DBUILD_GEOMSERVER=OFF"
              "-DBUILD_EXAMPLES=OFF"
              "-DBUILD_DOCUMENTATION=OFF"
              "-DBUILD_SHARED_LIBS=ON"
              "-DBUILD_QTVIEWER=OFF"
              "-DHDF5_SUPPORT=ON"
              "-DGLTF_SUPPORT=ON"
              "-DJSON_INCLUDE_DIR=${nlohmann_json}/include"
              "-DUSERSPACE_PYTHON_PREFIX=ON"
              "-DOCC_INCLUDE_DIR=${opencascade-occt}/include/opencascade"
              "-DOCC_LIBRARY_DIR=${opencascade-occt}/lib"
              "-DOPENCOLLADA_INCLUDE_DIR=${opencollada}/include/opencollada"
              "-DOPENCOLLADA_LIBRARY_DIR=${opencollada}/lib/opencollada"
              "-DLIBXML2_INCLUDE_DIR=${libxml2.dev}/include/libxml2"
              "-DLIBXML2_LIBRARIES=${libxml2.out}/lib/libxml2${stdenv.hostPlatform.extensions.sharedLibrary}"
              "-DGMP_INCLUDE_DIR=${gmp.dev}/include"
              "-DGMP_LIBRARY_DIR=${gmp.out}/lib"
              "-DMPFR_INCLUDE_DIR=${mpfr.dev}/include"
              "-DMPFR_LIBRARY_DIR=${mpfr.out}/lib"
              "-DCGAL_INCLUDE_DIR=${cgal_5.out}/include"
              "-DHDF5_INCLUDE_DIR=${hdf5-cpp.dev}/include"
              "-DHDF5_LIBRARIES=${hdf5-cpp.out}/lib/libhdf5_cpp${stdenv.hostPlatform.extensions.sharedLibrary}"
            ];
          }
        );
      in
      rec {
        # `nix build`
        packages.freecad = pkgs.freecad.overrideAttrs (old: {
          buildInputs = old.buildInputs ++ [
            pkgs.yaml-cpp
            ifcopenshell
          ];
        });
        defaultPackage = packages.freecad;

        # `nix run`
        apps.freecad = flake-utils.lib.mkApp { drv = packages.freecad; };
        defaultApp = apps.freecad;
      }
    );
}
