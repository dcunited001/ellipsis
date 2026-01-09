{
  pkgs,
  lib,
  ...
}:
let
  profile = pkgs.symlinkJoin {
    name = "rocmOptBuildEnv";
    paths = with pkgs.rocmPackages; [
      # primatives -----------------------
      hipcub
      rocprim
      # hiptensor
      rocthrust

      # math -----------------------
      hipfort
      hipblas-common
      hipblaslt
      hipblas # libhipblas.so.XX
      rocblas # librocblas.so.XX

      hiprand
      rocrand
      rocsparse # libvgrocsparse.so.XX
      hipsparse
      rocalution
      hipfft
      rocfft # librocfft.so.XX
      hipsolver
      rocsolver

      composable_kernel
      # composable_kernel_base # not intended to build directly
      ck4inductor

      # graphics -----------------------
      hiprt

      # vision -----------------------
      miopen # libMIOpen.so.XX
      migraphx
      mivisionx
      mivisionx-hip
      # mivisionx-cpu

      # rpp
      rpp-hip
      # rpp-cpul
      # rpp-opencl

      # python -----------------------
      # aotriton # implicity included

      # llvm -----------------------
      llvm.bintools
      # llvm.clang-unwrapped
      llvm.compiler-rt
      llvm.libcxx
      llvm.llvm
      llvm.lld
      llvm.openmp
      # llvm.rocmClangStdenv

      # build -----------------------
      rocm-comgr
      rocm-device-libs
      rocm-cmake

      # runtime -----------------------
      rocm-core
      rocm-runtime
      hsakmt # libhsa-runtime64.so.1.15.0 (not included with runtime)
      clr
      clr.icd
      hipify
      rccl # librccl.so.XX (when rcclSupport is enabled)

      # debug -----------------------
      rocgdb
      rocdbgapi
      rocr-debug-agent
      roctracer
      rocprofiler
      rocprofiler-register
      aqlprofile
    ];
  };
in
profile
