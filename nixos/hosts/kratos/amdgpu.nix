{
  config,
  lib,
  pkgs,
  ...
}:
{
  # ---------------------------------------------
  # GPU
  hardware.amdgpu.opencl.enable = true;

  # equivalent to boot.initrd.kernelModules = [ "amdgpu" ];
  hardware.amdgpu.initrd.enable = true; # could cause problems

  # LACT: Tune GPU (overdrive recommended)
  # hardware.amdgpu.overdrive.enable = true;
  # services.hardware.lact.enable = true;
  #
  # Most of these expand the system profile's dependency scope. some light
  # overrides would help somewhat (but often require restarts)
  #
  # TODO: nix: consider mangohud (overlay gpu perf), amdgpu-i2c (rgb control)
  # TODO: nix: amdgpu AMF? libamdenc (req. amdgpu-pro?)
  #
  # llama-cpp + amd? (req. rocblas)
  # virglrenderer: qemu with GPU rendering for guests
  #
  # use variants.pkgsRocm
  #
  # + torchWithRocm, but maybe no JAX
  # - https://github.com/NixOS/nixpkgs/tree/master/pkgs/top-level/variants.nix#L116

}
