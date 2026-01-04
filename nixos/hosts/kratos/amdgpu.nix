{
  config,
  lib,
  pkgs,
  ...
}:
{

  nixpkgs = {
    config = {
      allowUnfree = true;
      rocmSupport = true;
    };
  };

  environment.systemPackages = [
    pkgs.gpu-viewer
    pkgs.clinfo
    pkgs.radeontop
    pkgs.vulkan-tools
    pkgs.vulkan-loader

    # VDPAU: Video Decode/Presentation API For Unix
    pkgs.vdpauinfo # `vdpauinfo` query caps 4

    # VA: Video Acceleration: for AMD, the mesa drivers check the box
    pkgs.libva-utils # `vainfo` list of supported profiles (VP9, VA1)

    # these are tools and should maybe be in users' packages
    pkgs.rocmPackages.rocminfo
    pkgs.rocmPackages.rocm-smi
    pkgs.rocmPackages.rocm-core
    # pkgs.rocmPackages.rocmPath # removed
    pkgs.rocmPackages.rccl

    pkgs.btop-rocm
  ];

  # For VAAPI/VDPAU compatibility:
  #
  # https://wiki.archlinux.org/title/Hardware_video_acceleration#Comparison_tables

  # ---------------------------------------------
  # AMDGPU driver
  hardware.amdgpu.opencl.enable = true;

  # equivalent to boot.initrd.kernelModules = [ "amdgpu" ];
  hardware.amdgpu.initrd.enable = true; # could cause problems
  # boot.initrd.kernelModules = ["amdgpu"] # the above sets this
  # boot.kernelModules = ["amdgpu"] # can also set this

  # =============================================
  # Runtime

  # -----------------------
  # dynamic linking
  hardware.graphics.extraPackages = [
    pkgs.rocmPackages.clr
    pkgs.rocmPackages.clr.icd
    # pkgs.libva
    # pkgs.libva-vdpau-driver
    # pkgs.libvdpau-va-gl
  ];

  # -----------------------
  # Environment

  # These may cause problems in a multi-GPU environment
  environment.sessionVariables.AMD_VULKAN_ICD = "RADV";
  environment.sessionVariables.HCC_AMDGPU_TARGET = "gfx906"; # specific to my 6700
  environment.sessionVariables.HSA_OVERRIDE_GFX_VERSION = "10.3.0";

  # https://github.com/alyraffauf/bazznix/blob/24d345beb5de17acb6e33d906d5b482c85403f13/hwModules/common/gpu/amd/default.nix#L3
  # environment.sessionVariables.VDPAU_DRIVER = "radeonsi";

  # https://github.com/randomizedcoder/nixos/blob/2fa7e78ab73141d87d9f391fd0b42d789da66a9a/desktop/l/home.nix#L69-L73
  # home.sessionVariables.LD_LIBRARY_PATH = "${pkgs.libdrm}/lib";

  # building onnxruntime package to support either CUDA or ROCm
  # https://github.com/randomizedcoder/nixos/blob/2fa7e78ab73141d87d9f391fd0b42d789da66a9a/desktop/l/custom-packages/onnxruntime/package.nix#L32

  # TODO: correct variables for amd?
  # environment.sessionVariables = {
  #   LIBVA_DRIVER_NAME = "iHD";
  #   VDPAU_DRIVER = "va_gl";
  # };

  # =============================================
  # Programs

  # -----------------------
  # Blender HIP (and anything that links rocm)
  systemd.tmpfiles.rules = [
    "L+ /opt/rocm/hip - - - - ${pkgs.rocmPackages.clr}"
  ];
}

# TODO: nix: consider mangohud (overlay gpu perf), amdgpu-i2c (rgb control)
# TODO: nix: amdgpu AMF? libamdenc (req. amdgpu-pro?)

# ---------------------------------------------
# LACT: Tune GPU (overdrive recommended)
# hardware.amdgpu.overdrive.enable = true;
# services.hardware.lact.enable = true;
#
# Most of these expand the system profile's dependency scope. some light
# overrides would help somewhat (but often require restarts)

# ---------------------------------------------
# llama-cpp + amd? (req. rocblas)
# virglrenderer: qemu with GPU rendering for guests
#
# use variants.pkgsRocm
#
# + torchWithRocm, but maybe no JAX
# - https://github.com/NixOS/nixpkgs/tree/master/pkgs/top-level/variants.nix#L116

# ---------------------------------------------
# Blender HIP compatability
# https://github.com/SquirrelModeller/squirrel-nixos/blob/d24bd1fcdbec17838e04a292d530ebb7c86772c0/modules/hardware/gpu/amd.nix#L3-L22

#   systemd.tmpfiles.rules = [
#     "L+ /opt/rocm/hip - - - - ${pkgs.rocmPackages.clr}"
#   ];

# ---------------------------------------------
# Running ollama with rocm (on AMD 6700)

# services.ollama = {
#   enable = true;
#   acceleration = "rocm";
#   host = "0.0.0.0";

#   loadModels = [
#     "gemma3:12b"
#     "gemma3:4b"
#     "nomic-embed-text"
#   ];

#   openFirewall = true;
#   rocmOverrideGfx = "10.3.0"; # Wepretend because ollama/ROCM does not support the 6700.
# };



