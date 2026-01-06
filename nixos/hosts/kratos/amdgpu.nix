{
  config,
  lib,
  pkgs,
  ...
}:
let
  rocmOptBuildEnv = pkgs.symlinkJoin {
    name = "rocmOptBuildEnv";
    paths = with pkgs.rocmPackages; [
      # primatives -----------------------
      hipcub
      rocprim
      # hiptensor
      rocthrust

      # math -----------------------
      hipblas-common
      hipblaslt
      hipblas # libhipblas.so.XX
      rocblas # librocblas.so.XX

      hiprand
      rocrand
      rocsparse # librocsparse.so.XX
      hipsparse
      roctracer
      hipfft
      rocfft # librocfft.so.XX
      hipsolver
      rocsolver

      composable_kernel
      # composable_kernel_base # not intended to build directly

      # vision -----------------------
      miopen # libMIOpen.so.XX
      mivisionx
      mivisionx-hip
      # mivisionx-cpu

      # rpp
      rpp-hip
      # rpp-cpu
      # rpp-opencl

      # dev -----------------------
      rocdbgapi

      # build -----------------------
      rocm-comgr
      rocm-device-libs

      # runtime -----------------------
      rocm-core
      rocm-runtime # hsakmt: libhsa-runtime64.so.1.15.0
      clr
      clr.icd
      hipify
      rccl # librccl.so.XX (when rcclSupport is enabled)
    ];

    # python ------------
    # aotriton

    # tools ------------
    # rocminfo
    # rocm-smi

  };
in
{
  nixpkgs = {
    config = {
      allowUnfree = true;
      rocmSupport = true;
    };
  };

  # CLR: Common Language Runtime

  # VDPAU: Video Decode/Presentation API For Unix
  # libglvnd: GL Vendor-Neutral Dispatch (dispatch for systems with multiple gpu drivers)
  # libGL: GL Vendor-Neutral Dispatch Library
  # libGLU: OpenGL Utility Library
  # glfw: multi-platform lib 4 creating opengl contexts & managing input (kbd, mouse, joystick, time)

  environment.systemPackages = [
    pkgs.gpu-viewer
    pkgs.clinfo # rocmPackages.clr contains another `clinfo`
    pkgs.radeontop

    # GL
    pkgs.libGL
    pkgs.libGLU

    pkgs.vdpauinfo # `vdpauinfo` query caps 4

    # VA: Video Acceleration: for AMD, the mesa drivers check the box
    pkgs.libva-utils # `vainfo` list of supported profiles (VP9, VA1)

    # these are tools and should maybe be in users' packages
    pkgs.rocmPackages.amdsmi
    pkgs.rocmPackages.rocminfo
    pkgs.rocmPackages.rocm-smi

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
  # these extraPackages go in /run/opengl-driver

  hardware.graphics.extraPackages = [
    pkgs.libglvnd

    rocmOptBuildEnv

    # pkgs.libva
    # pkgs.libva-vdpau-driver
    # pkgs.libvdpau-va-gl

    # vulkan: blender HIP & Vulkan display seems to work fine without amdvlk
    pkgs.vulkan-tools
    pkgs.vulkan-loader
    pkgs.vulkan-validation-layers
    pkgs.vulkan-extension-layer
  ];

  # -----------------------
  # nix-ld

  programs.nix-ld.enable = true;
  programs.nix-ld.libraries = with pkgs; [
    rocmPackages.clr
    rocmPackages.clr.icd
    libglvnd
    libGL
    glfw
    libdrm
    # xorg.libX11
    # xorg.libXScrnSaver
    # xorg.libXcomposite
    # xorg.libXcursor
    # xorg.libXdamage
    # xorg.libXext
    # xorg.libXfixes
    # xorg.libXi
    # xorg.libXrandr
    # xorg.libXrender
    # xorg.libXtst
    # xorg.libxcb
    # xorg.libxkbfile
    # xorg.libxshmfence
  ];

  # see mic92 dots. many of these are defined only here (implicitly imported
  # into system)
  # https://github.com/Mic92/dotfiles/blob/004e86355aa517fb1d9527d440c1b57cec275a6b/nixosModules/fhs-compat.nix#L12-L70

  # -----------------------
  # Environment

  # https://rocm.docs.amd.com/en/latest/reference/env-variables.html

  # These may cause problems in a multi-GPU environment
  environment.sessionVariables.AMD_VULKAN_ICD = "RADV";
  environment.sessionVariables.HCC_AMDGPU_TARGET = "gfx1030"; # specific to my 6700
  environment.sessionVariables.HSA_OVERRIDE_GFX_VERSION = "10.3.0";

  # TODO: try adding to LD_LIBRARY_PATH? ...
  environment.sessionVariables.ROCM_PATH = "/opt/rocm";
  systemd.tmpfiles.rules = [
    "L+ /opt/rocm - - - - ${rocmOptBuildEnv}"
    # "L+ /opt/rocm/hip - - - - ${pkgs.rocmPackages.clr}"
    # "L+ /opt/rocm/llvm - - - - ${pkgs.rocmPackages.llvm.llvm}"
  ];
  # `rocm_agent_enumerator` says gfx1030

  # https://github.com/alyraffauf/bazznix/blob/24d345beb5de17acb6e33d906d5b482c85403f13/hwModules/common/gpu/amd/default.nix#L3
  # environment.sessionVariables.VDPAU_DRIVER = "radeonsi";
  #
  # VDPAU was removed from mesa (wayland incompat; also VAAPI > VDPAU apparently)
  # https://gitlab.freedesktop.org/mesa/mesa/-/merge_requests/36632

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

  # - needs symlink-join, then bind to /opt/rocm
  #
  # - could also install multiple versions, technically.. though the the
  #   system's running driver seems sometimes involved with builds

  environment.sessionVariables.ROCM_PATH = "/opt/rocm";
  systemd.tmpfiles.rules = [
    "L+ /opt/rocm - - - - ${rocmOptBuildEnv}"
    # "L+ /opt/rocm/hip - - - - ${pkgs.rocmPackages.clr}"
    # "L+ /opt/rocm/llvm - - - - ${pkgs.rocmPackages.llvm.llvm}"
  ];

  # "L+ /opt/rocm/hip - - - - /opt/rocm/hip2" # <--- disable link like this
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
