{
  config,
  lib,
  pkgs,
  ...
}:
{
  services.ollama = {
    enable = true;
    package = pkgs.ollama-rocm;

    host = "127.0.0.1";
    port = "11434";
    openFirewall = false;

    user = "ollama";
    group = "ollama";

    environmentVariables = {
      AMD_VULKAN_ICD = "RADV";
      # HSA_OVERRIDE_GFX_VERSION = "10.3.0";
      HCC_AMDGPU_TARGET = "gfx1030"; # specific to my 6700
      ROCM_PATH = "/opt/rocm";
    };

    # models = "{cfg.home}/models";
    loadModels = [ "qwen3-embedding:8b-q8_0" ];
    syncModels = true;
    rocmOverride = "10.3.0";

  };
}
