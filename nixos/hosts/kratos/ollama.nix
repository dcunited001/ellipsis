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

    host = "127.0.0.1"; # OLLAMA_HOST = "${cfg.host}:${cfg.port}"
    port = 11434;
    openFirewall = false;

    user = "ollama";
    group = "ollama";

    # models = "{cfg.home}/models";
    loadModels = [
      "qwen3-embedding:8b-q8_0"
      "gemma3:12b"
    ];
    syncModels = true;
    rocmOverrideGfx = "10.3.0";

    environmentVariables = {
      AMD_VULKAN_ICD = "RADV";
      # HSA_OVERRIDE_GFX_VERSION = "10.3.0";
      HCC_AMDGPU_TARGET = "gfx1030"; # specific to my 6700
      ROCM_PATH = "/opt/rocm";
      HIP_VISIBLE_DEVICES = "0";

      # OLLAMA_DEBUG = "False"
      # OLLAMA_CONTEXT_LENGTH = "4096";
      # OLLAMA_KEEP_ALIVE = "5m";
      OLLAMA_MAX_LOADED_MODELS = "2";
      OLLAMA_MAX_QUEUE = "10";
      OLLAMA_NUM_PARALLEL = "3";
      # OLLAMA_MODELS = "${cfg.home}/models";
      # OLLAMA_NUM_PARALLEL = "0";
      # OLLAMA_NOPRUNE = "false";
      # OLLAMA_ORIGINS                # CORS
      # OLLAMA_SCHED_SPREAD           # multi-GPU
      # OLLAMA_FLASH_ATTENTION        # experimental, dep on GPU features
      # OLLAMA_KV_CACHE_TYPE = "f16"; # Quantization for K/V cache
      # OLLAMA_LLM_LIBRARY = "";      # auto-detects
      # OLLAMA_GPU_OVERHEAD           # reserve VRAM per-GPU
      # OLLAMA_LOAD_TIMEOUT = "5m";   # wait 4 model load

      # see: https://github.com/ollama/ollama/issues/5026#issuecomment-2811576422
      #
      # OLLAMA_TMPDIR = "/tmp"; # no longer used, ends up being this:
      #
      # /tmp/systemd-private-c85e31b34bf54212884da26acbdf2a01-ollama.service-QKQ0LE/tmp
    };
  };
}
