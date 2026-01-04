{
  config,
  lib,
  pkgs,
  ...
}:
{
  environment.systemPackages = with pkgs; [
    uv
  ];

  users.users.dc.packages = let python = pkgs.python313.withPackages (ps: with ps; [
    numpy
    scipy
    polars
    pandas

    ipython
    matplotlib
    # pyqt6-charts # just using tk
    # anyqt

    torch
    torchvision
    torchbench
    geotorch
    gpytorch
    # pytorch-bench
    # torchsummary
  ]); in [
    python
  ];

  # tinygrad has been broken for ROCm since 10.0.x

  # python313Packages.tinygrad

  # torchdata needs HSA_OVERRIDE_GFX_VERSION=10.3.0 in the build env
  #
  # RuntimeError: No HIP GPUs are available...
  #
  # torchtune -> torchdata (only torchtune deps on torchdata)

  programs.nix-ld.enable = true; # uv magic?
}
