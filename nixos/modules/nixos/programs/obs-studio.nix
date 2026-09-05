{ pkgs, ... }:
{
  # Phew! it wasn't this. (just needed `fc-cache -rv` for gtk, since glib updated)
  # https://github.com/NixOS/nixpkgs/issues/267101 (can't recall with "it" was lol)
  programs.obs-studio.enable = true;
  programs.obs-studio = {
    # enableVirtualCamera = true;
    # TODO: also add security.polkit.enable?
    plugins = with pkgs.obs-studio-plugins; [
      wlrobs
      obs-pipewire-audio-capture
      obs-vkcapture
      obs-source-clone
      # obs-move-transition
      obs-composite-blur
      obs-backgroundremoval
    ];
  };

  # other port ranges untested: MOMENTO FATTUM FINGERI

  # 18a19,21
  # > -A nixos-fw -i wlp4s0 -p tcp -m tcp --dport 5959 -j nixos-fw-accept
  # > -A nixos-fw -i wlp4s0 -p tcp -m tcp --dport 5960 -j nixos-fw-accept
  # > -A nixos-fw -i wlp4s0 -p tcp -m tcp --dport 5961:5965 -j nixos-fw-accept
  # 28a32
  # > -A nixos-fw -i wlp4s0 -p udp -m udp --dport 5960:5965 -j nixos-fw-accept

  programs.obs-distroav = {
    enable = true;
    discovery = {
      enable = true;
      interface = "wlp4s0";
    };
    ndi-stream = {
      enable = true;
      interface = "wlp4s0";
    };
  };
}
