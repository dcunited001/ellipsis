{ pkgs, ... }:
{
  services.pipewire = {
    enable = true;
    alsa = {
      enable = true;
      support32Bit = true;
    };

    pulse.enable = true;
    jack.enable = true;

    extraConfig.pipewire."91-null-sinks" = {
      "context.objects" = [
        # pipewire seems to already have a dummy driver
        #
        # {
        #   factory = "spa-node-factory";
        #   args = {
        #     "factory.name" = "support.node.driver";
        #     "node.name" = "Dummy-Driver";
        #     "priority.driver" = 8000;
        #   };
        # }
        {
          factory = "adapter";
          args = {
            "factory.name" = "support.null-audio-sink";
            "node.name" = "Microphone-Proxy";
            "node.description" = "Microphone";
            "media.class" = "Audio/Source/Virtual";
            "audio.position" = "MONO";
          };
        }
        {
          factory = "adapter";
          args = {
            "factory.name" = "support.null-audio-sink";
            "node.name" = "Main-Output-Proxy";
            "node.description" = "Main Output";
            "media.class" = "Audio/Sink";
            "audio.position" = "FL,FR";
          };
        }
      ];
    };

    # low-latency settings
    extraConfig.pipewire."92-low-latency" = {
      "context.properties" = {
        "default.clock.rate" = 48000;
        # quantum: buffer size
        "default.clock.quantum" = 256;
        "default.clock.min-quantum" = 256;
        "default.clock.max-quantum" = 256;
      };
    };
    extraConfig.pipewire-pulse."92-low-latency" = {
      context.modules = [
        {
          name = "libpipewire-module-protocol-pulse";
          args = {
            pulse.min.req = "256/48000";
            pulse.default.req = "256/48000";
            pulse.max.req = "256/48000";
            pulse.min.quantum = "256/48000";
            pulse.max.quantum = "256/48000";
          };
        }
      ];
    };

    # bluez5 roles:
    # hsp_hs/ag: headset, audio gateway
    # hfp_hf/ag: hands-free, audio gateway
    # a2dp_sink/source
    # LE Audio BAP: Basic Audio Profile
    # bap_sink/source: BAP Sink/Source
    wireplumber.extraConfig.bluetoothEnhancements = {
      "monitor.bluez.properties" = {
        "bluez5.enable-sbc-xq" = true; # high-quality SBC Codec (A2DP; wp bluez default)
        "bluez5.enable-msbc" = true; # mSBC wideband speech codec (HFP/HSP; wp bluez default)
        "bluez5.enable-hw-volume" = true;
        "bluez5.roles" = [
          "a2dp_sink"
          "a2dp_source"
          "hsp_hs"
          "hsp_ag"
          "hfp_hf"
          "hfp_ag"
        ]; # bap_sink bap_source
      };
    };
  };

  security = {
    rtkit.enable = true;
    # requires rtkit and adding user to realtime group
    pam.loginLimits = [
      {
        domain = "@audio";
        item = "memlock";
        type = "-";
        value = "unlimited";
      }
      {
        domain = "@audio";
        item = "rtprio";
        type = "-";
        value = "99";
      }
      {
        domain = "@audio";
        item = "nofile";
        type = "soft";
        value = "99999";
      }
      {
        domain = "@audio";
        item = "nofile";
        type = "hard";
        value = "99999";
      }
      {
        domain = "@audio";
        item = "nice";
        type = "-";
        value = "-15";
      }
    ];
  };
}
