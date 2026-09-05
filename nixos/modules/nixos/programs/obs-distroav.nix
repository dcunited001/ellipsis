{
  config,
  pkgs,
  lib,
  ...
}:

let
  cfg = config.programs.obs-distroav;

  ndiVersion = "6"; # 6.3.2.0
  ndiInstallerName = "Install_NDI_SDK_v${ndiVersion}_Linux";
  # ndiFix = lib.overrideDerivation pkgs.ndi-6 (prev: {
  # name = "ndi-6-6.3.2.0";
  ndiFix = pkgs.ndi-6.overrideAttrs (prev: {
    src = (
      pkgs.fetchurl {
        url = "https://downloads.ndi.tv/SDK/NDI_SDK_Linux/${ndiInstallerName}.tar.gz";
        hash = "sha256:f0314f245446defc488b63ceb4689acf1a965aeefdadacb70571bb216a8cc183";
      }
    );
  });
  distroavFix = pkgs.obs-studio-plugins.distroav.override {
    ndi-6 = ndiFix;
  };
in
{
  options.programs.obs-distroav = with lib; {
    # May move this to services.ob-distroav if it starts to configure running services.
    #
    # Initially intended to also configure programs.obs, but only configures ports.

    enable = lib.mkOption {
      type = lib.types.bool;
      default = false;
      description = ''
        Enable firewall configuration for Distroav.
                  
                  Whether or not to enable bundling the DistroAV plugin. All firewall logic will no-op if this is disabled...

                  The following can make NDI and DistroAV fail or perform badly:

                  - Avahi broadcasts the mDNS service with the wrong IP address. Firewall must permit connections to this port/IP.
                  
                  - Per-interface ip routes cause too much traffic to use the same layer-2 subnet. This will become worse once a 6000kpbs stream is started.
                  
                  - Encoding-specific intermittent increase of bandwidth between keyframes. The size of encoded video stream is a proxy for instantaneous layer-2 bandwidth consumption. Videos that would exhibit compression artifacts are likely to require much more bandwidth during their first transmission.

                  - The layer-2 load on a subnet causes WiFi/Ethernet collisions. Layer-2 frame collisions require retransmission. WiFi is brittle, esp. with multiple video streams.

                  Troubleshoot with ss, avahi-browse, socat and other tools. Configuring Avahi to fix this sucks.

                  If outputs are configured in the OBS DistroAV settings, then your computer is probably advertising an NDI video stream with mDNS... on whatever layer2 interfaces that Avahi broadcasts on! Don't be evil.

                  Firewall rules are required for clients to connect and receive a stream. For many reasons, enable/disable these ports on a per-interface basis, as needed.
      '';
    };

    distroav = {
      package = mkPackageOption pkgs [ "obs-studio-plugins" "distroav" ] { };
    };

    discovery = {
      enable = lib.mkOption {
        type = lib.types.bool;
        default = false;
        description = "Whether to open TCP Ports 5959,5960 for DistroAV Discovery Service. Only configures firewall.";
      };

      interface = mkOption {
        type = lib.types.nullOr lib.types.str;
        default = null;
        description = ''
          Name of interface to add firewall rules to.

            Doesn't affect the interface(s) that NDI Discovery Services would bind to (they are not configured here).
        '';
      };

      discoveryTCP = mkOption {
        type = lib.types.listOf lib.types.port;
        default = [ 5959 ];
        example = [ 5959 ];
        description = ''
          NDI Discovery Server is an optional method to have NDI devices perform discovery. This can be beneficial in large configurations when you need to connect NDI devices between subnets or if mDNS is blocked.
        '';
      };

      queryTCP = mkOption {
        type = lib.types.listOf lib.types.port;
        default = [ 5960 ];
        example = [ 5960 ];
        description = "TCP port used for remote sources to query this machine and discover all the sources running on it. This is used, for instance, when a machine is added by an IP address in the access manager so that from an IP address alone, all the sources currently running on that machine can be discovered automatically.";
      };
    };

    ndi-stream = {

      enable = lib.mkOption {
        type = lib.types.bool;
        default = false;
        description = "Open TCP/UDP Ports for active video streams

Listed for documentation purposes. These ports are required for basic NDI usage.

These ports will remain open, even when they're not used. NDI requires UDP connections in the reverse.

https://docs.ndi.video/all/getting-started/white-paper/ndi-related-network-ports

Only IPV4 ports will be opened by default. IPV6 is potentially global.

If you're serious, you probably want interface-level firewall rules.";
      };

      interface = mkOption {
        type = lib.types.nullOr lib.types.str;
        default = null;
        description = ''
          Name of interface to add firewall rules to.

            Doesn't affect the interface(s) that NDI Discovery Services would bind to (they are not configured here).
        '';
      };

      allowedTCPPortRanges = lib.mkOption {
        type = lib.types.listOf (lib.types.attrsOf lib.types.port);
        default = [
          {
            from = 5961;
            to = 5965;
          }
        ];
        example = [
          {
            from = 5961;
            to = 5959;
          }
        ];
      };

      allowedUDPPortRanges = lib.mkOption {
        type = lib.types.listOf (lib.types.attrsOf lib.types.port);
        default = [
          {
            from = 5960;
            to = 5965;
          }
        ];
        example = [
          {
            from = 5960;
            to = 5959;
          }
        ];
      };
    };

    multi-stream-receiving = {
      enable = lib.mkOption {
        type = lib.types.bool;
        default = false;
        description = "Open TCP/UDP Ports for multi-TCP/UDP receiving.

Listed for documentation purposes. They should be disabled unless you need them! (you don't)

These ports will remain open, even when they're not used. NDI requires UDP connections in the reverse.

https://docs.ndi.video/all/getting-started/white-paper/ndi-related-network-ports

Only IPV4 ports will be opened by default. IPV6 is potentially global.

If you're serious, you probably want interface-level firewall rules.";
      };

      interface = mkOption {
        type = lib.types.nullOr lib.types.str;
        default = null;
        description = ''
          Name of interface to add firewall rules to.

            Doesn't affect the interface(s) that NDI Discovery Services would bind to (they are not configured here).
        '';
      };

      allowedTCPPortRanges = lib.mkOption {
        type = lib.types.listOf (lib.types.attrsOf lib.types.port);
        default = [
          {
            from = 6960;
            to = 6965;
          }
        ];
        example = [
          {
            from = 6960;
            to = 6959;
          }
        ];
      };

      allowedUDPPortRanges = lib.mkOption {
        type = lib.types.listOf (lib.types.attrsOf lib.types.port);
        default = [
          {
            from = 6960;
            to = 6965;
          }
        ];
        example = [
          {
            from = 6960;
            to = 6959;
          }
        ];
      };
    };

    multi-stream-sending = {
      enable = lib.mkOption {
        type = lib.types.bool;
        default = false;
        description = "Open TCP/UDP Ports for multi-TCP/UDP sending.

Listed for documentation purposes. They should be disabled unless you need them! (you don't)

These ports will remain open, even when they're not used. NDI requires UDP connections in the reverse.

https://docs.ndi.video/all/getting-started/white-paper/ndi-related-network-ports

Only IPV4 ports will be opened by default. IPV6 is potentially global.

If you're serious, you probably want interface-level firewall rules.";
      };

      allowedTCPPortRanges = lib.mkOption {
        type = lib.types.listOf (lib.types.attrsOf lib.types.port);
        default = [
          {
            from = 7960;
            to = 7965;
          }
        ];
        example = [
          {
            from = 7960;
            to = 7959;
          }
        ];
      };

      allowedUDPPortRanges = lib.mkOption {
        type = lib.types.listOf (lib.types.attrsOf lib.types.port);
        default = [
          {
            from = 7960;
            to = 7965;
          }
        ];
        example = [
          {
            from = 7960;
            to = 7959;
          }
        ];
      };
    };
  };

  # fdsa = "fdsa";

  config = lib.mkIf cfg.enable (
    lib.mkMerge [
      {
        programs.obs-studio = {
          plugins = [ cfg.distroav.package ];
        };
      }

      # discovery
      (lib.mkIf (cfg.discovery.enable && cfg.discovery.interface != null) {
        networking.firewall.interfaces."${cfg.discovery.interface}".allowedTCPPorts =
          cfg.discovery.discoveryTCP ++ cfg.discovery.queryTCP;
      })

      (lib.mkIf (cfg.discovery.enable && cfg.discovery.interface == null) {
        networking.firewall.allowedTCPPorts = cfg.discovery.discoveryTCP ++ cfg.discovery.queryTCP;
      })

      # ndi-stream
      (lib.mkIf (cfg.ndi-stream.enable && cfg.ndi-stream.interface != null) {
        networking.firewall.interfaces."${cfg.ndi-stream.interface}" = {
          allowedTCPPortRanges = cfg.ndi-stream.allowedTCPPortRanges;
          allowedUDPPortRanges = cfg.ndi-stream.allowedUDPPortRanges;
        };
      })

      (lib.mkIf (cfg.ndi-stream.enable && cfg.ndi-stream.interface == null) {
        networking.firewall.allowedTCPPortRanges = cfg.ndi-stream.allowedTCPPortRanges;
        networking.firewall.allowedUDPPortRanges = cfg.ndi-stream.allowedUDPPortRanges;
      })

      # multi-stream-receiving
      (lib.mkIf (cfg.multi-stream-receiving.enable && cfg.multi-stream-receiving.interface != null) {
        networking.firewall.interfaces."${cfg.multi-stream-receiving.interface}" = {
          allowedTCPPortRanges = cfg.multi-stream-receiving.allowedTCPPortRanges;
          allowedUDPPortRanges = cfg.multi-stream-receiving.allowedUDPPortRanges;
        };
      })

      (lib.mkIf (cfg.multi-stream-receiving.enable && cfg.multi-stream-receiving.interface == null) {
        networking.firewall.allowedTCPPortRanges = cfg.multi-stream-receiving.allowedTCPPortRanges;
        networking.firewall.allowedUDPPortRanges = cfg.multi-stream-receiving.allowedUDPPortRanges;
      })

      # multi-stream-sending
      (lib.mkIf (cfg.multi-stream-sending.enable && cfg.multi-stream-sending.interface != null) {
        networking.firewall.interfaces."${cfg.multi-stream-sending.interface}" = {
          allowedTCPPortRanges = cfg.multi-stream-sending.allowedTCPPortRanges;
          allowedUDPPortRanges = cfg.multi-stream-sending.allowedUDPPortRanges;
        };
      })

      (lib.mkIf (cfg.multi-stream-sending.enable && cfg.multi-stream-sending.interface == null) {
        networking.firewall.allowedTCPPortRanges = cfg.multi-stream-sending.allowedTCPPortRanges;
        networking.firewall.allowedUDPPortRanges = cfg.multi-stream-sending.allowedUDPPortRanges;
      })
    ]
  );
}

# ... i don't think using `config` here works. it also matches on identity...
#
# programs.obs-studio.plugins = lib.mkForce (
#   lib.subtractLists [ pkgs.obs-studio-plugins.obs-move-transition ] config.programs.obs-studio.plugins
# );
