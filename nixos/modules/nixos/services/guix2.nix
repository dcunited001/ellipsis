{
  config,
  pkgs,
  lib,
  ...
}:

let
  cfg = config.services.guix2;

  # package = cfg.package.override { inherit (cfg) stateDir storeDir; };

  rootGuix = "/var/guix/profiles/per-user/root/current-guix/bin/guix";
  rootGuixDaemon = "/var/guix/profiles/per-user/root/current-guix/bin/guix-daemon";

  guixBuildUser = id: {
    name = "guixbuilder${toString id}";
    group = cfg.group;
    extraGroups = [ cfg.group ];
    createHome = false;
    description = "Guix build user ${toString id}";
    isSystemUser = true;
  };

  guixBuildUsers =
    numberOfUsers:
    builtins.listToAttrs (
      map (user: {
        name = user.name;
        value = user;
      }) (builtins.genList guixBuildUser numberOfUsers)
    );

  guixUserProfiles = {
    "current-guix" = "\${XDG_CONFIG_HOME}/guix/current";
    "guix-home" = "$HOME/.guix-home/profile";
    "guix-profile" = "$HOME/.guix-profile";
  };

  guixProfiles = lib.attrValues guixUserProfiles;

  serviceEnv = {
    GUIX_LOCPATH = "${cfg.stateDir}/guix/profiles/per-user/root/guix-profile/lib/locale";
    LC_ALL = "C.UTF-8";
  };
in
{
  options.services.guix2 = with lib; {
    enable = mkEnableOption "Guix build daemon service";

    group = mkOption {
      type = types.str;
      default = "guixbuild";
      example = "guixbuild";
      description = ''
        The group of the Guix build user pool.
      '';
    };

    nrBuildUsers = mkOption {
      type = types.ints.unsigned;
      description = ''
        Number of Guix build users to be used in the build pool.
      '';
      default = 10;
      example = 20;
    };

    extraArgs = mkOption {
      type = with types; listOf str;
      default = [ ];
      example = [
        "--max-jobs=4"
        "--debug"
      ];
      description = ''
        Extra flags to pass to the Guix daemon service.
      '';
    };

    substituters = {
      urls = lib.mkOption {
        type = with lib.types; listOf str;
        default = [
          "https://ci.guix.gnu.org"
          "https://bordeaux.guix.gnu.org"
          "https://berlin.guix.gnu.org"
          "https://substitutes.nonguix.org"
        ];
        example = lib.literalExpression ''
          options.services.guix.substituters.urls.default ++ [
            "https://guix.example.com"
            "https://guix.example.org"
          ]
        '';
        description = ''
          A list of substitute servers' URLs for the Guix daemon to download
          substitutes from.
        '';
      };
    };

    storeDir = mkOption {
      type = types.path;
      default = "/gnu/store";
      description = ''
        The store directory where the Guix service will serve to/from. Take
        note Guix cannot take advantage of substitutes if you set it something
        other than {file}`/gnu/store` since most of the cached builds are
        assumed to be in there.

        ::: {.warning}
        This will also recompile all packages because the normal cache no
        longer applies.
        :::
      '';
    };

    stateDir = mkOption {
      type = types.path;
      default = "/var";
      description = ''
        The state directory where Guix service will store its data such as its
        user-specific profiles, cache, and state files.

        ::: {.warning}
        Changing it to something other than the default will rebuild the
        package.
        :::
      '';
      example = "/gnu/var";
    };

    gc = {
      enable = mkEnableOption "automatic garbage collection service for Guix";

      extraArgs = mkOption {
        type = with types; listOf str;
        default = [ ];
        description = ''
          List of arguments to be passed to {command}`guix gc`.

          When given no option, it will try to collect all garbage which is
          often inconvenient so it is recommended to set [some
          options](https://guix.gnu.org/en/manual/en/html_node/Invoking-guix-gc.html).
        '';
        example = [
          "--delete-generations=1m"
          "--free-space=10G"
          "--optimize"
        ];
      };

      dates = lib.mkOption {
        type = types.str;
        default = "03:15";
        example = "weekly";
        description = ''
          How often the garbage collection occurs. This takes the time format
          from {manpage}`systemd.time(7)`.
        '';
      };
    };
  };

  config = lib.mkIf cfg.enable (
    lib.mkMerge [
      {

        users.users = guixBuildUsers cfg.nrBuildUsers;
        users.groups.${cfg.group} = {
          gid = 989;
        };

        # Guix uses Avahi (through guile-avahi) both for the auto-discovering and
        # advertising substitute servers in the local network.
        services.avahi.enable = lib.mkDefault true;
        services.avahi.publish.enable = lib.mkDefault true;
        services.avahi.publish.userServices = lib.mkDefault true;

        # It's similar to Nix daemon so there's no question whether or not this
        # should be sandboxed.
        systemd.services.guix-daemon = {
          environment = serviceEnv // config.networking.proxy.envVars;
          script = ''
            exec ${rootGuixDaemon} \
              --build-users-group=${cfg.group} \
              ${
                lib.optionalString (
                  cfg.substituters.urls != [ ]
                ) "--substitute-urls='${lib.concatStringsSep " " cfg.substituters.urls}'"
              } \
              ${lib.escapeShellArgs cfg.extraArgs}
          '';
          serviceConfig = {
            OOMPolicy = "continue";
            RemainAfterExit = "yes";
            Restart = "always";
            TasksMax = 8192;
          };
          unitConfig.RequiresMountsFor = [
            cfg.storeDir
            cfg.stateDir
          ];
          wantedBy = [ "multi-user.target" ];
        };

        systemd.sockets.guix-daemon = {
          description = "Guix daemon socket";
          before = [ "multi-user.target" ];
          listenStreams = [ "${cfg.stateDir}/guix/daemon-socket/socket" ];
          unitConfig.RequiresMountsFor = [
            cfg.storeDir
            cfg.stateDir
          ];
          wantedBy = [ "sockets.target" ];
        };

        systemd.mounts = [
          {
            description = "Guix read-only store directory";
            before = [ "guix-daemon.service" ];
            what = cfg.storeDir;
            where = cfg.storeDir;
            type = "none";
            options = "bind,ro";

            unitConfig.DefaultDependencies = false;
            wantedBy = [ "guix-daemon.service" ];
          }
        ];

        # Make transferring files from one store to another easier with the usual
        # case being of most substitutes from the official Guix CI instance.
        environment.etc."guix/acl".text = (builtins.readFile ./guix2.acl);

        system.userActivationScripts.guix-activate-user-profiles.text =
          let
            guixProfile = profile: "${cfg.stateDir}/guix/profiles/per-user/\${USER}/${profile}";
            linkProfile =
              profile: location:
              let
                userProfile = guixProfile profile;
              in
              ''
                [ -d "${userProfile}" ] && ln -sfn "${userProfile}" "${location}"
              '';
            linkProfileToPath =
              acc: profile: location:
              acc + (linkProfile profile location);

            # This should contain export-only Guix user profiles. The rest of it is
            # handled manually in the activation script.
            guixUserProfiles' = lib.attrsets.removeAttrs guixUserProfiles [ "guix-home" ];

            linkExportsScript = lib.foldlAttrs linkProfileToPath "" guixUserProfiles';
          in
          ''
            # Don't export this please! It is only expected to be used for this
            # activation script and nothing else.
            XDG_CONFIG_HOME=''${XDG_CONFIG_HOME:-$HOME/.config}

            # Linking the usual Guix profiles into the home directory.
            ${linkExportsScript}

            # Activate all of the default Guix non-exports profiles manually.
            ${linkProfile "guix-home" "$HOME/.guix-home"}
            [ -L "$HOME/.guix-home" ] && "$HOME/.guix-home/activate"
          '';

        environment.sessionVariables.GUIX_LOCPATH = lib.makeSearchPath "lib/locale" guixProfiles;
        environment.profiles = lib.mkBefore guixProfiles;
      }

      (lib.mkIf cfg.gc.enable {
        # This service should be handled by root to collect all garbage by all
        # users.
        systemd.services.guix-gc = {
          description = "Guix garbage collection";
          startAt = cfg.gc.dates;
          script = ''
            exec ${rootGuix} gc ${lib.escapeShellArgs cfg.gc.extraArgs}
          '';
          serviceConfig = {
            Type = "oneshot";
            PrivateDevices = true;
            PrivateNetwork = true;
            ProtectControlGroups = true;
            ProtectHostname = true;
            ProtectKernelTunables = true;
            SystemCallFilter = [
              "@default"
              "@file-system"
              "@basic-io"
              "@system-service"
            ];
          };
        };

        systemd.timers.guix-gc.timerConfig.Persistent = true;
      })
    ]
  );
}
