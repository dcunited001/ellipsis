# USAGE in your configuration.nix.
# Update devices to match your hardware.
# {
#   imports = [ ./disko-config.nix ];
#   disko.devices.disk.root.device = "/dev/sda";
#   disko.devices.disk.data1.device = "/dev/sdb";
#   disko.devices.disk.data2.device = "/dev/sdc";
# }

{ lib, ... }:
let
  diskoHostname = "helius";
  diskoDisk = "/dev/nvme0n1";
in {
  disko.devices = {
    disk.main = {
      type = "disk";
      content = {
        type = "gpt";
        partitions = {
          esp = {
            priority = 1;
            name = "${diskoHostname}ESP";
            size = "512M";
            type = "EF00";
            content = {
              type = "filesystem";
              format = "vfat";
              mountpoint = "/boot";
              mountOptions = [ "umask=0077" "fmask=0022" "dmask=0022" ];
            };
          };

          root = {
            # Subvolumes must set a mountpoint in order to be mounted,
            # unless their parent is mounted
            content = {
              type = "btrfs";
              extraArgs = [ "-f" ]; # force overwrite
              subvolumes = {
                "@root" = {
                  mountpoint = "/";
                  mountOptions = [ "compress=zstd" "noatime" ];
                };
                "@persist" = {
                  mountpoint = "${config.hostSpec.persistFolder}";
                  mountOptions = [ "compress=zstd" "noatime" ];
                };
                "@nix" = {
                  mountpoint = "/nix";
                  mountOptions = [ "compress=zstd" "noatime" ];
                };
              };
            };
          };
        };
      };

      # this is luks-on-LVM in case I want to separate out LVs as devices
      # (btrfs kinda renders my whole partition-based container setup
      # pointless)
      #
      # for now, I'mma see how disko plays out, depending on
      #
      # - incremental updates, adding brtfs subvolumes
      # - feasibility of extracting a container's volume/data footprint
      #   (whether inside of the native application(s) logic or as OCI volumes
      #   or as partitions)
      # - I've already hit the "oh shit my LVM metadata" button

      lvm_vg = {
        pool = {
          type = "lvm_vg";
          lvs = {
            swap = {
              name = "${diskoHostname}Swap";
              size = "4GB";
              resumeDevice = false;
            };
            root = {
              size = "180GB";
              type = "btrfs";
              content = {
                # pass to mkfs.btrfs
                extraArgs = [ "-f" ];
                # subvolumes created if `btrfs subvolume show` lacks name
                # subvolumes are mounted at ./${subvol.name}
                # - directly under subvolid=5
                subvolumes = {
                  "/root" = {
                    mountpoint = "/";
                    mountOptions = [ "compress=zstd" "noatime" ];
                  };
                  "/root/var" = {
                    mountpoint = "/var";
                    mountOptions = [ "compress=zstd" "noatime" ];
                  };
                  "/root/var/cache" = {
                    mountpoint = "/var/cache";
                    mountOptions = [ "compress=zstd" "noatime" ];
                  };
                  "/root/var/tmp" = {
                    mountpoint = "/var/tmp";
                    mountOptions = [ "compress=zstd" "noatime" ];
                  };
                  "/root/var/log" = {
                    mountpoint = "/var/log";
                    mountOptions = [ "compress=zstd" "noatime" ];
                  };

                  "/home" = {
                    mountpoint = "/home";
                    mountOptions = [ "compress=zstd" "noatime" ];
                  };
                  "/home/theseus" = {
                    mountOptions = [ "compress=zstd" "noatime" ];
                  };

                  # separate subvolume root
                  #
                  # - /nix/store perf req. & file content are distinct
                  # - no compression: faster builds, faster reads
                  # - /nix/store is like overlayfs but the layers have been
                  #   shattered into millions of tiny pieces. garbage collect
                  #   will dedupe with less reads/writes
                  "/nix" = {
                    mountpoint = "/nix";
                    mountOptions = [ "noatime" ];
                  };

                  # rootful graphroot. the remaining mountpoints are inferred
                  #
                  # - for now, only images are stored in non-standard locations
                  # - btrfs driver will not dedupe images downloaded by separate users
                  #   - neither would overlay. both are CoW, but don't know whether
                  #     file content is identical
                  # - btrfs has a better shot at dedupe though. the subvolume
                  #   boundary will prevent this except for images between
                  #   multiple users.
                  #   - nevermind, podman manages image/storage data /as/ subvolumes
                  #     that inherit the mount options of their parent
                  #   - this introduces subvolume boundaries.
                  #
                  # - the podman/docker prune operation should dedupe through
                  #   removal (these shouldn't be created), but some btrfs(fs)
                  #   for images should be able to catch large repeated blocks
                  #   of bytes. maybe only ZFS can do this (it's retroactive
                  #   CoW: written-butitsa-copy)
                  #   - that's also mostly useless except for maybe a server used
                  #     for a lab where many images are frequently downloaded. or
                  #     for an image repository
                  "/pod" = { mountpoint = "/pod"; };

                  # rootful image_storage:
                  #
                  # imagestore must match /etc/container/containers.conf
                  "/pod/root/image" = {
                    # mountpoint = "/pod/root/image";
                    mountOptions = [ "compress=zstd" "noatime" ];
                  };

                  # this must be configured in the user's $XDG_CONFIG_HOME/containers/storage.conf
                  # graphroot = "/pod/rootless/graph/$USER" # if images and storage need an alternate path
                  # imagestore = "/pod/rootless/image/$USER" # if only images need an alternate path
                  #
                  # the driver should be BTRFS! (otherwise overlay; also it has more specific options)
                  "/pod/rootless/image" = {
                    # mountpoint = "/pod/rootless/image";
                    mountOptions = [ "compress=zstd" "noatime" ];
                  };

                  # rootless_storage_graph = "/pod/graph/$USER" # system-wide (useful for NFS/ephemeral)
                  #
                  # this requires ensuring umask/perm! (somewhat handled by podman)
                  #
                  # "/pod/rootless/graph" = {
                  #   mountpoint = "/pod/rootless/graph";
                  #   mountOptions = [ "compress=zstd" "noatime" ];
                  # };
                };
              };
            };

            # if backup were image-based...
            #
            # rootless = {
            #   size = "40GB";
            #   type = "btrfs";
            #   content = {
            #
            #     "/home/nautobot" = {
            #       mountpoint = "/home/nautobot";
            #       mountOptions = [ "compress=zstd" "noatime" ];
            #     };
            #
            #   # ... then ummm. this still wouldn't work (SMH)
            #
            #     "/home/app2" = {
            #       mountpoint = "/home/app2";
            #       mountOptions = [ "compress=zstd" "noatime" ];
            #     };
            #   };
            # };
          };
        };
      };
    };
  };
}

# boot = { name = "boot"; size = "1M"; type = "EF02"; };
# esp = {
#   name = "ESP"; size = "500M"; type = "EF00";
#   content = { type = "filesystem"; format = "vfat"; mountpoint = "/boot"; };
# };

# root = {
#   size = "20G";
#   name = "${diskoHostname}Root";
#   content = {
#     type = "filesystem";
#     format = "ext4";
#     mountpoint = "/";
#     mountOptions = [ "defaults" ];
#   };
# };
#
# root = {
#   name = "root";
#   size = "100%";
#   content = {
#     type = "lvm_pv";
#     vg = "pool";
#   };
# };

# content = {
#   type = "btrfs";
#   extraArgs = [ "-f" ];
#   subvolumes = {
#     "/root" = {
#       mountpoint = "/";
#       mountOptions = [ "compress=zstd" "noatime" ];
#     };
#     "/home" = {
#       mountpoint = "/home";
#       mountOptions = [ "compress=zstd" "noatime" ];
#     };
#     "/nix" = {
#       mountpoint = "/nix";
#       mountOptions = [ "compress=zstd" "noatime" ];
#     };
#     "/swap" = {
#       mountpoint = "/.swapvol";
#       swap.swapfile.size = "20M";
#     };
#   };
# };
