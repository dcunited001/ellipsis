{ ... }:
{
  services.locate.enable = true;
  services.locate = {
    prunePaths = [
      "/afs"
      "/gnu/store"
      "/net"
      "/media"
      "/mnt"
      "/sfs"
      "/tmp"
      "/udev"
      "/root"
      "/proc"
    ];
    pruneBindMounts = true;
    # others: /sys /run

    # defaults:
    # pruneNames = [ ".bzr" ".cache" ".git" ".hg" ".svn" ];
    # pruneFS = [ "looks good" ];
    # extraFlags = []; # arch doesn't pass anything else
    # prunePaths = [ "/tmp" "/var/tmp" "/var/cache" "/var/lock" "/var/run" "/var/spool"
    #   "/nix/store" "/nix/var/log/nix" ];
  };
}
