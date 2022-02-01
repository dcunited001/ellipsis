;; This is an operating system configuration generated
;; by the graphical installer.

(use-modules (gnu))
(use-service-modules
  cups
  desktop
  networking
  ssh
  xorg)

(operating-system
  (locale "en_US.utf8")
  (timezone "America/New_York")
  (keyboard-layout
    (keyboard-layout "us" "altgr-intl"))
  (host-name "stubby")
  (users (cons* (user-account
                  (name "dc")
                  (comment "dc")
                  (group "users")
                  (home-directory "/home/dc")
                  (supplementary-groups
                    '("wheel" "netdev" "audio" "video")))
                %base-user-accounts))
  (packages
    (append
      (list (specification->package "i3-wm")
            (specification->package "i3status")
            (specification->package "dmenu")
            (specification->package "st")
            (specification->package "nss-certs"))
      %base-packages))
  (services
    (append
      (list (service gnome-desktop-service-type)
            (service openssh-service-type)
            (service tor-service-type)
            (set-xorg-configuration
              (xorg-configuration
                (keyboard-layout keyboard-layout))))
      %desktop-services))
  (bootloader
    (bootloader-configuration
      (bootloader grub-efi-bootloader)
      (targets (list "/boot/efi"))
      (keyboard-layout keyboard-layout)))
  (swap-devices
    (list (uuid "8cf1a2d9-572d-404b-a8ae-f28997c16e8d")))
  (file-systems
    (cons* (file-system
             (mount-point "/data")
             (device
               (uuid "6b03e238-e432-4a13-9bc9-757cbd0b3fc2"
                     'ext4))
             (type "ext4"))
           (file-system
             (mount-point "/home")
             (device
               (uuid "e0a8f348-11fe-49a9-8ab5-152c8cafc400"
                     'ext4))
             (type "ext4"))
           (file-system
             (mount-point "/")
             (device
               (uuid "c0088a2f-3f1c-4f43-a4dd-cfcf638e7d55"
                     'btrfs))
             (type "btrfs"))
           (file-system
             (mount-point "/boot/efi")
             (device (uuid "5FE9-E6C7" 'fat32))
             (type "vfat"))
           %base-file-systems)))
