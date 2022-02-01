;;* Module: hersai
(define-module (hersai)
  #:use-module (base-system)
  #:use-module (gnu services)
  #:use-module (gnu system)
  #:use-module (gnu system uuid)
  #:use-module (gnu system file-systems)
  #:use-module (gnu system mapped-devices)

  #:use-module (gnu packages firmware)
  #:use-module (nongnu packages linux))

;;** desktop-services
(define %hersai-desktop-services
  (modify-services
   %dc-desktop-services

   (guix-service-type config =>
                      (guix-configuration
                       (inherit config)
                       (substitute-urls
                        (append (list "https://substitutes.nonguix.org")
                                %default-substitute-urls))
                       (authorized-keys
                        (append (list (local-file "../nonguix.pub"))
                                %default-authorized-guix-keys))))

   ;; (fuse-desktop-type)
   ))

;;** operating-system
(operating-system
 (inherit base-operating-system)
 (host-name "hersai")

 ;; NOTE: has Broadcom BCM4360 wifi (broadcom-sta n/a)
 ;; TODO: add broadcom-bt-firmware
 (firmware (list linux-firmware
                 openfwwf-firmware))

 (mapped-devices
  (list (mapped-device
         (source (uuid "5d969658-9af4-48f0-b467-0ea6a4f82195"))
         (targets ("pde"))
         (type luks-device-mapping))

        (mapped-device
         (source "matrix")
         (targets (list "matrix-rootvol" "matrix-swapvol" "matrix-homevol"))
         (type lvm-device-mapping))))

 (file-systems (cons*

                (file-system
                 (device (file-system-label "root"))
                 (mount-point "/")
                 (type "ext4")
                 (needed-for-boot? #t)
                 (dependencies mapped-devices))

                (file-system
                 (device (file-system-label "home"))
                 (mount-point "/home")
                 (type "ext4")
                 (needed-for-boot? #f)
                 (dependencies mapped-devices))

                (file-system
                 (device (file-system-label "Data"))
                 (mount-point "/data")
                 (type "ext4")
                 (needed-for-boot? #f))

                ;; /boot/efi needs to be enumerated here
                ;;   in addition to the (bootloader...) declaration
                (file-system
                 (device "/dev/sda1")
                 (mount-point "/boot/efi")
                 (type "vfat"))
                %base-file-systems))

 (services %hersai-desktop-services)
 (swap-devices (list (file-system-label "swap"))))
