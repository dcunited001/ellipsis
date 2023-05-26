;;* Module: kharis-x11
(define-module (dc systems kharis-x11)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 format)

  #:use-module (gnu)
  #:use-module (gnu system)
  #:use-module (gnu system nss)
  #:use-module (gnu system setuid)

  #:use-module (nongnu packages linux)
  #:use-module (nongnu system linux-initrd)

  #:use-module (dc systems base)
  #:use-module (dc systems kharis)
  #:use-module (ellipsis home config))


(use-service-modules guix admin sysctl pm nix avahi dbus cups
                     desktop linux mcron networking xorg ssh
                     security-token docker audio virtualization)

(use-package-modules nfs certs shells ssh tls gnupg security-token
                     bash emacs emacs-xyz gnome networking libusb
                     fonts cups audio xorg xdisorg linux file-systems
                     version-control package-management freedesktop
                     cryptsetup hardware guile vim suckless)

(define %host-name "kharis")

;; system-specific users should go here
(define %dc-users '())

(define %kharis-users
  (append
   (list (dc-user
          (cons* "libvirt" "docker" %dc-my-groups)))
   %dc-users
   %base-user-accounts))

;; TODO: configure in wayland
;;
;; https://wayland.freedesktop.org/libinput/doc/1.10.7/faq.html#faq_config_options
(define %kharis-libinput-config
  "Section \"InputClass\"
  Identifier \"Touchpads\"
  Driver \"libinput\"
  MatchDevicePath \"/dev/input/event*\"
  MatchIsTouchpad \"on\"

  Option \"Tapping\" \"on\"
  Option \"TappingDrag\" \"on\"
  Option \"DisableWhileTyping\" \"on\"
  Option \"MiddleEmulation\" \"on\"
  Option \"ScrollMethod\" \"twofinger\"
EndSection

Section \"InputClass\"
  Identifier \"Keyboards\"
  Driver \"libinput\"
  MatchDevicePath \"/dev/input/event*\"
  MatchIsKeyboard \"on\"
EndSection
")

(define %kharis-xorg-configuration
  (xorg-configuration
   ;; seems to default to this
   ;; (drivers '("amdgpu" "vesa"))
   (keyboard-layout %kharis-shell-keyboard)
   (modules (append (list xf86-input-wacom)
                    %default-xorg-modules))
   (extra-config (list %kharis-libinput-config))))

(define system
  (operating-system
    (host-name %host-name)
    (timezone "America/New_York")
    (locale "en_US.UTF-8")

    (kernel linux)
    (firmware (cons* linux-firmware
                     amd-microcode
                     ;; realtek-firmware
                     %base-firmware))

    (kernel-loadable-modules
     ;; wacom module?
     (list v4l2loopback-linux-module))

    (keyboard-layout %kharis-shell-keyboard)
    (bootloader (bootloader-configuration
                 (bootloader grub-efi-bootloader)
                 (targets '("/boot/efi"))
                 (keyboard-layout keyboard-layout)))

    (groups %dc-base-groups)
    (users %kharis-users)

    (mapped-devices
     (list (mapped-device
            (source (uuid "c6684f7e-a5e2-4096-a7d0-a0970221c971"))
            (targets (list "pde"))
            (type luks-device-mapping))

           ;; TODO: change these label names (to work across systems)
           (mapped-device
            (source "matrix")
            (targets (list "matrix-root"
                           "matrix-swapvol"
                           "matrix-home"
                           "matrix-flatpak"
                           "matrix-data"))
            (type lvm-device-mapping))))
    (swap-devices (list (file-system-label "kharisSwap")))

    (file-systems
     (cons*
      (file-system
        (device (file-system-label "kharisRoot"))
        (mount-point "/")
        (type "btrfs")
        (flags '(no-atime))
        (options "space_cache=v2")
        (needed-for-boot? #t)
        (dependencies mapped-devices))

      (file-system
        (device (file-system-label "Home"))
        (mount-point "/home")
        (type "ext4")
        (needed-for-boot? #f)
        (dependencies mapped-devices))

      (file-system
        (device (file-system-label "Data"))
        (mount-point "/data")
        (type "ext4")
        (needed-for-boot? #f)
        (dependencies mapped-devices))

      (file-system
        (device (file-system-label "Flatpak"))
        (mount-point "/flatpak")
        (type "ext4")
        (needed-for-boot? #f)
        (dependencies mapped-devices))

      (file-system
        (device (file-system-label "Steam"))
        (mount-point "/flatpak/steam")
        (type "ext4")
        (needed-for-boot? #f))

      ;; /boot/efi needs to be enumerated here
      ;;   in addition to the (bootloader...) declaration
      (file-system
        ;; (device (uuid "B184-6A00" 'fat))
        ;; or: (device (file-system-label "KHARISEFI"))
        (device "/dev/nvme0n1p1")
        (mount-point "/boot/efi")
        (type "vfat"))
      %base-file-systems))

    (packages (append %dc-packages-xorg-only
                      %dc-desktop-packages
                      %base-packages))

    (services
     (append
      ;; (modify-services %base-services)
      ;; %base-services
      (modify-services %base-services
        (guix-service-type
         config => (guix-configuration
                    (inherit config)
                    (extra-options '("-c10")))))

      ;; from %desktop-services
      (list
       (service gdm-service-type
                (gdm-configuration
                 ;; (debug? #f)
                 ;; (x-session) ;; default: (xinitrc)
                 (xorg-configuration
                  %kharis-xorg-configuration)))
       (screen-locker-service slock)
       (screen-locker-service xlockmore "xlock")
       (simple-service 'mtp udev-service-type (list libmtp))
       (simple-service
        'mount-setuid-helpers
        setuid-program-service-type
        (map (lambda (program)
               (setuid-program
                (program program)))
             (list (file-append nfs-utils "/sbin/mount.nfs")
                   (file-append ntfs-3g "/sbin/mount.ntfs-3g"))))
       gdm-file-system-service
       (simple-service 'network-manager-applet
                       profile-service-type
                       (list network-manager-applet))
       (accountsservice-service)        ; missing in kharis.scm
       (service colord-service-type)
       x11-socket-directory-service

       ;; TODO: add pipewire
       ;; (service pulseaudio-service-type)
       ;; (service alsa-service-type)
       )

      (list
       %dc-nonguix-substitutes-service

       %dc-extra-file-env
       %dc-extra-file-ld-linux

       polkit-wheel-service

       %dc-ntp-service
       %dc-network-manager-service
       (service wpa-supplicant-service-type)
       (service modem-manager-service-type)
       (service bluetooth-service-type
                (bluetooth-configuration
                 (auto-enable? #t)))
       (service usb-modeswitch-service-type)

       (service thermald-service-type)
       %kharis-tlp-service
       %dc-ras-daemon-service
       %kharis-gpm-service

       (service avahi-service-type)
       (service udisks-service-type)
       (service upower-service-type)
       (service geoclue-service-type)
       (service polkit-service-type)
       (service elogind-service-type
                (elogind-configuration
                 (handle-lid-switch-external-power 'suspend)))
       (service dbus-root-service-type)

       fontconfig-file-system-service

       %dc-nntp-service
       %kharis-openssh-service

       (service pcscd-service-type)
       (udev-rules-service 'fido2 libfido2 #:groups '("plugdev"))
       (udev-rules-service 'u2f libu2f-host #:groups '("plugdev"))
       (udev-rules-service 'yubikey yubikey-personalization)

       %dc-docker-service
       %dc-libvirt-service
       %dc-virtlog-service

       (service sane-service-type)
       (service cups-pk-helper-service-type)
       %dc-cups-service

       %dc-pam-limits-service
       (udev-rules-service 'pipewire-add-udev-rules pipewire)
       (udev-rules-service 'brightnessctl-udev-rules brightnessctl)

       %dc-unattended-upgrade-service-type
       )))))

system
