;;* Module: kharis
(define-module (kharis)
  #:use-module (base-system)
  #:use-module (guix gexp)
  #:use-module (guix channels)

  #:use-module (gnu)
  #:use-module (gnu services)
  #:use-module (gnu services base)
  #:use-module (gnu services pm)             ;; for thermald
  #:use-module (gnu services virtualization) ;; for libvirt
  #:use-module (gnu services audio)
  #:use-module (gnu services cups)
  ;;#:use-module (gnu services authentication)
  #:use-module (gnu services security-token)

  #:use-module (gnu system)
  #:use-module (gnu system uuid)
  #:use-module (gnu system file-systems)
  #:use-module (gnu system mapped-devices)

  #:use-module (gnu packages firmware)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages cups)

  ;;NONFREE
  #:use-module (nongnu packages linux))

(use-service-modules desktop xorg networking ssh admin)
(use-package-modules certs shells linux)

;;** system info
;; this will fail on guix deploy
(define %host-name (gethostname))

;;** keyboard
(define-public %kharis-default-shell-keyboard
  (keyboard-layout "us" "altgr-intl"
                   #:model "pc105"
                   #:options '("caps:hyper"
                               "lv3:ralt_alt"
                               "lv3:menu_switch")))

;;** desktop-packages
(define %kharis-desktop-packages
  (append (list
           xf86-video-amdgpu)
          %dc-desktop-packages))

;;** desktop-services
;; TODO: docker is no longer added as a service by default
(define %kharis-desktop-services
  (modify-services
   dc-desktop-services

   (gdm-service-type config =>
                     (gdm-configuration
                      (inherit config)
                      (xorg-configuration
                       (xorg-configuration
                        (keyboard-layout %kharis-default-shell-keyboard)
                        (modules (append (list xf86-input-wacom)
                                         %default-xorg-modules))
                        (extra-config (list %xorg-libinput-config)))
                       )
                      (default-user "dc")))

   (guix-service-type config =>
                      (guix-configuration
                       (inherit config)
                       (extra-options '("-c10"))
                       (substitute-urls
                        (append (list "https://substitutes.nonguix.org")
                                %default-substitute-urls))
                       (authorized-keys
                        (append (list (local-file "/etc/guix/nonguix.pub"))
                                %default-authorized-guix-keys))))

   ;; (udev-service-type config =>
   ;;                    (udev-configuration
   ;;                     (inherit config)
   ;;                     (rules (cons %ifuse-udev-rule
   ;;                                  (udev-configuration-rules config)))))

   ))

;; TODO %kharis-modprobe-blacklist

(define %kharis-desktop-services-slim
  (modify-services

   ;; guix is pretty stubborn about lazily adding GDM back in
   (remove-gdm-service
    (cons*
     (service slim-service-type

              ;; TODO: customize slim
              ;; - %default-slim-theme
              ;; - %default-slim-theme-name
              (slim-configuration
               (xorg-configuration
                (xorg-configuration
                 (keyboard-layout %kharis-default-shell-keyboard)
                 (extra-config (list %xorg-libinput-config))))
               (default-user "dc")))
     dc-desktop-services))

   ;; and here we modify some services
   (guix-service-type config =>
                      (guix-configuration
                       (inherit config)
                       (extra-options '("-c10"))
                       (substitute-urls
                        (append (list "https://substitutes.nonguix.org")
                                %default-substitute-urls))
                       (authorized-keys
                        (append (list (local-file "/etc/guix/nonguix.pub"))
                                %default-authorized-guix-keys))))
   ))

;;** operating-system
(define system
  (operating-system
   (inherit base-operating-system)
   (host-name %host-name)

   (kernel linux)
   
   ;; TODO: add broadcom-bt-firmware
   (firmware (cons* linux-firmware
                    amd-microcode
                    ;; realtek-firmware
                    %base-firmware))

   ;; (kernel-loadable-modules (wacom))

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

   (packages %kharis-desktop-packages)

   ;; TODO dc-desktop-services is dropping services appended in base-system.scm
   ;; - fix this, then refactor
   (services (cons*

              (service tlp-service-type
                       (tlp-configuration
                        (cpu-boost-on-ac? #t)
                        (tlp-default-mode "AC") ;; this is the default
                        (wifi-pwr-on-bat? #t)))

              (extra-special-file "/usr/bin/env"
                                  (file-append coreutils "/bin/env"))

              (service thermald-service-type)

              (service libvirt-service-type ;; TODO how is libvirt configured?
                       (libvirt-configuration
                        (unix-sock-group "libvirt")
                        (tls-port "16555")))

              (service pcscd-service-type)
              (service openssh-service-type
                       (openssh-configuration
                        (port-number (string->number
                                      (or (getenv "_OPENSSH_PORT") "22")))
                        (password-authentication? #f)
                        (allow-agent-forwarding? #f)
                        (allow-tcp-forwarding? #t)
                        (accepted-environment '("COLORTERM"))
                        (authorized-keys
                         `(("dc" ,(local-file ".ssh/dc.pub"))))))

              (service unattended-upgrade-service-type
                       (unattended-upgrade-configuration
                        (schedule "30 2 * * 0")
                        (channels #~(list
                                     (channel
                                      (name 'nonguix)
                                      (url "https://gitlab.com/nonguix/nonguix")
                                      (branch "master"))
                                     %default-channels))
                        (system-expiration (* 6 7 24 3600))
                        (operating-system-file
                         (file-append
                          (local-file "." "systems-dir" #:recursive? #t)
                          (string-append
                           "/root/.config/guix/systems/" %host-name ".scm")))))

              (service cups-service-type
                       (cups-configuration
                        (web-interface? #t)
                        ;; TODO ssl-options? TLS 1.0+
                        (extensions
                         (list cups-filters epson-inkjet-printer-escpr hplip-minimal))))

              (udev-rules-service 'pipewire-add-udev-rules pipewire)

              (bluetooth-service #:auto-enable? #t)

              %kharis-desktop-services))

   (groups (append (list (user-group (name "julia") (system? #t)))
                   %dc-groups))

    
   (users (append (list (dc-user (cons* "libvirt"
                                        %dc-my-groups))
                        (user-account
                         (name "julia")
                         (group "julia")
                         (system? #t)
                         (comment "Julia User")
                         (home-directory "/home/jovyan")
                         (shell (file-append shadow "/sbin/nologin"))))
                  %dc-users
                  %base-user-accounts))

   (file-systems (cons*

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

   ;; TODO: change to (list (swap-space (target ... )))
   (swap-devices (list (file-system-label "kharisSwap")))))

system

