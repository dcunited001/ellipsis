;;* Module: kharis
(define-module (kharis)
  #:use-module (base-system)
  #:use-module (guix gexp)
  #:use-module (guix channels)

  #:use-module (gnu)
  #:use-module (gnu services)
  #:use-module (gnu services base)
  #:use-module (gnu services linux)  
  #:use-module (gnu services pm)             ;; for thermald
  #:use-module (gnu services virtualization) ;; for libvirt
  #:use-module (gnu services audio)
  #:use-module (gnu services cups)
  #:use-module (gnu services docker)
  ;;#:use-module (gnu services authentication)
  #:use-module (gnu services security-token)

  #:use-module (gnu system)
  #:use-module (gnu system uuid)
  #:use-module (gnu system file-systems)
  #:use-module (gnu system mapped-devices)

  #:use-module (gnu packages base)
  #:use-module (gnu packages firmware)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages cups)

  ;;NONFREE
  #:use-module (nongnu packages linux)

  #:use-module (guix transformations))

(use-service-modules desktop xorg networking ssh admin)
(use-package-modules certs shells linux)
(use-package-modules security-token)

;;** system info
;; this will fail on guix deploy
(define %host-name (gethostname))

;;** keyboard
(define-public %kharis-default-shell-keyboard
  (keyboard-layout "us" "altgr-intl"
                   #:model "pc105"
		   ;; see gitlab.freedesktop.org/xkeyboard-config/xkeyboard-config/-/issue/344
                   #:options '("caps:ctrl_modifier"
                               ;; "ctrl:swapcaps_hyper" ; in 1.3.0 (hyper as Mod3)
                               ;; "ctrl:hyper_capscontrol" ; in 1.5.0 (hyper as Mod4)
                               "lv3:ralt_alt"
                               "lv3:menu_switch")))

;;** desktop-packages
(define %kharis-desktop-packages
  (append (list
           ;; Guix qemu builds in virglrenderer support
           ;; virglrenderer

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
                        ;; seems to default to this
                        ;; (drivers '("amdgpu" "vesa"))
                        (keyboard-layout %kharis-default-shell-keyboard)
                        (modules (append (list xf86-input-wacom)
                                         %default-xorg-modules))
                        (extra-config (list %xorg-libinput-config))))
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

   (keyboard-layout %kharis-default-shell-keyboard)

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
                        (sound-power-save-on-bat 0)
                        (nmi-watchdog? #t)
                        (cpu-scaling-min-freq-on-bat 1700000)
                        (cpu-scaling-max-freq-on-bat 2100000)
                        (cpu-scaling-min-freq-on-ac 2100000)
                        (cpu-scaling-max-freq-on-ac 2100000)
                        (wifi-pwr-on-bat? #t)))

              (extra-special-file "/usr/bin/env"
                                  (file-append coreutils "/bin/env"))
              (extra-special-file "/lib64/ld-linux-x86-64.so.2"
                                  (file-append glibc "/lib/ld-linux-x86-64.so.2"))

              ;; if using (local-file "." "subdir" ...)
              ;; this would pull the entire "." into the store

              ;; (extra-special-file "/etc/flatpak/installations.d/steam.conf"
              ;;                     (local-file "flatpak/steam.conf"))
              ;; (extra-special-file "/etc/flatpak/installations.d/agenda.conf"
              ;;                     (local-file "flatpak/agenda.conf"))

              (extra-special-file
               "/etc/flatpak/installations.d"
               (file-union "installations.d"
                           `(("steam.conf" ,(local-file "flatpak/steam.conf"))
                             ("agenda.conf" ,(local-file "flatpak/agenda.conf")))))

              (service thermald-service-type)

              ;; TODO validate that disabling user-proxy doesn't cause issues
              ;; TODO inherit from service and add --data-root
              ;;
              ;; see %docker-activation in service definition
              ;; can't package as service and an alternate image storage here
              ;; but could
              (service docker-service-type
                       (docker-configuration
                        (enable-proxy? #f)))

              (service libvirt-service-type ;; TODO how is libvirt configured?
                       (libvirt-configuration
                        (unix-sock-group "libvirt")
                        (tls-port "16555")))

              (service virtlog-service-type
                       (virtlog-configuration
                        ;; (max-clients 1024) ;; default
                        (max-size (* 32 (expt 1024 2)))))

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
                        ;; TODO is this running?
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

              (service gpm-service-type
                       (gpm-configuration
                        ;; defaults, should work for IBM trackpoints
                        (options '("-m" "/dev/input/mice" "-t" "ps2"))))

              ;; TODO try to fix libfuse update
              (udev-rules-service 'fido2 libfido2 #:groups '("plugdev"))
              (udev-rules-service 'u2f libu2f-host #:groups '("plugdev"))
              (udev-rules-service 'pipewire-add-udev-rules pipewire)
              (udev-rules-service 'backlight-rule %udev-backlight-rule)

              ;; this only tags the yubikey device with security-token in udev
              (udev-rules-service 'yubikey yubikey-personalization)

              (bluetooth-service #:auto-enable? #t)

              (simple-service 'nntp-config etc-service-type
                              (list `("nntpserver"
                                      ,%dc-nntpserver)))
                              
              ;; TODO add these to the other systems
              ;; 
              ;; rasdaemon-service/type, rasdaemon-configuration
              ;; - helps anticipate hardware failures by scanning events, analysis appended to syslog
              ;; - with record? t, also structure logs as sqlite database: /var/lib/rasdaemon/ras-mc_event.db
              (service rasdaemon-service-type
                       (rasdaemon-configuration (record? #t)))

              ;; TODO configure ZRAM (ensure swap/zwap are off, assess performance conseq, pick a compression alg)
              ;; TODO reintegrate the swap-sapce back into root fs

              ;; TODO (service early-oom-service-type
              ;;               (earlyoom-configuration ...)

              ;; (remove-pulseaudio-service %kharis-desktop-services-slim)
              (remove-pulseaudio-service %kharis-desktop-services)
              ))

   (groups (append (list (user-group (name "julia") (system? #t))
                         (user-group (name "docker") (system? #t)))
                   %dc-groups))

    
   (users (append (list (dc-user (cons* "libvirt"
                                        "docker"
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

