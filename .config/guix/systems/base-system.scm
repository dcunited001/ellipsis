;;* Module: base-system
(define-module (base-system)
  #:use-module (gnu)
  #:use-module (srfi srfi-1)
  #:use-module (gnu system nss)
  #:use-module (gnu services pm)
  #:use-module (gnu services cups)
  #:use-module (gnu services desktop)
  #:use-module (gnu services docker)
  #:use-module (gnu services networking)
  #:use-module (gnu services virtualization)
  #:use-module (gnu packages wm)
  #:use-module (gnu packages cups)
  #:use-module (gnu packages vim)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages emacs)
  #:use-module (gnu packages file-systems)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages mtools)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages audio)
  #:use-module (gnu packages gnuzilla)
  #:use-module (gnu packages pulseaudio)
  #:use-module (gnu packages web-browsers)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages package-management)
  #:use-module (nongnu packages linux)
  #:use-module (nongnu system linux-initrd))


;;** use-service-modules nix, desktop, xorg
(use-service-modules nix)
(use-service-modules desktop xorg) ;sway/wayland?

;;** use-package-modules
(use-package-modules certs)
(use-package-modules shells)

;;** backlight-udev-rule
;; Add udev rule that allows members of the "video" group to change brightness.
(define %backlight-udev-rule
  (udev-rule
   "90-backlight.rules"
   (string-append "ACTION==\"add\", SUBSYSTEM==\"backlight\", "
                  "RUN+=\"/run/current-system/profile/bin/chgrp video /sys/class/backlight/%k/brightness\""
                  "\n"
                  "ACTION==\"add\", SUBSYSTEM==\"backlight\", "
                  "RUN+=\"/run/current-system/profile/bin/chmod g+w /sys/class/backlight/%k/brightness\"")))

;;** %dc-desktop-services
;;
;; Override the default %desktop-services to add the udev backlight config
;; and include OpenVPN in the list of NetworkManager plugins.
(define %dc-desktop-services
  (modify-services
   %desktop-services

   (elogind-service-type
    config =>
    (elogind-configuration
     (inherit config)
     (handle-lid-switch-external-power 'suspend)))

   (udev-service-type
    config =>
    (udev-configuration
     (inherit config)
     (rules (cons %backlight-udev-rule
                  (udev-configuration-rules config)))))

   (network-manager-service-type
    config =>
    (network-manager-configuration
     (inherit config)
     (vpn-plugins (list network-manager-openvpn))))))



;;** %xorg-libinput-config

;; Use the =libinput-driver= for all input device
;;
;; Define the =base-operating-system=
;; which will be inherited by all machine configurations.
;; TODO: update this for wacom device
;; TODO: allow %xorg-libinput-config to be extended

(define %xorg-libinput-config
  "
Section \"InputClass\"
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

;;** base-operating-system
(define-public base-operating-system
  (operating-system
   (host-name "eerse")
   (timezone "America/New_York")
   (locale "en_US.utf8")

   (kernel linux) ;use the non-free Linux kernel and firmware
   (firmware (list linux-firmware))
   (initrd microcode-initrd)

   (keyboard-layout (keyboard-layout "us" "altgr-intl"
                                     #:model "pc105"))

   (bootloader (bootloader-configuration
                (bootloader grub-efi-bootloader)
                (targets (list "/boot/efi"))
                (keyboard-layout keyboard-layout)))

    ;; Guix doesn't like it when there isn't a file-systems
    ;; entry, so add one that is meant to be overridden
    (file-systems (cons*
                   (file-system
                    (mount-point "/tmp")
                    (device "none")
                    (type "tmpfs")
                    (check? #f))
                   %base-file-systems))

    (users (cons (user-account
                  (name "dc")
                  (comment "David Conner")
                  (group "users")
                  (home-directory "/home/dc")
                  (supplementary-groups '(
                                          "wheel"      ;; sudo
                                          "netdev"     ;; network devices
                                          "kvm"
                                          "tty"
                                          "input"
                                          "docker"
                                          "realtime"   ;; Enable RT scheduling
                                          "lp"         ;; control bluetooth
                                          "audio"      ;; control audio
                                          "video"      ;; control video
                                          )))

                 %base-user-accounts))

    (groups (cons (user-group (system? #t) (name "realtime"))
                  %base-groups))

    ;; install bare-minimum system packages
    (packages (append (list
                       openssh
                       git
                       ntfs-3g
                       exfat-utils
                       fuse-exfat
                       stow
                       vim
                       emacs
                       xterm
                       bluez
                       bluez-alsa
                       pipewire ;; TODO: pipewire?
                       tlp
                       xf86-input-libinput
                       nss-certs
                       gvfs)
                      %base-packages))

    (services (cons* (service slim-service-type

                              ;; TODO: customize slim
                              ;; - %default-slim-theme
                              ;; - %default-slim-theme-name
                              (slim-configuration
                               (xorg-configuration
                                (xorg-configuration
                                 (keyboard-layout keyboard-layout)
                                 (extra-config (list %xorg-libinput-config))))
                               (default-user "dc")
                               ))

                     (service tlp-service-type
                              (tlp-configuration
                               (cpu-boost-on-ac? #t)
                               (wifi-pwr-on-bat? #t)))
                     (pam-limits-service ;; This enables JACK to enter realtime mode
                                            (list
                                             (pam-limits-entry "@realtime" 'both 'rtprio 99)
                                             (pam-limits-entry "@realtime" 'both 'memlock 'unlimited)))
                     (extra-special-file "/usr/bin/env"
                                         (file-append coreutils "/bin/env"))
                     (service thermald-service-type)
                     (service docker-service-type)
                     (service libvirt-service-type ;; TODO how is libvirt configured?
                              (libvirt-configuration
                               (unix-sock-group "libvirt")
                               (tls-port "16555")))
                     (service cups-service-type
                              (cups-configuration
                               (web-interface? #t)
                               (extensions
                                (list cups-filters))))
                     (service nix-service-type)
                     (bluetooth-service #:auto-enable? #t)
                     (remove (lambda (service)
                               (eq? (service-kind service)  gdm-service-type))
                             %dc-desktop-services)))

    ;; allow resolution of '.local' hostnames with mDNS
    (name-service-switch %mdns-host-lookup-nss)))