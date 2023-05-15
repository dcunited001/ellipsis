;;* Module: base-system
(define-module (base-system)
  #:use-module (gnu)
  #:use-module (srfi srfi-1)
  #:use-module (gnu system nss)
  #:use-module (gnu system pam)

  #:use-module (gnu services pm)
  #:use-module (gnu services cups)
  #:use-module (gnu services desktop)
  #:use-module (gnu services docker)
  #:use-module (gnu services networking)
  #:use-module (gnu services sound)
  #:use-module (gnu services virtualization)
  #:use-module (gnu services authentication)
  #:use-module (gnu services security-token)

  #:use-module (gnu packages wm)
  #:use-module (gnu packages dns)
  #:use-module (gnu packages ssh)
  #:use-module (gnu packages security-token)
  #:use-module (gnu packages cryptsetup)
  #:use-module (gnu packages cups)
  #:use-module (gnu packages vim)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages emacs)
  #:use-module (gnu packages emacs-xyz)
  #:use-module (gnu packages file-systems)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages mtools)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages hardware)
  #:use-module (gnu packages libusb)
  #:use-module (gnu packages audio)
  #:use-module (gnu packages gnuzilla)
  #:use-module (gnu packages web-browsers)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages package-management)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages guile)

  ;;NONFREE
  #:use-module (nongnu packages linux)
  #:use-module (nongnu system linux-initrd)
  )

;;; TODO: create a link in /data for /etc/flatpak?
;; (instead of needing the config file)

;;** use-service-modules nix, desktop, xorg
(use-service-modules nix)
(use-service-modules desktop xorg) ;sway/wayland?

;;** use-package-modules
(use-package-modules certs tls ssh gnupg shells linux)

;;** udev rules
;;*** backlight-udev-rule
;; Add udev rule that allows members of the "video" group to change brightness.
(define-public %udev-backlight-rule
  (udev-rule
   "90-backlight.rules"
   (string-append "ACTION==\"add\", SUBSYSTEM==\"backlight\", "
                  "RUN+=\"/run/current-system/profile/bin/chgrp video /sys/class/backlight/%k/brightness\""
                  "\n"
                  "ACTION==\"add\", SUBSYSTEM==\"backlight\", "
                  "RUN+=\"/run/current-system/profile/bin/chmod g+w /sys/class/backlight/%k/brightness\"")))

;; this rule is already defined in /run/current-system/profile/lib/udev/rules.d/70-u2f.rules
;; - it is added by the
(define %udev-fido-rule
  (udev-rule
   "60-fido-id.rules"
   (string-join '("KERNEL==\"hidraw*\", SUBSYSTEM==\"hidraw\", MODE=\"0660\", GROUP=\"plugdev\", ATTRS{idVendor}==\"1050\"") "\n")))

;;(pretty-print %base-groups)

(define-public %dc-groups
  (cons* (user-group (name "realtime") (system? #t))
         (user-group (name "yubikey") (system? #t))
         (user-group (name "fuse") (system? #t))
         (user-group (name "users") (id 1100))
         (user-group (name "dc") (id 1000))
         (remove (lambda (g) (equal? (user-group-name g) "users"))
                 %base-groups)))

(define-public %dc-users '())

(define-public %dc-my-groups
  '("wheel"  ;; sudo
    "netdev" ;; network devices
    "kvm"
    "tty"
    "input"
    "realtime" ;; Enable RT scheduling
    "lp"       ;; control bluetooth and cups
    "audio"    ;; control audio
    "video"    ;; control video
    ;; TODO: configure udev for group
    "yubikey" ;; yubikey (udev)
    "plugdev" ;; libu2f-host (udev)
    "users"
    "fuse"))

(define-public (dc-user my-groups)
  (user-account
   (uid 1000)
   (name "dc")
   (comment "David Conner")
   (group "dc")
   (home-directory "/home/dc")
   (supplementary-groups my-groups)))

;;(pretty-print (map user-group-name %base-groups))
;;(pretty-print (map (lambda (g) (eq? (user-group-name g) "users"))
;;   %base-groups))
;;(pretty-print dc-groups)

(define-public (remove-pulseaudio-service services-list)
  (remove (lambda (service)
            (eq? (service-kind service) pulseaudio-service-type))
          services-list))

(define-public (remove-gdm-service services-list)
  (remove (lambda (service)
            (eq? (service-kind service) gdm-service-type))
          services-list))

;;** %dc-i2c-packages
;; the following should be made available to a root profile via a manifest
;; - hw-probe (not yet tested)
;; - i2c-tools (requires i2c-dev kernel module & group membership)
;; - ddcci-driver-linux
;; - ddcui (can be loaded in user-land)
;;
;; these packages can also be made available to a secondary login user
;; with appropriate groups:
;;
;; modules should be loaded on-demand
;;
;; - https://gitlab.com/ddcci-driver-linux/ddcci-driver-linux
;; - https://github.com/dcunited001/zettelkasten/blob/master/slips/arch-setup-ddcutil-for-remote-display-config.org
(define-public %dc-i2c-packages
  (list i2c-tools
        hw-probe
        ddcci-driver-linux
        ddcutil
        ;; ddcui
        ))

;;** %dc-desktop-packages
(define-public %dc-desktop-packages
  (append (list
           git
           lvm2
           cryptsetup
           ntfs-3g
           exfat-utils
           fuse-exfat
           hwinfo
           stow
           vim

           ;; TODO: consider moving these to /root/.config/guix/manifests/emacs-root.org
           ;; emacs-native-comp
           emacs
           emacs-better-defaults
           emacs-auto-complete
           emacs-hydra
           emacs-modus-themes
           emacs-dash
           emacs-lispy
           emacs-geiser
           emacs-geiser-guile
           emacs-ac-geiser
           emacs-guix
           emacs-yasnippet
           emacs-yasnippet-snippets
           emacs-with-editor

           xterm
           bluez
           bluez-alsa
           pipewire ;; TODO: pipewire?
           xf86-input-libinput

           tlp
           cpupower
           turbostat
           lm-sensors

           openssh
           openssl

           ccid
           yubikey-personalization
           ;; breaking python crypto version
           ;; python-yubikey-manager
           libu2f-host
           ;; libfido2 ;; included as dependency
           opensc ;; for pkcs#11 (ssh using smartcard PIV certs)
           gnupg
           pcsc-lite
           hidapi ;; for HID devices to control FIDO/U2F

           rng-tools ;; req. to seed /dev/random with entropy from yubikey

           ;; required for wacom
           ;; - libwacom modifies udev rules & must be in system config
           xf86-input-wacom
           libwacom

           ;; required for xdg-user-dirs-update
           xdg-user-dirs
           libnotify

           ;; usbmuxd and ifuse for iphone-usb
           usbmuxd
           ifuse

           guile-fibers

           gvfs
           nss-certs)
          %base-packages))

;;** dc-desktop-services
;;
;; Override the default %desktop-services to add the udev backlight config
;; and include OpenVPN in the list of NetworkManager plugins.
(define-public dc-desktop-services
  (modify-services
      %desktop-services

    (elogind-service-type
     config =>
     (elogind-configuration
      (inherit config)
      (handle-lid-switch-external-power 'suspend)))

    (network-manager-service-type
     config =>
     (network-manager-configuration
      (inherit config)
      (vpn-plugins (list network-manager-openvpn))))))

;;** %dc-nntp-config
(define-public %dc-nntpserver
  (plain-file "nntpserver.conf"
              "news.gmane.io"))

;;** %xorg-libinput-config

;; Use the =libinput-driver= for all input device
;;
;; Define the =base-operating-system=
;; which will be inherited by all machine configurations.
;; TODO: update this for wacom device

(define-public %xorg-libinput-config
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

(define-public %dc-default-shell-keyboard
  (keyboard-layout "us" "altgr-intl"
                   #:model "pc105"
                   #:options '("caps:escape")))

(define-public %dc-broadcom-modules
  '("b43"
    "b43-legacy"
    "ssb"
    "bcm43xx"
    "brcm80211"
    "brcmfmac"
    "brcmsmac"
    "bcma"))

(define-public (dc-modprobe-blacklist modules)
  (string-concatenate
   (list "modprobe.blacklist=" 
         (string-join
          modules
          ","))))

;; (define-public xsecurelock-service-type
;;   (service-type
;;    (name 'xsecurelock)
;;    (extensions
;;     (list (service-extension pam-root-service-type
;;                              screen-locker-pam-services)
;;           (service-extension setuid-program-service-type
;;                              ;; (lambda (program)  ... )
;;                              (setuid-program
;;                               ((lambda (program)
;;                                  (pretty-print  (string-append  #$xsecure-lock "/libexec/xsecurelock/authproto_pam"))
;;                                  program
;;                                  )
;;                                (program (string-append  #$xsecure-lock "/libexec/xsecurelock/authproto_pam"))))
;;                              )))
;;    (description "Setup xsecurelock with authproto_pam to run xscreensaver and configure it as a PAM service")))

;;** base-operating-system
(define-public base-operating-system
  (operating-system
    (host-name "eerse")
    (timezone "America/New_York")
    (locale "en_US.UTF-8")

    ;; FREE (specify kernel in system.scm
    ;; (kernel linux-libre)                    ;use the non-free Linux kernel and firmware

    ;; NONFREE
    (firmware (cons* linux-firmware
                     %base-firmware))

    ;; NONFREE
    (initrd microcode-initrd)

    (keyboard-layout %dc-default-shell-keyboard)

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

    (groups %dc-groups)
    (users (append (list (dc-user %dc-my-groups))
                   %dc-users
                   %base-user-accounts))

    ;; install bare-minimum system packages
    (packages %dc-desktop-packages)

    ;; doesn't setup a guix service (and requires a pam service)
    ;; (setuid-programs
    ;;  (cons*
    ;;   (setuid-program
    ;;    (program (file-append xsecure-lock "/libexec/xsecurelock/authproto_pam")))
    ;;   %setuid-programs))

    ;; networking
    (services (cons*

               ;; TODO: tweak TLP config
               ;; - ensure cpu-scaling-governor-on-ac is not affecting performance
               (service tlp-service-type
                        (tlp-configuration
                         (nmi-watchdog? #t)
                         (cpu-boost-on-ac? #t)
                         (tlp-default-mode "AC") ;; this is the default
                         (wifi-pwr-on-bat? #t)))

                ;; This enables JACK to enter realtime mode
               (pam-limits-service-type
                (list
                 (pam-limits-entry "@realtime" 'both 'rtprio 99)
                 (pam-limits-entry "@realtime" 'both 'memlock 'unlimited)))

               (extra-special-file "/usr/bin/env"
                                   (file-append coreutils "/bin/env"))

               (service thermald-service-type)

               ;; (service docker-service-type)

               (service libvirt-service-type ;; TODO how is libvirt configured?
                        (libvirt-configuration
                         (unix-sock-group "libvirt")
                         (tls-port "16555")))

               ;; req. to start VM's with virtmanager
               (service virtlog-service-type
                        (virtlog-configuration
                         ;; (max-clients 1024) ;; default
                         (max-size (* 32 (expt 1024 2)))))

               (service pcscd-service-type)

               ;; (service cups-service-type
               ;;          (cups-configuration
               ;;           (web-interface? #t)
               ;;           (extensions
               ;;            (list cups-filters))))

               ;; (service nix-service-type)

               ;; this rule will automatically create the plugdev group on the system
               ;; - but it needs to be added to each users supplementary-groups
               (udev-rules-service 'u2f libu2f-host #:groups '("plugdev"))
               (udev-rules-service 'pipewire-add-udev-rules pipewire)
               (udev-rules-service 'backlight-rule %udev-backlight-rule)

               ;; this only tags the yubikey device with security-token in udev
               (udev-rules-service 'yubikey yubikey-personalization)

               (bluetooth-service #:auto-enable? #t)

               ;; NOTE: see also desktop-services-for-system
               ;;   in guix/gnu/services/desktop.scm

               (remove-pulseaudio-service dc-desktop-services)))

    ;; allow resolution of '.local' hostnames with mDNS
    (name-service-switch %mdns-host-lookup-nss)))
