(define-module (dc systems base)
  #:use-module (srfi srfi-1)
  #:use-module (gnu)
  #:use-module (gnu system)
  #:use-module (gnu system nss)
  #:use-module (gnu system setuid)
  ;; #:use-module (nongnu packages linux)
  ;; #:use-module (nongnu system linux-initrd)

  )


(use-service-modules guix admin sysctl pm nix
                     avahi dbus cups desktop
                     mcron networking xorg ssh
                     docker audio virtualization)

(use-package-modules nfs certs shells ssh tls gnupg security-token
                     bash emacs emacs-xyz gnome networking libusb
                     fonts cups audio xorg xdisorg linux file-systems
                     version-control package-management freedesktop
                     cryptsetup hardware guile vim)
;; guile-xyz: guile-fibers

(define-public %dc-kernel-modules
  ;; so gstreamer can hook into a virtual video device
  (list v4l2loopback-linux-module))

(define-public %dc-default-shell-keyboard
  (keyboard-layout "us" "altgr-intl"
                   #:model "pc105"
                   #:options '("caps:escape")))

(define-public %dc-nntpserver
  (plain-file "nntpserver.conf"
              "news.gmane.io"))

(define-public %dc-base-groups
  (cons* (user-group (name "realtime") (system? #t))
         ;; created by service
         ;; (user-group (system? #t) (name "docker"))
         (user-group (name "plugdev") (system? #t))
         (user-group (name "yubikey") (system? #t))
         (user-group (name "fuse") (system? #t))
         (user-group (name "users") (id 1100))
         (user-group (name "dc") (id 1000))
         (remove (lambda (g) (equal? (user-group-name g) "users"))
                 %base-groups)))

;; add libvirt & docker to the users who need it
(define-public %dc-my-groups
  '("wheel"  ;; sudo
    "netdev" ;; network devices
    "kvm"
    "tty"
    "input"
    "fuse"
    "realtime" ;; Enable RT scheduling
    "lp"       ;; control bluetooth and cups
    "audio"    ;; control audio
    "video"    ;; control video
    ;; TODO: configure udev for group
    "yubikey" ;; yubikey (udev)
    "plugdev" ;; libu2f-host (udev)
    "users"))

(define-public (dc-user my-groups)
  (user-account
   (uid 1000)
   (name "dc")
   (comment "David Conner")
   (group "dc")
   (home-directory "/home/dc")
   (supplementary-groups my-groups)))

;; use with (udev-rules-service %udev-backlight-rule)
(define-public %dc-backlight-udev
  (udev-rule
   "90-backlight.rules"
   (string-append "ACTION==\"add\", SUBSYSTEM==\"backlight\", "
                  "RUN+=\"/run/current-system/profile/bin/chgrp video /sys/class/backlight/%k/brightness\""
                  "\n"
                  "ACTION==\"add\", SUBSYSTEM==\"backlight\", "
                  "RUN+=\"/run/current-system/profile/bin/chmod g+w /sys/class/backlight/%k/brightness\"")))

(define-public %dc-desktop-packages
  (list
   git
   lvm2
   cryptsetup
   ntfs-3g
   exfat-utils
   fuse-exfat
   hwinfo
   stow
   vim

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
   pipewire
   ;; xf86-input-libinput

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
   libfido2 ;; included as dependency
   opensc   ;; for pkcs#11 (ssh using smartcard PIV certs)
   gnupg
   pcsc-lite
   hidapi ;; for HID devices to control FIDO/U2F

   rng-tools ;; req. to seed /dev/random with entropy from yubikey

   ;; required for xdg-user-dirs-update
   xdg-user-dirs
   libnotify

   ;; usbmuxd and ifuse for iphone-usb
   usbmuxd
   ifuse

   gvfs
   nss-certs))

(define-public %dc-packages-xorg-only
  (list

   ;; required for wacom
   ;; - libwacom modifies udev rules & must be in system config
   xf86-input-wacom
   libwacom))
