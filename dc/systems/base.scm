(define-module (dc systems base)
  #:use-module (srfi srfi-1)
  #:use-module (guix gexp)
  #:use-module (guix channels)
  #:use-module (gnu)
  #:use-module (gnu system)
  #:use-module (gnu system nss)
  #:use-module (gnu system setuid)
  ;; #:use-module (nongnu packages linux)
  ;; #:use-module (nongnu system linux-initrd)

  )


(use-service-modules guix admin sysctl pm nix
                     avahi dbus cups desktop linux
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

(define-public %dc-nntp-service
  ;; for GNUS
  (simple-service 'nntp-config etc-service-type
                  (list `("nntpserver"
                          ,%dc-nntpserver))))

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

(define-public %dc-docker-service
  (service docker-service-type
           (docker-configuration
            (enable-proxy? #f))))

(define-public %dc-libvirt-service
  (service libvirt-service-type
           (libvirt-configuration
            (unix-sock-group "libvirt")
            (tls-port "16555"))))

(define-public %dc-virtlog-service
  (service virtlog-service-type
           (virtlog-configuration
            ;; (max-clients 1024) ;; default
            (max-size (* 32 (expt 1024 2))))))

(define-public %dc-cups-service
  (service cups-service-type
           (cups-configuration
            (web-interface? #t)
            ;; TODO ssl-options? TLS 1.0+
            (extensions
             (list cups-filters
                   epson-inkjet-printer-escpr
                   hplip-minimal)))))

(define-public %dc-pam-limits-service
  ;; TODO: (service pam-limits-service-type
  (pam-limits-service
   (list
    (pam-limits-entry "@realtime" 'both 'rtprio 99)
    (pam-limits-entry "@realtime" 'both 'nice -19)
    (pam-limits-entry "@realtime" 'both 'memlock 'unlimited))))

;; TODO: this won't have environment vars set (for ssh)
(define-public %dc-unattended-upgrade-service-type
  (service
   unattended-upgrade-service-type
   (unattended-upgrade-configuration
    (schedule "30 2 * * 0")
    (channels #~(list
                 (channel
                  (name 'nonguix)
                  (url "https://gitlab.com/nonguix/nonguix")
                  (branch "master"))
                 %default-channels))
    ;; (operating-system-file
    ;;  (file-append
    ;;   (local-file "." "systems-dir" #:recursive? #t)
    ;;   (string-append
    ;;    "/root/.config/guix/systems/" %host-name ".scm")))
    (system-expiration (* 6 7 24 3600)))))

(define-public %dc-nonguix-substitutes-service
  (simple-service
   'add-nonguix-substitutes
   guix-service-type
   (guix-extension
    (substitute-urls
     (append (list "https://substitutes.nonguix.org")
             %default-substitute-urls))
    (authorized-keys
     (append (list (plain-file "nonguix.pub" "(public-key (ecc (curve Ed25519) (q #C1FD53E5D4CE971933EC50C9F307AE2171A2D3B52C804642A7A35F84F3A4EA98#)))"))
             %default-authorized-guix-keys)))))

(define-public %dc-extra-file-env
  (extra-special-file "/usr/bin/env"
                      (file-append coreutils "/bin/env")))

(define-public %dc-extra-file-ld-linux
  (extra-special-file "/lib64/ld-linux-x86-64.so.2"
                      (file-append glibc "/lib/ld-linux-x86-64.so.2")))

;; these files are not in the repo and will need to be added if the function is
;; added to a system
(define-public (dc-extra-file-flatpak)
  (extra-special-file
   "/etc/flatpak/installations.d"
   (file-union "installations.d"
               `(("steam.conf" ,(local-file "flatpak/installations.d/steam.conf"))
                 ("agenda.conf" ,(local-file "flatpak/installations.d/agenda.conf"))))))

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

;; DNS is wierd on some networks, other times my network is disabled
;; i would like to be sure that it's definitely the latter
(define-public %dc-ntp-service
  (service ntp-service-type
           (ntp-configuration
            (servers
             (list (ntp-server
                    (type 'pool)
                    (address "1.us.pool.ntp.org")
                    (options '("iburst")))
                   (ntp-server
                    (type 'pool)
                    (address "2.us.pool.ntp.org")
                    (options '("iburst")))
                   (ntp-server
                    (type 'pool)
                    (address "3.us.pool.ntp.org")
                    (options '("iburst"))))))))

(define-public %dc-network-manager-service
  (service network-manager-service-type
           (network-manager-configuration
            (vpn-plugins
             (list network-manager-openvpn)))))

;; rasdaemon-service/type, rasdaemon-configuration
;;
;; - helps anticipate hardware failures by scanning events, analysis appended to
;; syslog
;;
;; - with record? t, also structure logs as sqlite database:
;; /var/lib/rasdaemon/ras-mc_event.db
(define-public %dc-ras-daemon-service
  (service rasdaemon-service-type
           (rasdaemon-configuration (record? #t))))

(define-public %dc-i2c-packages
  (list i2c-tools
        hw-probe
        ddcci-driver-linux
        ddcutil
        ;; ddcui
        ))

(define-public %dc-packages-xorg-only
  (list

   ;; required for wacom
   ;; - libwacom modifies udev rules & must be in system config
   xf86-input-libinput
   xf86-input-wacom
   libwacom))

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
