(define-module (dc systems base)
  #:use-module (srfi srfi-1)
  #:use-module (guix gexp)
  #:use-module (guix channels)
  #:use-module (gnu)
  #:use-module (gnu system)
  #:use-module (gnu system nss)
  #:use-module (gnu system setuid)
  #:use-module (ellipsis systems common)
  #:use-module (json)
  #:use-module (json builder)

  ;; #:use-module (nongnu packages linux)
  ;; #:use-module (nongnu system linux-initrd)

  ;; #:use-module (nongnu services vpn)
  #:use-module (nongnu packages vpn))

(use-service-modules guix admin sysctl pm nix avahi dbus cups
                     desktop linux mcron networking xorg ssh
                     docker audio virtualization
                     containers)

(use-package-modules nfs certs shells ssh tls gnupg security-token acl
                     bash emacs emacs-xyz gnome networking libusb
                     texinfo fonts cups audio xorg xdisorg linux file-systems
                     version-control package-management freedesktop rsync
                     cryptsetup hardware guile vim golang golang-crypto
                     containers)

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

;; defaults to: mount,umount,fusermount,fusermount3,sudoedit,sudo,su,sg
;;   ping6,ping,newgidmap,newuidmap,newgrp,chfn,passwd
;; adds: mount.nfs, swaylock
(define-public %dc-privileged-programs
  (append (list (privileged-program
                 (program (file-append nfs-utils "/sbin/mount.nfs")))
                ;; (setuid? #t)
                (privileged-program
                 (program (file-append ntfs-3g "/sbin/mount.ntfs-3g"))))
          ;; (setuid? #t)
          %default-privileged-programs))

;; =============================================
;; see Other System Groups: https://wiki.debian.org/SystemGroups
;; ---------------------------------------------
;; included with %base-groups
;;
;; kmem: direct access to system's memory (via /dev/mem, /dev/port; formerly /dev/kmem)
;; kvm: direct access to /dev/kvm
;; kmem: ?
;; ---------------------------------------------
;; realtime: enables RT scheduling
;; plugdev: FIDO/U2F, libu2f-host (udev)
;; yubikey: access to yubikey devices (udev)
;; docker: control docker daemon (user-group gets created via account-service-type)
;; cgroup: control cgroups (for rootless podman)
;; libvirt: control virtd
;; fuse: mount filesystems w/o root
;; ---------------------------------------------

;; TODO: is kvm group required for libvirt-only access to VMs?
;; TODO: add libvirt & docker to the users who need it

(define-public %dc-base-groups
  (cons* (user-group (name "realtime") (system? #t))
         (user-group (name "render") (system? #t))
         (user-group (name "plugdev") (system? #t))
         (user-group (name "yubikey") (system? #t))
         (user-group (name "fuse") (system? #t))
         (user-group (name "cgroup") (system? #t))
         (user-group (name "seat") (system? #t))
         (user-group (name "users") (id 1100))
         (user-group (name "dc") (id 1000))
         (remove (lambda (g) (equal? (user-group-name g) "users"))
                 %base-groups)))

(define %dc-my-groups
  ;; "kmem"
  '("wheel" "users" "tty" "dialout"
    "input" "video" "audio" "netdev" "lp"
    ;; "disk" "floppy" "cdrom" "tape" "kvm"
    "fuse" "realtime" "yubikey" "plugdev"
    "libvirt" "docker" "cgroup"))

(define-public (dc-user my-groups)
  (user-account
   (uid 1000)
   (name "dc")
   (comment "David Conner")
   (group "dc")
   (home-directory "/home/dc")
   (supplementary-groups my-groups)))

;; TODO: maybe define suid elsewhere.
;; rootless-podman extends the subids-service-type
(define-public %dc-subid-range
  (subid-range (name "dc") (start 1000000) (range 65536)))

;; define the rest with subids extensions
(define-public %dc-subid-service-type
  (service subid-service-type
           (subids-configuration
            (subgids %dc-subid-range)
            (subuids %dc-subid-range))))

(define-public %dc-rootless-podman-service
  (service rootless-podman-service-type
           (rootless-podman-configuration)))

(define-public %dc-containerd-service
  (service containerd-service-type
           (containerd-configuration)))

(define-public %dc-docker-service
  (service docker-service-type
           (docker-configuration
            (enable-proxy? #f))))

;; runs docker commands under "oci-container" user, has "docker" group
(define-public %dc-oci-container-service
  (service oci-container-service-type (list)))

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

;; see man limits.conf
;; or https://baeldung.com/linux/error-too-many-open-files/
(define-public %dc-pam-limits-service
  (service pam-limits-service-type
           (list
            (pam-limits-entry "@realtime" 'both 'rtprio 99)
            (pam-limits-entry "@realtime" 'both 'nice -19)
            (pam-limits-entry "@realtime" 'both 'memlock 'unlimited))))

;; (operating-system-file
;;  (file-append
;;   (local-file "." "systems-dir" #:recursive? #t)
;;   (string-append
;;    "/root/.config/guix/systems/" %host-name ".scm")))

(define %dc-unattended-upgrade-configuration
  (unattended-upgrade-configuration
   (schedule "30 2 * * 0")
   (system-expiration (* 6 7 24 3600))))

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
     (append
      (list
       (plain-file "nonguix.pub"
                   "(public-key (ecc (curve Ed25519) (q #C1FD53E5D4CE971933EC50C9F307AE2171A2D3B52C804642A7A35F84F3A4EA98#)))"))
      %default-authorized-guix-keys)))))

(define-public %dc-extra-file-env
  (extra-special-file "/usr/bin/env"
                      (file-append coreutils "/bin/env")))

(define-public %dc-extra-file-ld-linux
  (extra-special-file "/lib64/ld-linux-x86-64.so.2"
                      (file-append glibc "/lib/ld-linux-x86-64.so.2")))

;; TODO: remove dc-extra-file-flatpak from systems
(define-public (dc-extra-file-flatpak)
  (extra-special-file
   "/etc/flatpak/installations.d"
   (file-union "installations.d"
               `
               (("steam.conf" ,(local-file "flatpak/installations.d/steam.conf"))
                ("agenda.conf"
                 ,(local-file "flatpak/installations.d/agenda.conf"))))))

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

;; TODO: auditd-configuration: %default-auditd-configuration-directory
(define-public %dc-auditd-service
  (service auditd-service-type))

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
        ddcutil
        ;; ddcui
        ))

(define-public %dc-packages-xorg-only
  (list
   xlockmore

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
   acl
   hwinfo
   stow
   vim
   rsync

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
   gnutls

   ;; no uml-utilities though (ohhh... so thats...)
   tunctl
   bridge-utils
   iptables-nft
   ;; ulogd: logging for

   zerotier

   ccid
   yubikey-personalization
   ;; breaking python crypto version
   ;; python-yubikey-manager
   libu2f-host
   libfido2 ;; included as dependency
   opensc   ;; for pkcs#11 (ssh using smartcard PIV certs)
   gnupg
   age
   pcsc-lite
   hidapi ;; for HID devices to control FIDO/U2F

   libsecret
   rng-tools ;; req. to seed /dev/random with entropy from yubikey

   ;; required for xdg-user-dirs-update
   xdg-user-dirs
   libnotify

   ;; usbmuxd and ifuse for iphone-usb
   usbmuxd
   ifuse

   ;; virt-manager needs dconf schemas/services available via dbus.
   ;; dconf

   ;; pinfo

   gvfs))
