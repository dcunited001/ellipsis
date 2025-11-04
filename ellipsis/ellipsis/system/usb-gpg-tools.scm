;;; Module
(define-module (ellipsis system usb-gpg-tools)
  #:use-module (srfi srfi-1)
  #:use-module (gnu)
  #:use-module (gnu system)
  #:use-module (gnu system nss)
  #:use-module (gnu system pam)

  #:use-module (ellipsis packages emacs-xyz)
  #:use-module (ellipsis services security-token)
  #:use-module (ellipsis system common)
  ;; #:use-module (gnu services certbot)

  #:use-module (nongnu packages linux)
  #:use-module (nongnu system linux-initrd)

  #:export (usb-gpg-tools
            usb-gpg-tools-amd))

(use-modules (guix utils))

;;;; AGE keygen: golang golang-crypto
;;;; PGP Packages: gnupg security-token
;;;; PGP Services: authentication security-token

;; certbot/letsencrypt packages


;; TODO: add gnupg service? if configuration file is in place

;; networking is needed for loopback and maybe other use cases
(use-service-modules networking ssh security-token authentication)
(use-package-modules emacs emacs-xyz emacs-build)

(define %ugt-my-groups
  '("wheel" "users" "tty" "dialout"
    "input" "video" "audio" "netdev" "lp"
    ;; "kmem" "disk" "floppy" "cdrom" "tape" "kvm"
    "fuse" "yubikey" "plugdev" "users"))

(define %ugt-system-groups
  (cons*
   (user-group (name "plugdev") (system? #t))
   (user-group (name "yubikey") (system? #t))
   (user-group (name "fuse") (system? #t))
   ;; (user-group (name "seat") (system? #t))
   %base-groups))

(define %ugt-user-name "dc")
(define %ugt-user-pass "dc1234231")
(define %ugt-user-groups
  (list
   (user-group (name %ugt-user-name) (id 1000))
   (user-group (name "users") (id 1100))))

;; TODO %ugt-default-user set to 1000:1000 to be consistent
(define %ugt-default-user
  (user-account
    (name "dc")
    (comment "Default User")
    (group %ugt-user-name)
    (supplementary-groups %ugt-my-groups)))

(define-public %ugt-packages-emacs
  ;; still needs either emacs or emacs-no-x-toolkit
  (list
   emacs-x509-mode
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
   emacs-yasnippet-snippets))

(define %ugt-services
  (append
   (list
    (service pcscd-service-type))
   yubikey-udev-rules

   (modify-services %base-services
     (agetty-service-type
      config => (agetty-configuration
                  (inherit config)
                  (login-pause? #t)
                  (timeout 30)))

     (mingetty-service-type
      config => (mingetty-configuration
                  (inherit config)
                  (login-pause? #t))))))

;;;; Image

;; guix system -L ~/.dotfiles/ellipsis -L ~/.dotfiles/dc \
;; image --image-type=iso9660 \
;; -e '(@@ (ellipsis system usb-gpg-tools) usb-gpg-tools)'
(define usb-gpg-tools
  (operating-system
    (host-name "usbgpgtool")
    (timezone "America/New_York")
    (locale "en_US.UTF-8")

    ;; to install on a system with just BIOS (e.g. a VM)
    (bootloader (bootloader-configuration
                  (bootloader grub-efi-bootloader)
                  (targets "/dev/sda")))

    (file-systems (cons (file-system
                          (device (file-system-label "usb-gpg-disk"))
                          (mount-point "/")
                          (type "ext4"))
                        %base-file-systems))
    (kernel-arguments '("modprobe.blacklist=radeon"
                        ;; "quiet" ;; .....
                        ;; "net.iframes=0"
                        ))
    (groups (append %ugt-user-groups %ugt-system-groups))
    (users (append (list %ugt-default-user)
                   %base-user-accounts))

    ;; misc packages:
    ;; f3: test flash storage
    ;; paperkey: print keys to paper
    ;; certdata2pem: convert between cert formats
    ;; datefudge: mock system time to set arbitrary cert start times
    ;; exfat-utils: work with FAT disks
    ;; pwsafe: manage passwords

    (packages
     (append

      ;; see ./ellipsis/ellipsis/system/common.scm for packages
      %el-profile-pkgs-cli
      %el-profile-pkgs-net
      %el-profile-pkgs-net-plus
      %el-profile-pkgs-data
      %el-profile-pkgs-fs
      %el-profile-pkgs-hardware
      %el-profile-pkgs-i2c
      %el-profile-pkgs-age
      %el-profile-pkgs-tls
      %el-profile-pkgs-smartcard
      %el-profile-pkgs-yubikey
      %el-profile-pkgs-step
      %el-profile-pkgs-gnupg
      %el-profile-pkgs-secrets
      %el-profile-pkgs-tpm

      (list emacs-no-x-toolkit)
      %ugt-packages-emacs

      %base-packages))

    (services %ugt-services)))

;; guix system -L ~/.dotfiles/ellipsis -L ~/.dotfiles/dc \
;; image --image-type=iso9660 \
;; -e '(@@ (ellipsis system usb-gpg-tools) usb-gpg-tools-amd)'

(define usb-gpg-tools-amd
  (operating-system
    (inherit usb-gpg-tools)
    (host-name "usbgpgtools-amd")
    (timezone "America/New_York")
    (locale "en_US.UTF-8")

    ;; NONFREE
    (kernel linux)
    (firmware (cons* ;; linux-firmware
               amd-microcode
               amdgpu-firmware
               realtek-firmware
               %base-firmware))

    (kernel-arguments '("modprobe.blacklist=radeon"
                        ;; "quiet" ;; .....
                        ;; "net.iframes=0"
                        ))))
