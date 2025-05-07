;;; Module
(define-module (ellipsis system usb-gpg-tools)
  #:use-module (srfi srfi-1)
  #:use-module (gnu)
  #:use-module (gnu system)
  #:use-module (gnu system nss)
  #:use-module (gnu system pam)

  #:use-module (ellipsis packages gnupg)
  #:use-module (ellipsis packages tls)
  #:use-module (ellipsis packages emacs-xyz)
  #:use-module (ellipsis packages password-utils)
  #:use-module (ellipsis packages security-token)
  #:use-module (ellipsis packages golang-crypto)
  #:use-module (ellipsis services security-token)

  #:use-module (nongnu packages linux)
  #:use-module (nongnu system linux-initrd)

  #:export (usb-gpg-tools
            usb-gpg-tools-amd))

(use-modules (guix utils))

;; This system does contain nonfree software built from patched binaries, but
;; it includes LinuxLibre and does not include Intel/AMD microcode.

;;;; AGE keygen: golang golang-crypto
;;;; PGP Packages: gnupg security-token
;;;; PGP Services: authentication security-token

;; certbot/letsencrypt packages
;; #:use-module (gnu services certbot)

;; networking is needed for loopback and maybe other use cases

;; TODO: add gnupg service if configuration file is in place

(use-service-modules networking ssh security-token authentication)
(use-package-modules wget curl screen password-utils vim tmux emacs emacs-xyz
                     package-management ; remove?
                     networking linux hardware rsync acl
                     time mtools lsof file-systems disk version-control
                     ssh gnupg cryptsetup security-token tls certs libusb
                     golang-crypto)

(define %ugt-my-groups
  '("wheel" "users" "tty" "dialout"
    "input" "video" "audio" "netdev" "lp"
    ;; "kmem" "disk" "floppy" "cdrom" "tape" "kvm"
    "fuse" "yubikey" "plugdev"))

(define %ugt-system-groups
  (cons*
   (user-group (name "plugdev") (system? #t))
   (user-group (name "yubikey") (system? #t))
   (user-group (name "fuse") (system? #t))
   ;; (user-group (name "seat") (system? #t))
   %base-groups))

(define %ugt-default-user
  (user-account
   (name "dc")
   (comment "Default User")
   (group "users")
   (supplementary-groups %ugt-my-groups)))

(define-public %ugt-packages-cli
  (list lsof git stow vim screen tmux))

;; TODO: enable local networking for usb-gpg-tools
(define-public %ugt-packages-net
  (list tunctl bridge-utils iptables-nft))

(define-public %ugt-packages-net-plus
  (list wget curl rsync))

(define-public %ugt-packages-hardware
  (list lvm2 cryptsetup dosfstools ntfs-3g exfat-utils fuse-exfat f3
        acl hwinfo rng-tools hw-probe))

(define-public %ugt-packages-i2c
  (list i2c-tools ddcci-driver-linux ddcutil))

(define-public %ugt-packages-age
  (list age age-keygen age-plugin-tpm-bin age-plugin-yubikey-bin))

(define-public %ugt-packages-tls
  ;; desec-certbot-hook
  (list openssh openssl le-certs gnutls certdata2pem))

(define-public %ugt-packages-smartcard
  ;; hidapi: HID Devices for FIDO/OTP
  (list ccid pcsc-lite opensc pinentry-tty hidapi libu2f-host libfido2))

(define-public %ugt-packages-yubikey
  (list yubico-piv-tool yubikey-personalization python-yubikey-manager))

;; NOTE: step-kms-plugin should work if ldd discovers
;; pscscd via rpath

(define-public %ugt-packages-step
  (list step-kms-plugin-bin step-ca-bin step-cli-bin))

(define-public %ugt-packages-gnupg
  (list gnupg paperkey datefudge))

(define-public %ugt-packages-secrets
  (list sops-bin))

(define-public %ugt-packages-tpm
  (list tpm2-tss ssh-tpm-agent-bin))

(define-public %ugt-packages-emacs
  ;; still needs either emacs or emacs-no-x-toolkit
  (list
   emacs-x509-mode
   emacs-better-defaults
   ;; emacs-with-profile
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
   (yubikey-udev-rules)

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
    (groups %ugt-system-groups)
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

      %ugt-packages-cli
      %ugt-packages-net
      %ugt-packages-net-plus
      %ugt-packages-hardware
      %ugt-packages-i2c
      %ugt-packages-age
      %ugt-packages-tls
      %ugt-packages-smartcard
      %ugt-packages-yubikey
      %ugt-packages-step
      %ugt-packages-gnupg
      %ugt-packages-secrets
      %ugt-packages-tpm

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
