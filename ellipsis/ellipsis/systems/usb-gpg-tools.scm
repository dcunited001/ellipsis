;;; Module
(define-module (ellipsis systems usb-gpg-tools)
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

  #:export (usb-gpg-tools))

(use-modules (guix utils))

;; This system does contain nonfree software built from patched binaries, but
;; it includes LinuxLibre and does not include Intel/AMD microcode.

;;;; AGE keygen: golang golang-crypto
;;;; PGP Packages: gnupg security-token
;;;; PGP Services: authentication security-token

;; certbot/letsencrypt packages
;; #:use-module (gnu services certbot)

;; networking is [probably] needed for loopback
(use-service-modules networking ssh security-token authentication)
(use-package-modules wget screen password-utils vim emacs emacs-xyz ; curl
                     package-management                             ; remove?
                     linux time mtools lsof file-systems disk version-control
                     ssh gnupg cryptsetup security-token tls certs libusb
                     golang-crypto)

(define %my-user "dc")

(define %my-services
  (modify-services
      %base-services

    (agetty-service-type
     config => (agetty-configuration
                (inherit config)
                (login-pause? #t)
                (timeout 30)))

    (mingetty-service-type
     config => (mingetty-configuration
                (inherit config)
                ;; (auto-login %my-user)
                (login-pause? #t)))))

;;;; Image
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

    ;; NONFREE
    ;; (kernel linux)

    (kernel-arguments '("modprobe.blacklist=radeon"
                        ;; "quiet" ;; .....
                        ;; "net.iframes=0"
                        ))

    ;; TODO: users/groups (autologin to tty

    (groups (append (list (user-group (name "plugdev") (system? #t)))
                    %base-groups))
    (users (append (list
                    (user-account
                     (name %my-user)
                     (comment "Default User")
                     (group "users")
                     (supplementary-groups '("wheel"
                                             "tty"
                                             "plugdev"))))
                   %base-user-accounts))



    ;; TODO:  add YK UDev Rules?
    ;; (udev-rules-service 'fido2 libfido2 #:groups '("plugdev"))
    ;; (udev-rules-service 'u2f libu2f-host #:groups '("plugdev"))
    ;; (udev-rules-service 'yubikey yubikey-personalization)

    ;; misc packages:
    ;; f3: test flash storage
    ;; paperkey: print keys to paper
    ;; certdata2pem: convert between cert formats
    ;; datefudge: mock system time to set arbitrary cert start times
    ;; exfat-utils: work with FAT disks
    ;; pwsafe: manage passwords

    (packages
     (append (list lvm2
                   cryptsetup
                   dosfstools
                   ntfs-3g
                   exfat-utils
                   fuse-exfat
                   f3

                   lsof

                   wget
                   git
                   stow
                   vim

                   screen
                   emacs-no-x-toolkit
                   emacs-x509-mode ;; very helpful for certs
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
                   emacs-yasnippet-snippets

                   ;; req. to seed /dev/random with entropy from yubikey
                   rng-tools

                   screen
                   openssh
                   openssl

                   pcsc-lite
                   gnupg

                   ccid
                   yubico-piv-tool
                   yubikey-personalization
                   python-yubikey-manager
                   libu2f-host
                   opensc
                   hidapi ;; HID Devices for FIDO/OTP

                   pinentry-tty
                   paperkey
                   datefudge

                   ;; TODO: remove shroud-nox?
                   ;; shroud-nox

                   le-certs
                   gnutls

                   ;; NOTE: step-kms-plugin should work if ldd discovers
                   ;; pscscd via rpath
                   step-kms-plugin-bin
                   step-ca-bin
                   step-cli-bin

                   certdata2pem
                   ;; desec-certbot-hook

                   tpm2-tss
                   ssh-agent-tpm-bin
                   age
                   sops-bin
                   age-keygen
                   age-plugin-tpm-bin
                   age-plugin-yubikey-bin)
             %base-packages))

    ;; age

    ;; acl
    ;; hwinfo
    ;; libfido2 ;; included as dependency

    (services
     (append (list
              ;; (dhcp-client-service-type)
              (service pcscd-service-type))

             %my-services))))

;; TODO: add gnupg service if configuration file is in place

usb-gpg-tools
