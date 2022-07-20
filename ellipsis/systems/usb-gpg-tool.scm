;;* Module
(define-module (ellipsis systems usb-gpg-tools)
  #:use-module (gnu)
  #:use-module (gnu system)
  #:use-module (gnu system nss)
  #:use-module (gnu system pam)

  ;;** Basic Packages
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages package-management) ;; TODO: remove?
  #:use-module (gnu packages vim)
  ;; #:use-module (gnu packages curl)
  #:use-module (gnu packages emacs)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages mtools) ;; for msdos file systems
  #:use-module (gnu packages file-systems)

  ;;** PGP Packages
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages security-token)

  ;;** PGP Services
  #:use-module (gnu services authentication)
  #:use-module (gnu services security-token)

  #:export (usb-gpg-tools)

  ;;NONFREE
  ;; #:use-module (nongnu packages linux)
  ;; #:use-module (nongnu system linux-initrd)
  )

;; networking is [probably] needed for loopback
(use-service-modules networking ssh security-token)
(use-package-modules wget vim emacs emacs-xyz)
(use-package-modules linux mtools file-systems version-control)
(use-package-modules screen ssh gnupg cryptsetup security-token)

(define %my-user "dc")

(define %my-services
  (modify-services
   %base-services
   (mingetty-service-type config => (mingetty-configuration
                                     (inherit config)
                                     (auto-login %my-user)))))

;;** Image
(define usb-gpg-tools
  (operating-system
   (host-name "usbgpgtool")
   (timezone "America/New_York")
   (locale "en_US.utf8")

   ;; to install on a system with just BIOS (e.g. a VM)
   (bootloader (bootloader-configuration
                (bootloader grub-efi-bootloader)
                (targets "/dev/sda")))
   (file-systems (cons (file-system
                        (device (file-system-label "root"))
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

   (packages
    (append (list lvm2
                  cryptsetup
                  ntfs-3g
                  exfat-utils
                  fuse-exfat
                  openssh

                  git
                  ;; curl
                  stow
                  vim

                  emacs-no-x-toolkit
                  emacs-better-defaults
                  emacs-auto-complete
                  emacs-hydra
                  emacs-modus-themes
                  emacs-dash
                  emacs-lispy
                  emacs-geiser
                  emacs-geiser-guile
                  emacs-guix
                  emacs-yasnippet
                  emacs-yasnippet-snippets

                  pcsc-lite
                  screen
                  gnupg

                  ccid
                  yubikey-personalization
                  python-yubikey-manager
                  libu2f-host

                  pinentry-tty
                  paperkey

                  wget
                  )
            %base-packages-disk-utilities
            %base-packages))

   (services
    (append (list
             ;; (dhcp-client-service-type)
             (service pcscd-service-type)
             ;; NOTE: udev rules should automatically be in place
             ;; (udev-rules-service
             ;;  (udev-rule "90-libu2f.rules"
             ;;             (string-append "")))

             )

            %my-services
            ))))

usb-gpg-tools