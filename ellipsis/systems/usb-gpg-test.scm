;;* Module
(define-module (ellipsis systems usb-gpg-test)
  #:use-module (gnu system)
  #:use-module (gnu system nss)
  #:use-module (gnu system pam)
  #:use-module (gnu system install) ; TODO necessary to (inherit installtion-os)

  ;;** Basic Packages
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages package-management) ;; TODO: remove?
  #:use-module (gnu packages vim)
  ;; #:use-module (gnu packages curl)
  #:use-module (gnu packages emacs)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages mtools)    ;; for msdos file systems
  #:use-module (gnu packages file-systems)

  ;;** PGP Packages
  #:use-module (gnu packages security-token)

  ;;** PGP Services
  #:use-module (gnu services authentication)
  #:use-module (gnu services security-token)

  #:export (usb-gpg-tools))

;; networking is [probably] needed for loopback
(use-service-modules networking ssh)
(use-service-modules udev security-token)
(use-package-modules vim emacs linux mtools file-systems version-control)
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
   (locale "en_us.UTF-8")

   ;; to install on a system with just BIOS (e.g. a VM)
   (bootloader (bootloader-configuration
                (bootloader grub-efi-bootloader)
                (targets "" '("/dev/sda"))))
   (file-systems (cons (file-system
                        (device (file-system-label "root"))
                        (mount-point "/")
                        (type "ext4"))
                       %base-file-systems))

   ;; TODO: (bootloader...)
   ;; TODO: (file-systems...)

   (kernel-arguments '("modprobe.blacklist=radeon"
                       ;; "quiet" ;; .....
                       ;; "net.iframes=0"
                       ))

   ;; TODO: users/groups (autologin to tty

   (groups (cons* (user-group (name "plugdev") (system? #t))
                  %base-groups))
   (users (cons (user-account
                 (name %my-user)
                 (comment "Default User")
                 (group "users")
                 (supplementary-groups '("wheel"
                                         "tty"
                                         "plugdev")))))

   (packages
    (append (list exfat-utils
                  fuse-exfat
                  git
                  ;; curl
                  stow
                  vim
                  emacs-no-x-toolkit
                  pcsc-lite
                  screen
                  gnupg

                  ;; Yubikey tools
                  python-yubikey-manager

                  ;; pinentry
                  pinentry-curses
                  ;; pinentry-tty?
                  ;; pinentry-qt?
                  paperkey

                  ;; pcscd
                  ccid

                  ;; for host-side u2f protocol (should include udev rules)
                  libu2f-host

                  wget
                  )
            %base-packages-disk-utilites
            %base-packages))

   (services
    (append (list
             ;; (dhcp-client-service-type)
             (service pcscd-service-type)
             ;; (udev-rules-service
             ;;  (udev-rule "90-libu2f.rules"
             ;;             (string-append "")))

             )

            %my-services
            ))))

;; usb-gpg-tools
