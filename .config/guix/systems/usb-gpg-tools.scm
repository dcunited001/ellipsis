
;;* Module
(define-module (dc usb-gpg-tools)
  #:use-module (gnu system)
  #:use-module (gnu system install) ; TODO necessary to (inherit installtion-os)

  ;;** Basic Packages
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages package-management) ;; TODO: remove?
  #:use-module (gnu packages vim)
  ;; #:use-module (gnu packages curl)
  #:use-module (gnu packages emacs)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages mtools)
  #:use-module (gnu packages file-systems)

  ;;** PGP Packages
  #:use-module (gnu packages security-token)
  ;; gnu/packages/openpgp.scm
  ;; gnu/packages/gnu-pw-mgr.scm

  ;;** PGP Services
  #:use-module (gnu services authentication)

  #:export (usb-gpg-tools))

;; TODO: pcscd support
;; - pcsd
;; TODO: udev.packages support for yubikey-personalization
;; TODO: Nix packages: gnupg pinentry-curses pinentry-qt paperkey wget
;; TODO: services: disable ssh-agent
;; TODO: services: enable gpg-agent (+enable ssh support)
;; TODO: validate inherited packages/services from installation-os

;;** Image
(define usb-gpg-tools
  (operating-system
   (inherit installation-os) ;; TODO is there a better starting point?
   (kernel-arguments '("modprobe.blacklist=radeon" ;; TODO double-check
                       ;; "quiet" ;; .....
                       ;; "net.iframes=0"
                       ))
   (packages (append (list exfat-utils
                           fuse-exfat
                           git
                           ;; curl
                           stow
                           vim
                           emacs-no-x-toolkit

                           pcsc-lite
                           )
                     (operating-system-packages
                      installation-os)

;;;  TODO check essential-services
;;;  TODO check services
;;;  TODO check pam-services
                     ))))

;; usb-gpg-tools
