
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

  ;; only for keepass w/ yubico support
  ;; #:use-module (gnu packages password-utils)

  ;; only for yubico-pam
  ;; #:use-module (gnu packages authentication)
  ;; gnu/packages/openpgp.scm
  ;; gnu/packages/gnu-pw-mgr.scm

  ;;** PGP Services
  #:use-module (gnu services authentication)

  #:export (usb-gpg-tools))


;; TODO: this will require building an image with firmware
;; - e.g. on the HP laptop
;; TODO: pcscd support
;; - pcsd
;; TODO: udev.packages support for yubikey-personalization
;; TODO: Nix packages: gnupg pinentry-curses pinentry-qt paperkey wget
;; TODO: services: disable ssh-agent
;; TODO: services: enable gpg-agent (+enable ssh support)
;; TODO: validate inherited packages/services from installation-os

;;** Image
;; TODO: consider (define (usb-gpg-tools some-clojure-destructured-args) ...)
;; - to pass things like
;;
;; invoke this style with
;;
;; ... or consider extending
(define usb-gpg-tools
  (operating-system
   (inherit installation-os) ;; TODO is there a better starting point?
   ;;
   ;; TODO: see if kernel-arguments are composable?
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

                           pcsc-lite)
                     (operating-system-packages
                      installation-os)

;;;  TODO check essential-services
;;;  TODO check services
;;;  TODO check pam-services
                     ))))

;; usb-gpg-tools
