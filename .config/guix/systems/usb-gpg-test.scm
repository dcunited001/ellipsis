
;;* Module
(define-module (dc usb-gpg-test)
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

;; networking is [probably] needed for loopback
(use-service-modules networking ssh)
(use-service-modules udev security-token)
(use-package-modules vim emacs linux mtools file-systems version-control)
(use-package-modules screen ssh gnupg cryptsetup security-token)

;; TODO: this will require building an image with firmware
;; - e.g. on the HP laptop
;; TODO: pcscd support
;; - pcsd
;; TODO: udev.packages support for yubikey-personalization
;; TODO: Nix packages: gnupg pinentry-curses pinentry-qt paperkey wget
;; TODO: services: disable ssh-agent
;; TODO: services: enable gpg-agent (+enable ssh support)
;; TODO: validate inherited packages/services from installation-os

(define usb-gpg-agent-shepherd-service
  (shepherd-service
   (documentation "Run a GPG agent")
   (provision '(usb-gpg-agent gpg-agent))
   (requirement '(user-processes syslogd loopback))

   (start #~ (make-forkexec-constructor ))
   ()))

;; if more entropy is needed
;; "echo 'SCD RANDOM 512 | gpg-connect-agent | tee /dev/random | "

;; (define-configuration usb-gpg-agent-configuration)



(define usb-gpg-agent-service-type
  (service-type
   (name 'usb-gpg-agent-service-type)
   (extensions (list
                (service-extensions profile-service-type '(gnupg))))
   (description ))

  )

(define xsecurelock-service-type
  (service-type
   (name 'xsecurelock)
   (extensions
    (list (service-extension pam-root-service-type
                             screen-locker-pam-services)
          (service-extension setuid-program-service-type
                             ;; (lambda (program)  ... )
                             (setuid-program
                              ((lambda (program)
                                 (pretty-print  (string-append  #$xsecure-lock "/libexec/xsecurelock/authproto_pam"))
                                 program
                                 )
                               (program (string-append  #$xsecure-lock "/libexec/xsecurelock/authproto_pam"))))
                             )))
   (description "Setup xsecurelock with authproto_pam to run xscreensaver and configure it as a PAM service")))


;;** Image
;; TODO: consider (define (usb-gpg-tools some-clojure-destructured-args) ...)
;; - to pass things like
;;
;; invoke this style with
;;
;; ... or consider extending
(define usb-gpg-tools
  (operating-system
    ;; (inherit installation-os)
    ;; TODO is there a better starting point?

    (host-name "usbgpgtool")
    (timezone "America/New_York")
    (locale "en_us.UTF-8")

    ;; if not using the installation-os, this is going to be different
    ;; TODO: (bootloader...)
    ;; TODO: (file-systems...)

    (kernel-arguments '("modprobe.blacklist=radeon" ;; TODO double-check
                        ;; "quiet" ;; .....
                        ;; "net.iframes=0"
                        ))

    (packages
     (cons* (list exfat-utils
                  fuse-exfat
                  git
                  ;; curl
                  stow
                  vim
                  emacs-no-x-toolkit
                  pcsc-lite
                  screen
                  gnupg
                  )
            %base-packages))
            ;; (operating-system-packages installation-os)

;;;  TODO check essential-services
;;;  TODO check services
;;;  TODO check pam-services

    (services
     (append (cons*
              (service pcscd-service-type)
              %base-services
              )))))

;; usb-gpg-tools
