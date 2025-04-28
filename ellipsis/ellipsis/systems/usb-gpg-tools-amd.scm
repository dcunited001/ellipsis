;;; Module
(define-module (ellipsis systems usb-gpg-tools-amd)
  #:use-module (gnu)
  #:use-module (gnu system)

  #:use-module (ellipsis packages tls)
  #:use-module (ellipsis packages emacs-xyz)
  #:use-module (ellipsis packages password-utils)
  #:use-module (ellipsis systems usb-gpg-tools)

  #:export (usb-gpg-tool-amd)

  ;;NONFREE
  #:use-module (nongnu packages linux)
  #:use-module (nongnu system linux-initrd))

;; This system containes other nonfree software built from patched binaries,
;; but it includes a patched kernel from nonguix and includes
;; Intel/AMD microcode.

;;;; Image
(define usb-gpg-tools-amd
  (operating-system
    (inherit usb-gpg-tools)
    (host-name "usbgpgtoolamd")
    (timezone "America/New_York")
    (locale "en_US.UTF-8")

    ;; NONFREE
    (kernel linux)
    (firmware (cons* linux-firmware
                     amd-microcode
                     ;; realtek-firmware
                     %base-firmware))

    (kernel-arguments '("modprobe.blacklist=radeon"
                        ;; "quiet" ;; .....
                        ;; "net.iframes=0"
                        ))))

usb-gpg-tools-amd
