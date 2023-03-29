;;* Module
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

;; networking is [probably] needed for loopback
;; (use-service-modules networking ssh security-token)
;; (use-package-modules wget screen password-utils vim emacs emacs-xyz)
;; (use-package-modules linux time mtools lsof file-systems disk version-control)
;; (use-package-modules ssh gnupg cryptsetup security-token tls certs libusb)

;; (define %my-user "dc")

 ;; (define %my-services
 ;;  (modify-services
 ;;      %base-services

 ;;    (agetty-service-type
 ;;     config => (agetty-configuration
 ;;                (inherit config)
 ;;                (login-pause? #t)
 ;;                (timeout 30)))

 ;;    (mingetty-service-type
 ;;     config => (mingetty-configuration
 ;;                (inherit config)
 ;;                ;; (auto-login %my-user)
 ;;                (login-pause? #t)))))


;;** Image
(define usb-gpg-tools-amd
  (operating-system
    (inherit usb-gpg-tools)
    (host-name "usbgpgtoolamd")
    (timezone "America/New_York")
    (locale "en_US.utf8")

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
