;;* Module: tokos
(define-module (tokos)
  #:use-module (base-system)
  #:use-module (guix gexp)
  #:use-module (gnu services)
  #:use-module (gnu services base)
  #:use-module (gnu system)
  #:use-module (gnu system uuid)
  #:use-module (gnu system file-system)
  #:use-module (gnu system mapped-devices)
  #:use-module (gnu packages firmware)
  #:use-module (nongnu packages linux))

;;** desktop-services
;; TODO: docker is no longer added as a service by default
(define %tokos-desktop-services
  (modify-services
   dc-desktop-services

   (guix-service-type config =>
                      (guix-configuration
                       (inherit config)
                       (substitute-urls
                        (append (list "https://substitutes.nonguix.org")
                                %default-substitute-urls))
                       (authorized-keys
                        (append (list (local-file "../nonguix.pub"))
                                %default-authorized-guix-keys))))
   ))

;;** operating-system
(operating-system
 (inherit base-operating-system)
 (hostname "tokos")

 (firmware (list linux-firmware
                 ;; has Broadcom BCM4331 wifi
                 openfwwf-firmware))


 ;; NOTE expect patch for:
 ;; - (mapped-devices ...)
 ;; - (file-systems ...)

 (services %tokos-desktop-services)
 (swap-devices (list (file-system-label "swap"))))
