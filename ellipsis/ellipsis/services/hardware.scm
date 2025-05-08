;;; Copyright © 2025 David Conner <aionfork@gmail.com>
(define-module (ellipsis services hardware)
  ;;#:use-module (ellipsis common)
  ;;#:use-module (ellipsis config)

  #:use-module (gnu packages hardware)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages)
  #:use-module (gnu services base)
  #:use-module (gnu services configuration)
  #:use-module (gnu services linux)
  #:use-module (gnu services shepherd)
  #:use-module (gnu services)
  #:use-module (gnu)
  #:use-module (guix records)

  #:use-module (guix gexp)
  #:use-module (srfi srfi-1)
  #:export (ddcci-driver-service-type))


;;;
;;; ddcci-driver-service
;;; 

;; TODO test
;;
;; module repo: https://gitlab.com/ddcci-driver-linux/ddcci-driver-linux
;;
;; AUR discussion about race conditions:
;;
;; https://aur.archlinux.org/packages/ddcci-driver-linux-dkms
;;
;; example in guix manual:
;; https://guix.gnu.org/manual/devel/en/guix.html#index-kernel_002dmodule_002dloader_002dservice_002dtype:

(define %ddcci-driver-conf
  (plain-file "ddcci-driver.conf"
              "options ddcci delay=120
options ddcci-backlight delay=120"))

(define-record-type* <ddcci-driver-configuration>
  ddcci-driver-configuration make-ddcci-driver-configuration
  ddcci-driver-configuration?
  (package ddcci-driver-configuration-package
           (default ddcci-driver-linux))
  (modprobe-conf ddcci-driver-configuration-modprobe-conf
                 (default %ddcci-driver-conf)))

(define (ddcci-driver-etc-service config)
  (list
   `("/etc/modprobe.d/ddcci-driver.conf"
     (ddcci-driver-configuration-modprobe-conf config))))

(define (ddcci-driver-profile-service config)
  (list (ddcci-driver-configuration-package config)))

(define ddcci-driver-service-type
  (service-type
   (name 'hardware)
   (extensions
    (list (service-extension kernel-module-loader-service-type
                             '("ddcci" "ddcci_backlight"))
          (service-extension profile-service-type
                             ddcci-driver-profile-service)
          (service-extension etc-service-type
                             ddcci-driver-etc-service)))
   (default-value (ddcci-driver-configuration))
   (description
    "Load and configure ddcci and ddcci-backlight modules.")))

;;; hardware.scm ends here
