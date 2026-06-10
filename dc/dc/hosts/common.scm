;;; Copyright © 2026 David Conner <aionfork@gmail.com>
(define-module (dc hosts common)
  #:use-module (srfi srfi-1)
  #:use-module (guix gexp)
  #:use-module (guix channels)
  #:use-module (gnu)
  #:use-module (gnu system)
  #:use-module (gnu system accounts)
  #:use-module (gnu system nss)
  #:use-module (gnu system privilege)
  #:use-module (gnu system setuid))

;; #:use-module (nongnu packages linux)
;; #:use-module (nongnu system linux-initrd)
;; #:use-module (nongnu services vpn)
;; #:use-module (nongnu packages vpn)

;;; Commentary:
;;;
;;; This module provides a service definition for the common service.
;;;
;;; Code:

(use-service-modules guix admin sysctl pm avahi dbus cups linux mcron shepherd
                     auditd networking ssh docker virtualization containers
                     security-token)

(use-package-modules security-token gnupg libusb)

(define-public (dc-hosts-subid-start id)
  (+ (expt 2 16) (expt 10 8) (* 100000 id)))

;; (dc-hosts-subid-start 0)    ;; => 100065536
;; (dc-hosts-subid-start 1)    ;; => 100165536
;; (dc-hosts-subid-start 45)   ;; => 104565536
;; (dc-hosts-subid-start 450)  ;; => 145065536
;; (dc-hosts-subid-start 950)  ;; => 195065536
;; (dc-hosts-subid-start 1000) ;; => 200065536
;; (dc-hosts-subid-start 1001) ;; => 200165536

(define-public (dc-hosts-subid-range name uid)
  (subid-range (name name) (count (expt 2 16))
               (start (dc-hosts-subid-start uid))))

(define dc-hosts-ntp-service
  (service ntp-service-type
           (ntp-configuration
            (servers
             (list (ntp-server
                    (type 'pool)
                    (address "1.us.pool.ntp.org")
                    (options '("iburst")))
                   (ntp-server
                    (type 'pool)
                    (address "2.us.pool.ntp.org")
                    (options '("iburst")))
                   (ntp-server
                    (type 'pool)
                    (address "3.us.pool.ntp.org")
                    (options '("iburst"))))))))

;; (define-public %el-profile-pkgs-age
;;   (list age age-keygen age-plugin-tpm-bin age-plugin-yubikey-bin))

;; (define-public %el-profile-pkgs-tls
;;   ;; desec-certbot-hook
;;   (list openssh openssl le-certs gnutls certdata2pem))

;; hidapi: HID Devices for FIDO/OTP
(define pkgs-smartcard
  (list opensc pinentry-tty hidapi libu2f-host libfido2))

(define pkgs-yubikey
  (list yubico-piv-tool yubikey-personalization python-yubikey-manager))

(define-public dc-hosts-smartcard-service-type
  (service-type
   (name 'dc-hosts-smartcard)
   (extensions
    (list
     ;; pcsc-lite, ccid provided by service/activation
     (service-extension shepherd-root-service-type (service pcscd-service-type))
     (service-extension udev-service-type
                        (udev-rules-service 'u2f libu2f-host))
     (service-extension udev-service-type
                        (udev-rules-service 'fido2 libfido2 #:groups
                                            '("plugdev")))
     (service-extension udev-service-type
                        (udev-rules-service 'yubikey yubikey-personalization))
     (service-extension profile-service-type
                        (lambda (config) (append pkgs-yubikey pkgs-smartcard)))))
   (default-value '())
   (description "Sets up some common services")))

(define-public (dc-hosts-tty-services)
  (modify-services %base-services
    (agetty-service-type
     config => (agetty-configuration
                (inherit config)
                (login-pause? #t)
                (timeout 30)))

    (mingetty-service-type
     config => (mingetty-configuration
                (inherit config)
                (login-pause? #t)))))

(define-public dc-hosts-common-service-type
  (service-type
   (name 'dc-hosts-common)
   (extensions
    (list
     (service-extension shepherd-root-service-type dc-hosts-ntp-service)
     (service-extension shepherd-root-service-type
                        (service rasdaemon-service-type
                                 (rasdaemon-configuration (record? #t))))
     (service-extension shepherd-root-service-type
                        (service auditd-service-type))))
   (default-value '())
   (description "Sets up some common services")))

(define-public dc-hosts-unattended-upgrade-defaults
  (unattended-upgrade-configuration
   (schedule "30 2 * * 0")
   (system-expiration (* 6 7 24 3600))))

;;; common.scm ends here
