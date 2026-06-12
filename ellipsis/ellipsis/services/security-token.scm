;;; Copyright © 2025 David Conner <aionfork@gmail.com>
(define-module (ellipsis services security-token)
  ;;#:use-module (ellipsis config)
  #:use-module (gnu home services)

  #:use-module (gnu)
  #:use-module (gnu packages)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages libusb)
  #:use-module (gnu packages security-token)
  #:use-module (gnu services base)
  #:use-module (gnu services configuration)
  #:use-module (gnu services security-token)
  #:use-module (gnu services shepherd)
  #:use-module (gnu services)

  #:use-module (ellipsis packages golang-crypto)
  #:use-module (ellipsis packages security-token)
  #:use-module (ellipsis packages tls)

  #:use-module (guix gexp)
  #:use-module (guix records)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 pretty-print)
  #:export (yubikey-udev-rules))

;;; Commentary:
;;;
;;; This module provides a service definition for system services related to
;;; security tokens.


;;;
;;; yubikey-udev-rules
;;;

(define-public yubikey-udev-rules
  ;; needs plugdev, but warns if multiple instantiations create using #:groups
  (list
   (udev-rules-service 'fido2 libfido2 #:groups '("plugdev"))
   (udev-rules-service 'u2f libu2f-host)
   (udev-rules-service 'yubikey yubikey-personalization)))

;; hidapi: HID Devices for FIDO/OTP
(define pkgs-smartcard
  (list opensc pinentry-tty hidapi libu2f-host libfido2))

(define pkgs-yubikey
  (list yubico-piv-tool yubikey-personalization python-yubikey-manager))

(define svc-pcscd
  (service pcscd-service-type))

(define-public ellipsis-smartcard-service-type
  (service-type
   (name 'ellipsis-smartcard)
   (extensions
    (list

     ;; pcsc-lite, ccid provided by service/activation
     ;; (service-extension shepherd-root-service-type svc-pcscd)

     (service-extension udev-service-type (lambda (config) yubikey-udev-rules))
     (service-extension profile-service-type
                        (lambda (config) (append pkgs-yubikey pkgs-smartcard)))))
   (default-value '())
   (description "Sets up some common services")))

;;; security-token.scm ends here
