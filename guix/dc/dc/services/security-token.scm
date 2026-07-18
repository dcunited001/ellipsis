;;; Copyright © 2025 David Conner <aionfork@gmail.com>
(define-module (dc services security-token)
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

  #:use-module (dc packages golang-crypto)
  #:use-module (dc packages security-token)
  #:use-module (dc packages tls)

  #:use-module (guix gexp)
  #:use-module (guix records)
  #:use-module (srfi srfi-1))

;;; Commentary:
;;;
;;; This module provides a service definition for system services related to
;;; security tokens.


;;;
;;; dc-smartcard-services
;;;

;; hidapi: HID Devices for FIDO/OTP
(define pkgs-smartcard
  (list opensc pinentry-tty hidapi libu2f-host libfido2))

(define pkgs-yubikey
  (list yubico-piv-tool yubikey-personalization python-yubikey-manager))

(define-public dc-smartcard-services
  (list
   (service pcscd-service-type)
   (udev-rules-service 'fido2 libfido2 #:groups '("plugdev"))
   (udev-rules-service 'u2f libu2f-host)
   (udev-rules-service 'yubikey yubikey-personalization #:groups '("yubikey"))
   (simple-service 'dc-smartcard-profile-service
                   profile-service-type
                   (append pkgs-yubikey pkgs-smartcard))))

;;; security-token.scm ends here
