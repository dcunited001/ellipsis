;;; Copyright © 2025 David Conner <aionfork@gmail.com>
(define-module (ellipsis services security-token)
  ;;#:use-module (ellipsis config)
  #:use-module (gnu home services)

  #:use-module (gnu)
  #:use-module (gnu packages)
  #:use-module (gnu packages security-token)
  #:use-module (gnu services base)
  #:use-module (gnu services configuration)
  #:use-module (gnu services shepherd)
  #:use-module (gnu services)

  #:use-module (ellipsis packages golang-crypto)
  #:use-module (ellipsis packages tls)

  #:use-module (guix gexp)
  #:use-module (guix records)
  #:use-module (srfi srfi-1)
  #:export (yubikey-udev-rules))

;;; Commentary:
;;;
;;; This module provides a service definition for system services related to
;;; security tokens.


;;;
;;; yubikey-udev-rules
;;;

(define-public (yubikey-udev-rules)
  ;; needs plugdev, but warns if multiple instantiations create using #:groups
  (list
   (udev-rules-service 'fido2 libfido2)
   (udev-rules-service 'u2f libu2f-host)
   (udev-rules-service 'yubikey yubikey-personalization)))


;;; security-token.scm ends here
