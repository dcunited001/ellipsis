;;; Copyright © 2026 David Conner <aionfork@gmail.com>
(define-module (dc hosts leto)
  #:use-module (srfi srfi-1)
  #:use-module (guix gexp)
  #:use-module (guix channels)
  #:use-module (gnu)
  #:use-module (gnu system)
  #:use-module (gnu system accounts)
  #:use-module (gnu system nss)
  #:use-module (gnu system privilege)
  #:use-module (gnu system setuid)
  #:use-module (dc services security-token)
  #:use-module (dc system common)
  #:use-module (dc hosts common)

  #:use-module (json)
  #:use-module (json builder)

  ;; #:use-module (nongnu packages linux)
  ;; #:use-module (nongnu system linux-initrd)

  ;; #:use-module (nongnu services vpn)
  #:use-module (nongnu packages vpn))

;;; Commentary:
;;;
;;; This module provides a system definition
;;;
;;; Code:

(use-service-modules guix admin sysctl pm avahi dbus cups linux mcron
                     networking ssh docker virtualization containers)

(use-package-modules nfs certs shells ssh tls gnupg security-token acl bash
                     emacs emacs-xyz networking libusb linux file-systems
                     version-control package-management rsync cryptsetup
                     hardware guile vim golang golang-crypto containers)

(define leto-kernel-modules (list))

(define-public %leto-channels
  (list
   (channel
     (name 'nonguix)
     (url "https://gitlab.com/nonguix/nonguix")
     (branch "master"))
   %default-channels))

(define unattended-upgrade-config
  (unattended-upgrade-configuration
    (inherit dc-hosts-unattended-upgrade-defaults)
    (channels #~(@ (dc hosts leto) %leto-channels))))

(define-public hosts-leto-foo "foo")

(define-public leto-network-manager-configuration
  (network-manager-configuration))

;; TODO: adjust dc-hosts-tty-services after append to %base-services
(define-public leto-services
  (append
   dc-smartcard-services
   (list
    (service dc-hosts-common-service-type)
    (service network-manager-service-type leto-network-manager-configuration)
    (service unattended-upgrade-service-type unattended-upgrade-config))))

;;; leto.scm ends here
