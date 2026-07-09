;;; Copyright © 2026 David Conner <aionfork@gmail.com>
(define-module (dc packages bootloaders)
  #:use-module (dc)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix gexp)
  #:use-module (guix utils)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build gnu-build-system)
  #:use-module (guix packages)

  #:use-module (gnu)
  #:use-module (gnu packages)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bootloaders)

  #:use-module (guix gexp)
  #:use-module (srfi srfi-1)
  #:export (ipxe-test))

;;; Commentary:
;;;
;;; This module provides packages for bootloaders.
;;;
;;;
;;; Code:

;;; Packages

(define ipxe-testfile (plain-file "boot.ipxe" "#!ipxe
dhcp
chain http://muh.netboot.xyz"))

(define ipxe-testscript "#!ipxe
dhcp
chain http://muh.netboot.xyz")

(define ipxe-test
  (package
    (inherit ipxe)
    (name "ipxe-test")
    (version "1.21.1-4.969ce2c-test-0")
    (source (origin
              (inherit (package-source ipxe))
              (snippet
               #~(begin (copy-file #$ipxe-testfile "src/boot.ipxe")))))
    (arguments
     (substitute-keyword-arguments (package-arguments ipxe)
       ((#:modules modules)
        `((ice-9 match) ,@modules))
       ((#:make-flags flags)
        #~(append '("EMBED=boot.ipxe") #$flags))))))
