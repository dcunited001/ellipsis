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
  #:export (ipxe-test
            ipxe-test-debug))

;;; Commentary:
;;;
;;; This module provides packages for bootloaders.
;;;
;;;
;;; Code:

;;; Packages

(define ipxe-netboot-server "muh.netboot.homelab:8080")

;; something in this script or on my machine is forcing iPXE to attempt to
;; fetch autoexec.ipxe, i think as a last resort.

(define ipxe-testscript
  (format #f "#!ipxe
:menu
menu Please choose a boot option
item shell Start iPXE shell
item netboot Netboot
item exit Exit iPXE and proceed

:shell
echo Entering iPXE command line...
shell
goto menu

:netboot
dhcp
chain --autofree http://~A/tftp-server/netboot/x64_64/netboot.xyz.efi

:exit
exit
"
          ipxe-netboot-server))

(define ipxe-testfile
  (plain-file "boot.ipxe" ipxe-testscript))

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

;; i saw the debugger output once..... i also tried replacing the script with just
;; 
;; #!ipxe
;; shell

(define ipxe-test-debug
  (package
    (inherit ipxe-test)
    (name "ipxe-test-debug")
    (version "1.21.1-4.969ce2c-test-debug-0")
    (arguments
     (substitute-keyword-arguments (package-arguments ipxe-test)
       ((#:modules modules)
        `((ice-9 match) ,@modules))
       ((#:make-flags flags)
        #~(append '("DEBUG=intel,tftp,dhcp") #$flags))))))
