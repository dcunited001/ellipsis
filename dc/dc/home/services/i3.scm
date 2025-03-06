;;; Copyright © 2025 David Conner <aionfork@gmail.com>

(define-module (dc home services i3)
  #:use-module (dc home common)
  #:use-module (dc home config)

  #:use-module (gnu home services)
  #:use-module (gnu packages)
  #:use-module (gnu services base)
  #:use-module (gnu services configuration)
  #:use-module (gnu services shepherd)
  #:use-module (gnu services)
  #:use-module (gnu)

  #:use-module (guix gexp)
  #:use-module (srfi srfi-1)
  #:export (i3-service-type))

;;; Commentary:
;;;
;;; This module provides a service definition for the i3 service.
;;;
;;; Code:

;;; Packages

(use-package-modules wm xdisorg)
(define (add-i3-packages config)
  (list i3-gaps i3status i3lock j4-dmenu-desktop))

;;; XDG Files

(define (add-i3-xdg-files config)
  (list
   `("i3"
     ,(local-file (string-append %files-directory "/.config/i3") #:recursive?
                  #t))))

;;; Shepherd Service

;;; Guix Home Service

(define i3-service-type
  (service-type
   (name 'dc-home-i3)
   (extensions
    (list
     (service-extension home-profile-service-type
                        add-i3-packages)
     (service-extension home-xdg-configuration-files-service-type
                        add-i3-xdg-files)))
   (default-value '())
   (description "Runs i3")))

;;; i3.scm ends here
