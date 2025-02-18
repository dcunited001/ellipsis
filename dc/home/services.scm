;;; Copyright © 2025 David Conner <aionfork@gmail.com>

(define-module (dc home services)
  #:use-module (dc home commmon)

  #:use-module (gnu home services)
  #:use-module (gnu packages)
  #:use-module (gnu services base)
  #:use-module (gnu services configuration)
  #:use-module (gnu services shepherd)
  #:use-module (gnu services)
  #:use-module (gnu)

  #:use-module (guix gexp)
  #:use-module (srfi srfi-1)
  #:export (dc-zathura-config))

;;; Commentary:
;;;
;;; This module provides a service definition for the services service.
;;;
;;; Code:

;;; Zathura
;;
;; TODO: HOME: zathura: after deciding on a PDF reader, extend XDG mime/desktop
(define dc-zathura-config
  (simple-service
   'dc-zathura-config
   home-xdg-configuration-files-service-type
   (list `("zathura/zathurarc" ,(local-file (string-append %files-directory "/.config/zathura/zathurarc"))))))
