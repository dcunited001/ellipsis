;;; Copyright © 2025 David Conner <aionfork@gmail.com>

(define-module (dc home services screen)
  #:use-module (dc home common)
  #:use-module (dc home config)

  #:use-module (gnu home services)
  #:use-module (gnu home services shells)
  #:use-module (gnu packages)
  #:use-module (gnu packages screen)
  #:use-module (gnu services base)
  #:use-module (gnu services configuration)
  #:use-module (gnu services shepherd)
  #:use-module (gnu services)
  #:use-module (gnu)

  #:use-module (guix gexp)
  #:use-module (srfi srfi-1)
  #:export (screen-service-type))

;;; Commentary:
;;;
;;; This module provides a service definition for the screen service.
;;;
;;; Code:

;;; Packages

;; (use-package-modules ...)
(define (add-screen-packages config)
  (list screen))

;;; XDG Files

(define (add-screen-files config)
  (list
   `(".screenrc"
     ,(local-file (string-append %files-directory "/.screenrc") "screenrc"))))

;;; Shell

(define add-shell-aliases
  '(("screen" . "screen -h 2000")))

(define (add-bash-configuration config)
  (home-bash-extension
    (aliases add-screen-aliases)))

;;; Guix Home Service

(define screen-service-type
  (service-type
    (name 'dc-home-screen)
    (extensions
     (list
      (service-extension home-profile-service-type add-screen-packages)
      (service-extension home-bash-service-type add-bash-configuration)
      (service-extension home-files-service-type add-screen-files)))
    (default-value '())
    (description "Configures Screen")))

;;; screen.scm ends here
