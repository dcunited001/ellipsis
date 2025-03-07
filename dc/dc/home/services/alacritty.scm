;;; Copyright © 2025 David Conner <aionfork@gmail.com>

(define-module (dc home services alacritty)
  #:use-module (dc home config)
  #:use-module (gnu home services xdg)
  #:use-module (gnu home services)
  #:use-module (gnu packages terminals)
  #:use-module (gnu services base)
  #:use-module (gnu services)
  #:use-module (gnu)
  #:use-module (guix gexp)
  #:use-module (rde serializers ini)

  #:use-module (srfi srfi-1)

  #:export (alacritty-service-type
            dc-alacritty-xdg-files))

;;; Commentary:
;;;
;;; This module provides a service definition for the alacritty service.
;;;
;;; Code:

(define (dc-alacritty-xdg-files config)
  (list
   `("alacritty/alacritty.toml"
     ,(local-file
       (string-append %files-directory "/.config/alacritty/alacritty.toml")))
   `("alacritty/alacritty-clean.toml"
     ,(local-file
       (string-append %files-directory "/.config/alacritty/alacritty.clean.toml")))))

(define (add-alacritty-packages config)
  (list alacritty))

;; defining as a procedure causes the syntax rule to shit the bed
;;
;; (define (alacritty-service-type xdg-files) ...)

(define (alacritty-service-type xdg-files)
  (service-type
   (name 'dc-home-alacritty)
   (extensions
    (list
     ;; packages
     (service-extension home-profile-service-type add-alacritty-packages)
     (service-extension home-xdg-configuration-files-service-type xdg-files)))
   ;; (default-value '())
   (description "Runs alacritty")))

;;; alacritty.scm ends here
