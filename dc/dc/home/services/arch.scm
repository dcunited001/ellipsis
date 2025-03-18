;;; Copyright © 2025 David Conner <aionfork@gmail.com>

(define-module (dc home services arch)
  #:use-module (dc home common)
  #:use-module (dc home config)

  #:use-module (gnu home services)
  #:use-module (gnu home services mcron)
  #:use-module (gnu packages)
  #:use-module (gnu services base)
  #:use-module (gnu services configuration)
  #:use-module (gnu services shepherd)
  #:use-module (gnu services)
  #:use-module (gnu)

  #:use-module (guix gexp)
  #:use-module (srfi srfi-1)
  #:export (arch-service-type))

;;; Commentary:
;;;
;;; This module provides a service definition for the arch service.
;;;
;;; Code:


;; TODO bash ext that sources a ~/.config/sh/{profile,rc}.d/arch.sh (if exists)
;;
;; - these profile/rc scripts could be de/tangled into an adjacent org file ...?
;; - this enables functionality to be added without `guix reconfigure`
;;   - home-dotfiles-service-type may accomplish similar functionality'

;; TODO mcron service to actually fucking update arch automagically

;;; Packages

;; (use-package-modules ...)
(define (add-arch-packages config)
  (list))

;;; Shell

(define add-shell-aliases
  '(("pac_noreason" . "pacman -Qtdq")
    ;; ("pac_rm" . "pacman -Rdd")
    ("garuda_up" . "/usr/bin/update")   ; garuda update
    ("pac_rip"
     . (string-join
        '("expac --timefmt='\\''%Y-%m-%d %T'\\'' '\\''%l\\t%n %v'\\''"
          "sort | tail -200 | nl")
        "|"))))

;;; Files

(define (add-arch-files config)
  ;; maybe use a map
  (list
   `("bin/yay"
     ,(local-file (string-append %files-directory "/.config/arch") #:recursive?
                  #t))))

;;; mcron

;; (define (add-arch-mcron-jobs config)
;;   (home-mcron-configuration))

;; mcron, jobs, log?, log-file, log-format, date-format, home-service?

;;; Guix Home Service

(define arch-service-type
  (service-type
   (name 'dc-home-arch)
   (extensions
    (list
     (service-extension home-profile-service-type
                        add-arch-packages)
     (service-extension home-files-service-type
                        add-arch-files)))
   (default-value '())
   (description "Runs arch")))

;;; arch.scm ends here
