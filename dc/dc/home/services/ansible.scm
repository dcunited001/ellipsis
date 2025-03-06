;;; Copyright © 2025 David Conner <aionfork@gmail.com>

(define-module (dc home services ansible)
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
  #:export (ansible-service-type))

;;; Commentary:
;;;
;;; This module provides a service definition for the ansible service.
;;;
;;; Code:

;;; Packages

;; (use-package-modules ...)
(define (add-ansible-packages config)
  (list))

;;; XDG Files

(define (add-ansible-xdg-files config)
  (list
   `("ansible"
     ,(local-file (string-append %files-directory "/.config/ansible")
                  #:recursive? #t))))

;;; Files

(define (add-ansible-files config)
  (list
   `("ansible"
     ,(local-file (string-append %files-directory "/.config/ansible")
                  #:recursive? #t))))

;;; Guix Home Service

(define ansible-service-type
  (service-type
   (name 'dc-home-ansible)
   (extensions
    (list
     (service-extension home-profile-service-type
                        add-ansible-packages)
     (service-extension home-xdg-configuration-files-service-type
                        add-ansible-xdg-files)
     ))
   (default-value '())
   (description "Runs ansible")))

;;; ansible.scm ends here
