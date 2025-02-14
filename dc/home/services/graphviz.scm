;;; Copyright © 2025 David Conner <aionfork@gmail.com>

(define-module (dc home services graphviz)

  #:use-module (gnu home services)
  #:use-module (gnu services)
  #:use-module (gnu services base)
  #:use-module (gnu services configuration)
  #:use-module (gnu services shepherd)
  #:use-module (gnu packages)
  #:use-module (gnu)

  #:use-module (guix gexp)
  #:use-module (srfi srfi-1)
  #:export (graphviz-service-type))

;;; Commentary:
;;;
;;; This module provides a service definition for the graphviz service.
;;;
;;; Code:
(use-package-modules graphviz)

(define graphviz-service-type
  (service-type
   (name 'dc-home-graphviz)
   (extensions
    (list
     (service-extension home-profile-service-type (list graphviz))
     (service-extension home-environment-variables-service-type
                        '(("GRAPHVIZ_DOT" . "$HOME/.guix-profile/bin/dot")))))
   (default-value '())
   (description "Runs graphviz")))

;;; graphviz.scm ends here
