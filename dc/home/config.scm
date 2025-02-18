;;; Copyright © 2025 David Conner <aionfork@gmail.com>

(define-module (dc home config)
  #:use-module (ice-9 format)
  #:use-module (srfi srfi-1)
  #:use-module (guix channels)

  #:export (%home
            %dotfiles-directory
            %files-directory))

(define %home
  (and=> (getenv "HOME")
         (lambda (home)
           home)))

(define %dotfiles-directory
  (string-append %home "/.dotfiles" "/.gh"))

(define %files-directory
  (string-append %home "/.dotfiles" "/.gh/f"))

(define %data-directory "/data")

;;; config.scm ends here
