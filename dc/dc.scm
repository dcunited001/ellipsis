;;; Copyright © 2025 David Conner <aionfork@gmail.com>

(define-module (dc)
  #:export (%dcroot
            %dotfiles-directory
            %files-directory))

(define %dcroot
  (getcwd))

(define %dotfiles-directory
  (string-append %dcroot "../gh"))

(define %files-directory
  (string-append %dcroot "../gh/f"))

(define %data-directory "/data")

;;; dc.scm ends here
