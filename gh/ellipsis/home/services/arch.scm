(define-module (ellipsis home services arch)
  #:use-module (gnu home)
  #:use-module (srfi srfi-1)
  #:use-module (gnu packages)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu services)
  #:use-module (gnu home services)
  #:use-module (gnu home services shells)
  #:use-module (guix gexp))

(define-public arch-bash-configuration
  (home-bash-configuration
   (aliases '(("cleanup" . "sudo pacman -Rns `pacman -Qtdq`")
              ("fixpacman" . "sudo rm /var/lib/pacman/db.lck")
              ("rmpkg" . "sudo pacman -Rdd")
              ))))
