(define-module (ellipsis packages emacs-xyz)
  #:use-module ((guix licenses) #:prefix license:)

  #:use-module (guix gexp)
  #:use-module (guix utils)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix build-system emacs)

  #:use-module (gnu packages base)

  #:use-module (srfi srfi-1)
  )


;; https://github.com/jobbflykt/x509-mode
;; https://guix.gnu.org/manual/en/guix.html#emacs_002dbuild_002dsystem
;;
;; melpa recipe:
;; (x509-mode :repo "jobbflykt/x509-mode"
;;            :fetcher github
;;            :files (:defaults "*.txt"))
