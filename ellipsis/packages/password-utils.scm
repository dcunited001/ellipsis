(define-module (ellipsis packages password-utils)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix gexp)
  #:use-module (guix utils)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix build-system gnu)

  #:use-module (gnu packages base)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages password-utils)

  #:use-module (srfi srfi-1))

(define-public shroud-nox
  (package
    (inherit shroud)
    (name "shroud-nox")
    (version "0.1.2")
    (inputs
     (list guile-2.2 gnupg))
    (synopsis "GnuPG-based secret manager (without xclip integration")
    (description "Shroud is a simple secret manager with a command line
interface.  The password database is stored as a Scheme s-expression and
encrypted with a GnuPG key.  Secrets consist of an arbitrary number of
key/value pairs, making Shroud suitable for more than just password storage." )
    (home-page "https://dthompson.us/projects/shroud.html")
    (license license:gpl3+)))
