(define-module (ellipsis packages python-xyz)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix gexp)
  #:use-module (guix utils)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix build-system python)
  #:use-module (guix build-system pyproject)

  #:use-module (gnu packages)
  #:use-module (gnu packages python-xyz)

  #:use-module (srfi srfi-1)

  #:export (pyfoo))

(define pyfoo "pyfoo")
