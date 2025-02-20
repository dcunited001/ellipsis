(define-module (ellipsis packages groff)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix gexp)
  #:use-module (guix utils)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix packages)

  #:use-module (guix build utils)

  #:use-module (guix build-system gnu)
  #:use-module (guix build-system copy)
  #:use-module (guix build-system perl)

  #:use-module (gnu packages)
  #:use-module (gnu packages base)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages libusb)
  #:use-module (gnu packages openldap)
  ;; #:use-module (gnu packages hardware) ;; for tpm2-tss

  #:use-module (ellipsis packages)
  #:use-module (ellipsis packages perl)
  #:use-module (srfi srfi-1))

(define-public grofftl
  (let* ((commit "1f524383e898413a8c2085bb5518cead56824251")
         (revision "0"))
    (package
      (name "grofftl")
      (version (git-version "0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/randoragon/groffhl")
                      (commit version)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "0y279pcgs3jrsi9vzx086xhz9jbz23dqqijp4agygc9ackp9sxy5"))))
      (build-system gnu-build-system)
      (native-inputs (list autoconf
                           automake
                           libtool
                           pkg-config))
      ;; (inputs (list libgee json-glib libxkbcommon))
      (home-page "https://github.com/randoragon/groffhl")
      (synopsis "Groff Code Syntax Highlighting")
      (description
       "groffhl is a small program for converting Linux @code{truecolor} escape
sequences into native groff format. You can then take the output and
paste it into another groff document.")
      (license license:expat))))
