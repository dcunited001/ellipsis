(define-module (ellipsis packages golang-crypto)
  #:use-module ((guix licenses) #:prefix license:)

  #:use-module (guix gexp)
  #:use-module (guix utils)
  #:use-module (guix build utils)

  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix packages)

  #:use-module (guix build-system gnu)
  #:use-module (guix build-system go)
  #:use-module (guix build-system copy)
  #:use-module (nonguix build-system binary)

  #:use-module (gnu)
  #:use-module (gnu packages)

  #:use-module (gnu packages golang)

  #:use-module (srfi srfi-1))

(use-package-modules base gcc commencement)

(define-public age-plugin-tpm-bin
  (let* ((platform "linux-amd64"))
    (package
      (name "age-plugin-tpm-bin")
      (version "0.3.0")
      (source (origin
                (method url-fetch)
                (uri (string-append
                      "https://github.com/Foxboron/age-plugin-tpm/releases/download/v"
                      version "/" "age-plugin-tpm" "-v" version "-" platform
                      ".tar.gz"))
                (sha256
                 (base32
                  "0x1kx2c77nkg7ivzlrvvzjwg7fp22kpqp597yrpw7f1y926dzr2j"))))
      (build-system binary-build-system)
      (inputs `((,gcc "lib")
                ,gcc-toolchain))
      (arguments
       (list
        #:install-plan #~'(("." "bin/" #:include ("age-plugin-tpm")))))
      (home-page "https://github.com/Foxboron/age-plugin-tpm")
      (synopsis "")
      (description "")
      (license license:expat))))

(define-public ssh-tpm-agent-bin
  (let ((platform "linux-amd64"))
    (package
      (name "ssh-tpm-agent-bin")
      (version "0.8.0")
      (source
       (origin
         (method url-fetch)
         (uri (string-append
               "https://github.com/Foxboron/ssh-tpm-agent/releases/download/v"
               version "/" "ssh-tpm-agent" "-v" version "-" platform ".tar.gz"))
         (sha256
          (base32
           "137i088jd4s13vyrv9mlqfaf3x2vzl5xr070xzdsma74f7qw9sv1"))))
      (build-system binary-build-system)
      (arguments
       (list
        #:install-plan #~'(("." "bin/" #:include-regexp ("ssh-tpm-")))))
      (inputs
       (list coreutils pcsc-lite))
      (native-inputs
       (list go))
      (home-page "https://smallstep.com/cli/")
      (synopsis
       "A zero trust swiss army knife for working with X509, OAuth, JWT, OATH, OTP, etc")
      (description
       "step is an easy-to-use CLI tool for building, operating, and automating Public Key Infrastructure (PKI) systems and workflows. It's the client counterpart to the step-ca online Certificate Authority (CA). You can use it for many common crypto and X.509 operations—either independently, or with an online CA.")
      (license license:asl2.0))))
