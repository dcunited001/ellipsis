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
      (synopsis "TPM 2.0 plugin for age")
      (description
       "age-plugin-tpm is a plugin for age clients like age and rage, which
enables files to be encrypted to age identities sealed by the TPM.")
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
      (home-page "https://github.com/Foxboron/ssh-agent-tpm")
      (synopsis "An ssh-agent that fetches keys from a TPM")
      (description "ssh-tpm-agent is a ssh-agent compatible agent that allows
keys to be created by the Trusted Platform Module (TPM) for authentication
towards ssh servers.

TPM sealed keys are private keys created inside the Trusted Platform
Module (TPM) and sealed in .tpm suffixed files. They are bound to the hardware
they are produced on and can't be transferred to other machines.

This allows you to utilize a native client instead of having to side load
existing PKCS11 libraries into the ssh-agent and/or ssh client.")
      (license license:expat))))
