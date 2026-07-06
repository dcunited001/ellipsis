(define-module (dc packages tls)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix gexp)
  #:use-module (guix utils)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix packages)

  #:use-module (guix build-system gnu)
  #:use-module (guix build-system copy)
  #:use-module (nonguix build-system binary)

  #:use-module (gnu packages base)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages commencement)
  #:use-module (gnu packages golang)

  #:use-module (gnu packages tls)
  #:use-module (gnu packages security-token)
  ;; #:use-module (gnu packages gcc)

  #:use-module (srfi srfi-1))

;; (define-public step-ca
;;   (package
;;     (name "step-ca")
;;     (version "0.21.0")
;;     (source (origin
;;               (method url-fetch)
;;               (uri (string-append
;;                     "https://smallstep/certificates/releases/download/"
;;                     "v" version "/step-ca_" version ".tar.gz"))
;;               (sha256
;;                (base32
;;                 "61ea96696f139fac0c87f957a84a1f1dc74e58f4d7f1720d192ab454a8a589c0"))))
;;     (build-system gnu-build-system)
;;     (arguments)
;;     (inputs
;;      (list coreutils pcsc-lite))
;;     (propagated-inputs
;;      ;; arch lists this as an optional dep
;;      (list step-cli))
;;     (native-inputs)
;;     (home-page "https://smallstep.com/certificates/")
;;     (synopsis "Open-Source Certificate Authority & PKI Toolkit")
;;     (description "A private certificate authority (X.509 & SSH) & ACME server for secure automated certificate management, so you can use TLS everywhere & SSO for SSH.")
;;     (license license:asl20)))

;; go depends on gccgo
;; TODO: maybe finish trying to package this. it doesn't build
(define-public step-cli
  (package
    (name "step-cli")
    (version "0.30.6")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/smallstep/cli/releases/download/"
                    "v" version "/step_" version ".tar.gz"))
              (sha256
               (base32
                "120ywgrq6ifbb68h6wml95llfpsabfgfl70s2y2452pypmybvjqd"))))
    (build-system gnu-build-system)
    ;; (arguments
    ;;  )
    (inputs
     (list coreutils pcsc-lite))
    (native-inputs
     (list go))
    (home-page "https://smallstep.com/cli/")
    (synopsis
     "A zero trust swiss army knife for working with X509, OAuth, JWT, OATH, OTP, etc")
    (description
     "step is an easy-to-use CLI tool for building, operating, and automating Public Key Infrastructure (PKI) systems and workflows. It's the client counterpart to the step-ca online Certificate Authority (CA). You can use it for many common crypto and X.509 operations—either independently, or with an online CA.")
    (license license:asl2.0)))

;; TODO: remove command/certificate/remote_test.go (connects to remote server)
;; TODO: ld flags?

(define-public step-cli-bin
  (package
    (name "step-cli-bin")
    (version "0.30.6")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/smallstep/cli/releases/download/"
                    "v" version "/step_linux_" version "_amd64.tar.gz"))
              (sha256
               (base32
                "1phnb16ksi8qvvghy5skw1pbrc41kak42a8g9ar999l0z32msjp4"))))
    (build-system copy-build-system)
    (inputs
     (list coreutils pcsc-lite))
    (home-page "https://smallstep.com/cli/")
    (synopsis
     "(prebuilt) A zero trust swiss army knife for working with X509, OAuth, JWT, OATH, OTP, etc")
    (description
     "step is an easy-to-use CLI tool for building, operating, and automating Public Key Infrastructure (PKI) systems and workflows. It's the client counterpart to the step-ca online Certificate Authority (CA). You can use it for many common crypto and X.509 operations—either independently, or with an online CA.")
    (license license:asl2.0)))

(define-public step-ca-bin
  (let ((version-stem "0.30.2")
        (version-release "0.30.2"))
    (package
      (name "step-ca-bin")
      (version version-release)
      (source (origin
                (method url-fetch)
                (uri (string-append
                      "https://github.com/smallstep/certificates/releases/download/"
                      "v" version-stem "/step-ca_linux_" version-release
                      "_amd64.tar.gz"))
                (sha256
                 (base32
                  "0wvxqdy088sbsdqhd3dlbwqmfa3fz8lddqlhz39z5qxgbdwiarhj"))))
      (build-system copy-build-system)
      (arguments
       (list
        #:install-plan ''(("." "bin/" #:include-regexp ("step-ca$")))))
      (inputs
       (list coreutils pcsc-lite))
      (home-page "https://smallstep.com/certificates/")
      (synopsis "(prebuilt) Open-Source Certificate Authority & PKI Toolkit")
      (description
       "A private certificate authority (X.509 & SSH) & ACME server for secure automated certificate management, so you can use TLS everywhere & SSO for SSH.")
      (license license:asl2.0))))

(define-public step-kms-plugin-bin
  (package
    (name "step-kms-plugin-bin")
    (version "0.17.0")
    ;; (version "0.12.3-rc19")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/smallstep/step-kms-plugin/releases/download/"
                    "v" version "/step-kms-plugin_" version
                    "_linux_amd64.tar.gz"))
              (sha256
               (base32
                "1cz7rczx9iqq6r35q7mffr86724nnm8xy4dzfm1rr9gamg7g0kfx"))))
    (build-system binary-build-system)
    (inputs `((,gcc "lib")
              ,gcc-toolchain
              ,softhsm
              ;; ,pcsc-lite
              ;; ,glibc
              ))

    ;; causes pcsc server (4:5) client (4:4) mismatch
    ;; - using patchelf against guix's pcsc-lite 2.0.0
    ;; - while running nixos pcsc-lite 2.3.0
    ;; - this is more of a host-system incompatibility
    (propagated-inputs (list pcsc-lite))
    (arguments
     (list
      #:patchelf-plan ''(("step-kms-plugin"
                          ("pcsc-lite"
                           "gcc"
                           "gcc-toolchain"
                           ;; "glibc"
                           ;; "libc"
                           )))
      #:install-plan ''(("." "bin/" #:include-regexp ("step-kms-plugin$")))))

    ;; TODO: update details ...
    ;; segfault, no debugging symbols

    (home-page "https://smallstep.com/certificates/")
    (synopsis "(prebuilt) Open-Source Certificate Authority & PKI Toolkit")
    (description
     "A private certificate authority (X.509 & SSH) & ACME server for secure automated certificate management, so you can use TLS everywhere & SSO for SSH.")
    (license license:asl2.0)))
