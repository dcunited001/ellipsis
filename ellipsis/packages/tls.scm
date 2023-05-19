(define-module (ellipsis packages tls)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix gexp)
  #:use-module (guix utils)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system copy)

  #:use-module (gnu packages base)
  #:use-module (gnu packages golang)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages security-token)
  ;; #:use-module (gnu packages gcc)

  #:use-module (srfi srfi-1))

;; gnu-build-system phases:
;; set-SOURCE-DATE-EPOCH set-paths install-locale unpack
;; bootstrap
;; patch-user-bin-file
;; patch-source-shebangs configure patch-generated-file-shebangs
;; build check install
;; patch-shebangs strip
;; validate-runpath
;; validate-documentation-location
;; delete-info-dir-file
;; patch-dot-desktop-files
;; make-dynamic-linker-cache
;; install-license-files
;; reset-gzip-timestamps
;; compress-documentation
;;
;; simplified:
;; set-paths
;; unpack
;; configure
;; build
;; check
;; install
;; strip
;; make-dynamic-linker-cache
;; install-license-files
;; compress-documenation

;; cgo: gccgo
;; ccid
;; pcsc-lite
;; openssl
;; this requires building step-cli "using CGO"
;; make bootstrap && make build GOFLAGS=""



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
(define-public step-cli
  (package
    (name "step-cli")
    (version "0.24.4")
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
    (synopsis "A zero trust swiss army knife for working with X509, OAuth, JWT, OATH, OTP, etc")
    (description "step is an easy-to-use CLI tool for building, operating, and automating Public Key Infrastructure (PKI) systems and workflows. It's the client counterpart to the step-ca online Certificate Authority (CA). You can use it for many common crypto and X.509 operations—either independently, or with an online CA.")
    (license license:asl2.0)))

;; TODO: remove command/certificate/remote_test.go (connects to remote server)
;; TODO: ld flags?

(define-public step-cli-bin
  (package
    (name "step-cli-bin")
    (version "0.24.4")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/smallstep/cli/releases/download/"
                    "v" version "/step_linux_" version "_amd64.tar.gz"))
              (sha256
               (base32
                "12amxqcln1h20hfibvkawzsx44im552365djvwhd19f3sr1hbqfi"))))
    (build-system copy-build-system)
    ;; (arguments
    ;;  )
    (inputs
     (list coreutils pcsc-lite))
    (home-page "https://smallstep.com/cli/")
    (synopsis "(prebuilt) A zero trust swiss army knife for working with X509, OAuth, JWT, OATH, OTP, etc")
    (description "step is an easy-to-use CLI tool for building, operating, and automating Public Key Infrastructure (PKI) systems and workflows. It's the client counterpart to the step-ca online Certificate Authority (CA). You can use it for many common crypto and X.509 operations—either independently, or with an online CA.")
    (license license:asl2.0)))

(define-public step-ca-bin
  (package
    (name "step-ca-bin")
    (version "0.24.2")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/smallstep/certificates/releases/download/"
                    "v" version "/step-ca_linux_" version "_amd64.tar.gz"))
              (sha256
               (base32
                "08d4803795kl78qnscysimr875afsvcj578zqhmy0dfwid9rg58h"))))
    (build-system copy-build-system)
    ;; (arguments
    ;;  )
    (inputs
     (list coreutils pcsc-lite))
    (home-page "https://smallstep.com/certificates/")
    (synopsis "(prebuilt) Open-Source Certificate Authority & PKI Toolkit")
    (description "A private certificate authority (X.509 & SSH) & ACME server for secure automated certificate management, so you can use TLS everywhere & SSO for SSH.")
    (license license:asl2.0)))
