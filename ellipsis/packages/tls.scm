(define-module (ellipsis packages tls)
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

(define-public sops
  (let* ((bin-platform "linux.amd64")
         (bin-version "3.8.1")
         (bin-name (string-append "sops-v" bin-version "." bin-platform)))
    (package
      (name "sops")
      (version bin-version)
      (source (origin
                (method url-fetch)
                (uri (string-append
                      "https://github.com/getsops/sops/releases/download/"
                      "v" version "/" bin-name))
                (sha256
                 (base32
                  "15qnh4hi15i8689gnwbrkypirn624hqm48nnw34jf8cpc7xhggyn"))))
      (build-system copy-build-system)
      (arguments
       (list
        ;; run by guix build daemon, so bin-name not in scope
        ;; #:install-plan #~`((,bin-name "bin/"))
        ;; otherwise, should be double-quoted
        ;; #:install-plan ''(("sops" "bin/"))
        ;; nothing i try here seemed to work until i added 'make-symlink
        #:install-plan ''(("." "bin/"
                           #:include-regexp ("sops.*$")))
        #:modules '((guix build copy-build-system)
                    (guix build utils)  ; for find-file
                    (srfi srfi-26))     ; for cut, a swappier curry

        #:phases
        #~(modify-phases %standard-phases
            (add-after 'unpack 'make-executable
              (lambda _
                (for-each (cut chmod <> #o555)
                          (find-files "." "sops.*$"))))

            ;; from tcl's phase install-private-headers
            (add-after 'install 'make-symlink
              (lambda* (#:key inputs outputs #:allow-other-keys)
                (let ((bin (string-append (assoc-ref outputs "out")
                                          "/bin")))
                  (with-directory-excursion bin
                    (symlink (car (find-files "." "sops"))
                             "sops"))))))))
      (inputs '())
      (native-inputs '())
      (home-page "https://github.com/getsops/sops")
      (synopsis "Simple and flexible tool for managing secrets")
      (description "SOPS is an editor of encrypted files that supports YAML,
JSON, ENV, INI and BINARY formats and encrypts with AWS KMS, GCP KMS, Azure
Key Vault, age, and PGP.")
      (license license:mpl2.0))))

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
;; TODO: maybe finish trying to package this. it doesn't build
(define-public step-cli
  (package
    (name "step-cli")
    (version "0.25.0")
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
    (version "0.25.1")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/smallstep/cli/releases/download/"
                    "v" version "/step_linux_" version "_amd64.tar.gz"))
              (sha256
               (base32
                "0yyv8s1x0wg7090pxh4nzi0yh5bicwvfq17dxf5iszz1r5iihscq"))))
    (build-system copy-build-system)
    (inputs
     (list coreutils pcsc-lite))
    (home-page "https://smallstep.com/cli/")
    (synopsis "(prebuilt) A zero trust swiss army knife for working with X509, OAuth, JWT, OATH, OTP, etc")
    (description "step is an easy-to-use CLI tool for building, operating, and automating Public Key Infrastructure (PKI) systems and workflows. It's the client counterpart to the step-ca online Certificate Authority (CA). You can use it for many common crypto and X.509 operations—either independently, or with an online CA.")
    (license license:asl2.0)))

(define-public step-ca-bin
  (package
    (name "step-ca-bin")
    (version "0.25.2")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/smallstep/certificates/releases/download/"
                    "v" version "/step-ca_linux_" version "_amd64.tar.gz"))
              (sha256
               (base32
                "0hfs1fb74wa1cbjapacwyspnm40i5xrh5br724yxw9f3l6iwvp2c"))))
    (build-system copy-build-system)
    (arguments
     (list
      #:install-plan ''(("." "bin/" #:include-regexp ("step-ca$")))))
    (inputs
     (list coreutils pcsc-lite))
    (home-page "https://smallstep.com/certificates/")
    (synopsis "(prebuilt) Open-Source Certificate Authority & PKI Toolkit")
    (description "A private certificate authority (X.509 & SSH) & ACME server for secure automated certificate management, so you can use TLS everywhere & SSO for SSH.")
    (license license:asl2.0)))
(define-public step-kms-plugin-bin
  (package
    (name "step-kms-plugin-bin")
    (version "0.10.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/smallstep/step-kms-plugin/releases/download/"
                    "v" version "/step-kms-plugin_" version "_linux_amd64.tar.gz"))
              (sha256
               (base32
                "1cha21n2wbzasycz5ygfa8kc3riqq1njxv3gh7xw92yzn1w9wmq1"))))
    (build-system binary-build-system)
    (inputs `((,gcc "lib")
              ,gcc-toolchain
              ,pcsc-lite))
    (propagated-inputs (list))
    (arguments
     (list
      #:patchelf-plan ''(("step-kms-plugin"
                          ;; gcc gcc-toolchain
                          ("pcsc-lite" "gcc" "gcc-toolchain")))
      #:install-plan ''(("." "bin/" #:include-regexp ("step-kms-plugin$")))))

    ;; TODO: update details ...
    ;; segfault, no debugging symbols

    (home-page "https://smallstep.com/certificates/")
    (synopsis "(prebuilt) Open-Source Certificate Authority & PKI Toolkit")
    (description "A private certificate authority (X.509 & SSH) & ACME server for secure automated certificate management, so you can use TLS everywhere & SSO for SSH.")
    (license license:asl2.0)))
