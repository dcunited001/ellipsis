(define-module (ellipsis packages security-token)
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

  #:use-module (gnu packages base)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages commencement)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages security-token)

  #:use-module (gnu packages golang)

  ;; python packages
  ;; #:use-module (guix build-system python)
  ;; #:use-module (guix build-system pyproject)
  ;; #:use-module (gnu packages python-build)
  ;; #:use-module (gnu packages python-crypto)
  ;; #:use-module (gnu packages python-xyz)

  ;; python-pyusb
  ;; #:use-module (gnu packages libusb)

  ;; python-mock
  ;; #:use-module (gnu packages check)
  ;; #:use-module (gnu packages swig)
  ;; #:use-module (gnu packages pkg-config)

  ;; python-fido2: public-suffix-list
  ;; #:use-module (gnu packages dns)
  ;; #:use-module (gnu packages security-token)

  #:use-module (srfi srfi-1))

(define-public age-plugin-yubikey-bin
  (let* ((bin-platform "x86_64-linux")
         (bin-version "0.4.0")
         (bin-name (string-append "age-plugin-yubikey-v"
                                  bin-version "-" bin-platform
                                  ".tar.gz")))
    (package
      (name "age-plugin-yubikey-bin")
      (version "0.4.0")
      (source (origin
                (method url-fetch)
                (uri (string-append
                      "https://github.com/str4d/age-plugin-yubikey"
                      "/releases/download/" "v" version "/" bin-name))
                (sha256
                 (base32
                  "0is0sgycnm9ymb1ayk8rrppz67v97js5i352wjxrm2kkkipmk258"))))
      (build-system binary-build-system)
      (inputs `((,gcc "lib")
                ,gcc-toolchain
                ,pcsc-lite))
      (propagated-inputs (list))
      (arguments
       (list
        #:patchelf-plan ''(("age-plugin-yubikey"
                            ("pcsc-lite" "gcc" "gcc-toolchain"))
                          ;; #:include "age-plugin-yubikey"
                          )
        #:install-plan ''(("age-plugin-yubikey" "bin/")
                          ;; #:include "age-plugin-yubikey"
                          )))
      (home-page "")
      (synopsis "")
      (description "")
      (license license:expat))))


;; NOTE: it builds: no idea whether it works
(define-public go-github-com-go-piv-piv-go
  (package
    (name "go-github-com-go-piv-piv-go")
    (version "1.11.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/go-piv/piv-go")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "13njcz4lxrac5r3ww4bdrrzkiz2x057d6cyjq9p2jylygkwkp631"))))
    (build-system go-build-system)

    ;; the importpath is ./piv. makefile calls `go build ./...`
    ;; #:import-path "github.com/go-piv/piv-go"

    (native-inputs (list pkg-config))
    (inputs (list pcsc-lite))
    (arguments
     (list
      #:unpack-path "github.com/go-piv/piv-go"
      #:import-path "github.com/go-piv/piv-go/piv"
      ;; TODO: fix tests: pcsc_test.go and piv_test.go rely on hardware
      ;; the only test that passes is key_test.go
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'delete-python-script
            (lambda _
              (for-each delete-file (find-files "src" "pcsc_errors\\.py$")))))))
    (home-page "https://github.com/go-piv/piv-go")
    (synopsis "A Go YubiKey PIV implementation")
    (description "This is not an officially supported Google product")
    (license license:asl2.0)))

