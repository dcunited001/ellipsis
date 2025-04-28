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

  #:use-module (gnu)
  #:use-module (gnu packages)

  #:use-module (gnu packages golang)

  #:use-module (srfi srfi-1))

;; perl: pod2man for libtpms & swtpm
;; python: swtpm
;; gnome: json-glib for swtpm
;; tcl: expect
(use-package-modules base gcc glib commencement m4 autotools pkg-config perl
                     tcl
                     linux networking python gnome tls security-token hardware)

;; watchexec -p -e '*.scm' -- guix build -L $HOME/.dotfiles/ellipsis libtpms



(define-public libtpms
  (package
    (name "libtpms")
    (version "0.10.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/stefanberger/libtpms")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0nawrc09ahmb1hcxw58v79bwbm8v7xprg9r8nm78nl3wh9fkzav0"))))
    (build-system gnu-build-system)
    (native-inputs (list m4 autoconf automake libtool pkg-config perl))
    (inputs (list openssl tpm2-tss))
    
    (arguments
     (list
      #:phases #~(modify-phases %standard-phases
                   (delete 'check))
      #:configure-flags
      #~(list "--with-tpm2"
              "--with-openssl")))

    (home-page "https://github/stefanberger/libtpms")
    (synopsis
     "The libtpms library provides software emulation of a Trusted Platform Module (TPM 1.2 and TPM 2.0)")
    (description "Libtpms is a library that targets the integration of TPM functionality
into hypervisors, primarily into Qemu. Libtpms provides a very narrow
public API for this purpose so that integration is possible. Only the
minimum of necessary APIs are made publicly available.")
    (license license:bsd-3)))

;; https://github.com/stefanberger/swtpm/blob/9bdd62d1e96b5723920ffe9f09325d1ddad66905/configure.ac#L512-L522

;; also from swtpm Dockerfile "softhsm or certtool are crashing the pkcs11 test case"
;; also from swtpm Dockerfile --with-cuse https://man.archlinux.org/man/swtpm_cuse.8.en

;; so many

(define-public swtpm
  (package
    (name "swtpm")
    (version "0.10.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/stefanberger/swtpm")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1kbxps7kmkd6dnnfv1rzz83bm6ks4pls4lcz0k9y92g0la2m6jk4"))))
    (build-system gnu-build-system)
    (native-inputs
     (list m4 autoconf automake libtool pkg-config expect socat perl python
           net-tools which))
    (inputs
     (list libtpms openssl libtasn1 glib json-glib gnutls fuse libseccomp))
    (arguments
     (list
      #:tests? #f
      #:configure-flags
      #~(list "--with-openssl"
              "--localstatedir=/var"
              ;; "--with-cuse" ;; "could not get cflags for libfuse"
              "--without-selinux")
      #:make-flags
      #~(list
         (string-append "CC=" #$(cc-for-target))
         (string-append "PKG_CONFIG=" #$(pkg-config-for-target)))
      #:phases
      #~(modify-phases %standard-phases
          ;; this may not be an exhaustive list. i just grepped the *.log
          ;; files in ./tests for "port"

          (add-after 'unpack 'patch-source
            (lambda* (#:key inputs outputs #:allow-other-keys)
              (let ((certtool (assoc-ref inputs "gnutls"))
                    (out (assoc-ref outputs "out")))
                ;; (substitute* "configure.ac"
                ;;   (("^install-data-local") "do-not-execute:"))
                (substitute* "samples/Makefile.am"
                  (("^install-data-local:") "do-not-execute:"))
                (substitute* "src/swtpm_localca/swtpm_localca.c"
                  ;; the top is only used when __APPLE__
                  (("#define CERTTOOL_NAME \"gnutls-certtool\"")
                   (string-append
                    "#define CERTTOOL_NAME \""
                    (search-input-file inputs "bin/certtool") "\""))
                  (("#define CERTTOOL_NAME \"certtool\"")
                   (string-append
                    "#define CERTTOOL_NAME \""
                    (search-input-file inputs "bin/certtool") "\"")))))))))
    (home-page "https://github/stefanberger/swtpm")
    (synopsis
     "Libtpms-based TPM emulator with socket, character device, and Linux CUSE interface")
    (description "The SWTPM package provides TPM emulators with different
front-end interfaces to libtpms. TPM emulators provide socket
interfaces (TCP/IP and Unix) and the Linux CUSE interface for the creation of
multiple native /dev/vtpm* devices.

The SWTPM package also provides several tools for using the TPM emulator,
creating certificates for a TPM, and simulating the manufacturing of a TPM by
creating a TPM's EK and platform certificates etc.

Please read the READMEs in the individual tool's directory under src/.

Please consult the Wiki for information about swtpm")
    (license license:bsd-3)))

(define-public age-plugin-yubikey-bin
  (let* ((bin-platform "x86_64-linux")
         (bin-version "0.5.0")
         (bin-name (string-append "age-plugin-yubikey-v"
                                  bin-version "-" bin-platform
                                  ".tar.gz")))
    (package
      (name "age-plugin-yubikey-bin")
      (version "0.5.0")
      (source (origin
                (method url-fetch)
                (uri (string-append
                      "https://github.com/str4d/age-plugin-yubikey"
                      "/releases/download/" "v" version "/" bin-name))
                (sha256
                 (base32
                  "1hxja5ziy4c1cf1wdhinr42bsbq8laq3swnhfdnya6y87yhkb6q1"))))
      (build-system binary-build-system)
      (inputs `((,gcc "lib")
                ,gcc-toolchain
                ,pcsc-lite))
      (propagated-inputs (list))
      (arguments
       (list
        #:patchelf-plan #~'(("age-plugin-yubikey"
                             ("pcsc-lite" "gcc" "gcc-toolchain"))
                            ;; #:include "age-plugin-yubikey"
                            )
        #:install-plan #~'(("age-plugin-yubikey" "bin/"))))
      (home-page "https://github.com/str4d/age-plugin-yubikey")
      (synopsis "YubiKey plugin for age")
      (description
       "age-plugin-yubikey is a plugin for age clients like age and rage, which enables files to be encrypted to age identities stored on YubiKeys.")
      (license license:expat))))

;; NOTE: it builds: no idea whether it works
;; (define-public go-github-com-go-piv-piv-go
;;   (package
;;     (name "go-github-com-go-piv-piv-go")
;;     (version "1.11.0")
;;     (source
;;      (origin
;;        (method git-fetch)
;;        (uri (git-reference
;;              (url "https://github.com/go-piv/piv-go")
;;              (commit (string-append "v" version))))
;;        (file-name (git-file-name name version))
;;        (sha256
;;         (base32 "13njcz4lxrac5r3ww4bdrrzkiz2x057d6cyjq9p2jylygkwkp631"))))
;;     (build-system go-build-system)

;;     ;; the importpath is ./piv. makefile calls `go build ./...`
;;     ;; #:import-path "github.com/go-piv/piv-go"

;;     (native-inputs (list pkg-config))
;;     (inputs (list pcsc-lite))
;;     (arguments
;;      (list
;;       #:unpack-path "github.com/go-piv/piv-go"
;;       #:import-path "github.com/go-piv/piv-go/piv"
;;       ;; TODO: fix tests: pcsc_test.go and piv_test.go rely on hardware
;;       ;; the only test that passes is key_test.go
;;       #:tests? #f
;;       #:phases
;;       #~(modify-phases %standard-phases
;;           (add-after 'unpack 'delete-python-script
;;             (lambda _
;;               (for-each delete-file (find-files "src" "pcsc_errors\\.py$")))))))
;;     (home-page "https://github.com/go-piv/piv-go")
;;     (synopsis "A Go YubiKey PIV implementation")
;;     (description "This is not an officially supported Google product")
;;     (license license:asl2.0)))
