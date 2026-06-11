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

(define-public libtpms-dc
  (package
    (name "libtpms-dc")
    (version "0.10.2")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/stefanberger/libtpms")
                     (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "190qnjp384a8i8jqh3xyxac7i3cbsbphbl6wk91kw5gzjymjj4aj"))))
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
    ;; the TPM 2.0 code has a slightly different license
    (license license:bsd-3)))

(define-public swtpm-dc
  (package
    (name "swtpm-dc")
    (version "0.10.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/stefanberger/swtpm")
                     (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0wah3zsnkasccazzlycq8scc52q4h1g58w0br45sr185inw6zgrp"))))
    (build-system gnu-build-system)
    (native-inputs
     (list m4 autoconf automake libtool pkg-config expect socat perl python
           net-tools which))
    (inputs
     (list libtpms-dc openssl libtasn1 glib json-glib gnutls fuse libseccomp))
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
