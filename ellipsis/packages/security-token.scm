(define-module (ellipsis packages security-token)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix build utils)
  #:use-module ((guix licenses) #:prefix license:)

  ;; #:use-module (guix build-system python)
  #:use-module (guix build-system python)
  #:use-module (guix build-system pyproject)
  #:use-module (guix download)
  #:use-module (guix licenses)

  ;; #:use-module (gnu packages python-build)
  #:use-module (gnu packages python-crypto)
  #:use-module (gnu packages python-xyz)
  ;; python-pyusb
  #:use-module (gnu packages libusb)
  ;; python-mock
  #:use-module (gnu packages check)
  #:use-module (gnu packages swig)
  #:use-module (gnu packages pkg-config)
  ;; python-fido2: public-suffix-list
  #:use-module (gnu packages dns)
  #:use-module (gnu packages security-token)

  #:use-module (srfi srfi-1))

;; does build with emacs-guix (using poetry 1.1.12

;; ==================
;; doesn't build with:
;;
;; guix shell -L . -e '(@@ (ellipsis packages security-token) python-yubikey-manager)'

;; .... since it wants to build poetry

;; ==================

;; also doesn't build with:

;; guix shell -L . -e '(list (@ (ellipsis packages security-token) python-yubikey-manager) (@ (ellipsis packages security-token) python-pyscard) (@ (ellipsis packages security-token) python-fido2))'

;; since again, it wants to build poetry 1.4.2

;; ==================

;; well i don't always roll my own crypto, but when i do...

;; oh hey, the package builds
(define-public python-yubikey-manager
  (package
    (name "python-yubikey-manager")
    (version "5.1.1")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://developers.yubico.com/yubikey-manager/Releases"
                    "/yubikey_manager-" version ".tar.gz"))
              (sha256
               (base32
                "1kma08rxvpzn2gf8b9vxyyb2pvrakm7hhpdmbnb54nwbdnbxp1v4"))))
    (build-system pyproject-build-system)
    (arguments
     '( ;; This attempts to access
       ;; /System/Library/Frameworks/IOKit.framework/IOKit
       ;; The recommendation is to use tox for testing.
       #:tests? #false))
    (propagated-inputs
     (list python-six
           python-pyscard
           python-pyusb
           python-click
           python-cryptography
           python-pyopenssl
           python-fido2))
    (inputs
     (list pcsc-lite))
    (native-inputs
     (list swig python-mock poetry))
    (home-page "https://developers.yubico.com/yubikey-manager/")
    (synopsis "Command line tool and library for configuring a YubiKey")
    (description
     "Python library and command line tool for configuring a YubiKey.  Note
that after installing this package, you might still need to add appropriate
udev rules to your system configuration to be able to configure the YubiKey as
an unprivileged user.")
    (license license:bsd-2)))

;; fairly consistent changes (minimal) to setup.py from 1.9.8 - 2.0.7
(define-public python-pyscard
  (package
    (name "python-pyscard")
    (version "2.0.7")
    (source (origin
              (method url-fetch)
              ;; The maintainer publishes releases on various sites, but
              ;; SourceForge is apparently the only one with a signed release.
              (uri (string-append
                    "mirror://sourceforge/pyscard/pyscard/pyscard%20"
                    version "/pyscard-" version ".tar.gz"))
              (sha256
               (base32
                "1gy1hmzrhfa7bqs132v89pchm9q3rpnqf3a6225vwpx7bx959017"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         ;; Tell pyscard where to find the PCSC include directory.
         (add-after 'unpack 'patch-platform-include-dirs
           (lambda* (#:key inputs #:allow-other-keys)
             (let ((pcsc-include-dir (string-append
                                      (assoc-ref inputs "pcsc-lite")
                                      "/include/PCSC")))
               (substitute* "setup.py"
                 (("platform_include_dirs = \\[.*?\\]")
                  (string-append
                   "platform_include_dirs = ['" pcsc-include-dir "']")))
               #t)))
         ;; pyscard wants to dlopen libpcsclite, so tell it where it is.
         (add-after 'unpack 'patch-dlopen
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* "smartcard/scard/winscarddll.c"
               (("lib = \"libpcsclite\\.so\\.1\";")
                (simple-format #f
                               "lib = \"~a\";"
                               (search-input-file inputs
                                                  "/lib/libpcsclite.so.1"))))
             #t)))))
    (inputs
     (list pcsc-lite))
    (native-inputs
     (list swig pkg-config))
    (home-page "https://github.com/LudovicRousseau/pyscard")
    (synopsis "Smart card library for Python")
    (description
     "The pyscard smart card library is a framework for building smart card
aware applications in Python.  The smart card module is built on top of the
PCSC API Python wrapper module.")
    (license license:lgpl2.1+)))

;; version 1.0.0 changed from setup.py based build to pyproject.toml

;; build procedures defined in:
;; https://github.com/Yubico/python-fido2/commit/eae65b57a078b40465534bc21547073d434a89dc#diff-5c3fa597431eda03ac3339ae6bf7f05e1a50d6fc7333679ec38e21b337cb6721
(define-public python-fido2
  (package
    (name "python-fido2")
    (version "1.1.1")
    (source (origin
              (method url-fetch)
              (uri
               (string-append
                "https://github.com/Yubico/python-fido2/releases/download/"
                version "/fido2-" version ".tar.gz"))
              (sha256
               (base32
                "1hwz0xagkmy6hhcyfl66dxf2vfa69lqqqjrv70vw7harik59bi2x"))
              (snippet
               ;; Remove bundled dependency.
               '(delete-file "fido2/public_suffix_list.dat"))))
    (build-system pyproject-build-system)
    (arguments
     `( ;; This attempts to access
       ;; /System/Library/Frameworks/IOKit.framework/IOKit
       ;; The recommendation is to use tox for testing.
       #:tests? #false
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'install-public-suffix-list
           (lambda* (#:key inputs #:allow-other-keys)
             (copy-file
              (search-input-file inputs
                                 (string-append
                                  "/share/public-suffix-list-"
                                  ,(package-version public-suffix-list)
                                  "/public_suffix_list.dat"))
              "fido2/public_suffix_list.dat"))))))
    (propagated-inputs
     (list python-cryptography python-six))
    (native-inputs
     (list python-mock python-pyfakefs public-suffix-list poetry))
    (home-page "https://github.com/Yubico/python-fido2")
    (synopsis "Python library for communicating with FIDO devices over USB")
    (description
     "This Python library provides functionality for communicating with a Fast
IDentity Online (FIDO) device over Universal Serial Bus (USB) as well as
verifying attestation and assertion signatures.  It aims to support the FIDO
Universal 2nd Factor (U2F) and FIDO 2.0 protocols for communicating with a USB
authenticator via the Client-to-Authenticator Protocol (CTAP 1 and 2).  In
addition to this low-level device access, classes defined in the
@code{fido2.client} and @code{fido2.server} modules implement higher level
operations which are useful when interfacing with an Authenticator, or when
implementing a Relying Party.")
    ;; python-fido2 contains some derivative files originally from pyu2f
    ;; (https://github.com/google/pyu2f).  These files are licensed under the
    ;; Apache License, version 2.0.  The maintainers have customized these
    ;; files for internal use, so they are not really a bundled dependency.
    (license (list license:bsd-2 license:asl2.0))))


;; TODO: confirm that udev processes yubikey events
;; - and that can be used with opensc
;; - these files are in /run/current-system/profile/lib/udev/rules.d
;; - not found with the links in /etc/udev/rules.d!

;; (define-public libfido2-udev
;;   (package
;;     (name "libfido2-udev")
;;     (version "1.11.0")
;;     (source (origin
;;               (method git-fetch)
;;               (uri (git-reference
;;                     (url "https://github.com/Yubico/libfido2")
;;                     (commit version)))
;;               (file-name (git-file-name name version))
;;               (sha256
;;                (base32
;;                 "1234"))))
;;     (build-system trivial-build-system)
;;     (native-inputs `(("source" ,source)))
;;     (arguments
;;      '(#:modules ((guix build utils))
;;        #:builder
;;        (begin
;;          (use-modules (guix build utils))
;;          (let ((source (assoc-ref %build-inputs "source")))
;;            (install-file (string-append))
;;            ;;; TODO: here
;;            ))))
;;     (home-page "https://github.com/Yubico/libfido2")
;;     (synopsis "Provides udev rules for FIDO2 via yubikey devices")
;;     (description "'nuff said")
;;     (license license:bsd-2)))



;; (define-public android-udev-rules
;;   (package
;;     (name "android-udev-rules")
;;     (version "20210501")
;;     (source
;;      (origin
;;        (method git-fetch)
;;        (uri (git-reference
;;              (url "https://github.com/M0Rf30/android-udev-rules")
;;              (commit version)))
;;        (file-name (git-file-name name version))
;;        (sha256
;;         (base32 "0pl1wfd7k9vz8mvy2jb2icc5f11c5p07aixpyhjs6gi5cyaywm5f"))))
;;     (build-system trivial-build-system)
;;     (native-inputs `(("source" ,source)))
;;     (arguments
;;      '(#:modules ((guix build utils))
;;        #:builder
;;        (begin
;;          (use-modules (guix build utils))
;;          (let ((source (assoc-ref %build-inputs "source")))
;;            (install-file (string-append source "/51-android.rules")
;;                          (string-append %output "/lib/udev/rules.d"))))))
;;     (home-page "https://github.com/M0Rf30/android-udev-rules")
;;     (synopsis "udev rules for Android devices")
;;     (description "Provides a set of udev rules to allow using Android devices
;; with tools such as @command{adb} and @command{fastboot} without root
;; privileges.  This package is intended to be added as a rule to the
;; @code{udev-service-type} in your @code{operating-system} configuration.
;; Additionally, an @code{adbusers} group must be defined and your user added to
;; it.

;; @emph{Simply installing this package will not have any effect.}  It is meant
;; to be passed to the @code{udev} service.")
;;     (license license:gpl3+)))
