(define-module (ellipsis packages gnupg)
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

(define-public regpg-bin
  (package
    (name "regpg-bin")
    (version "1.11")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://dotat.at/prog/regpg/regpg"))
              (sha256
               (base32 "1frb0hx9lwbk2d79q01733bfvb4d12wdqqrv0cdqs6x6lsdjlflp"))))
    (build-system copy-build-system)

    (arguments
     (list
      #:install-plan ''(("." "bin/"
                         #:include-regexp ("regpg.*$")))
      #:modules '((guix build copy-build-system)
                  (guix build utils)    ; for find-file
                  (srfi srfi-26))       ; for cut, a swappier curry

      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'make-executable
            (lambda _
              (for-each (cut chmod <> #o555)
                        (find-files "." "regpg$")))))))

    (home-page "https://dotat.at/proj/regpg")
    (synopsis "Safely store server secrets with GnuPG")
    (description "The regpg program is a thin wrapper around gpg for looking after secrets that need to be stored engrypted in VCS. Secrets are encrypted as ASCII-armored messages to keyring for the project. The regpg tool is written in standard perl modules, but can integrate with Ansible Vault.")
    (license license:gpl3+)))


;; the main difference between 1.11 and 1.11.6 (from git) is that the latter
;; provides a getdnssec subcommand
;; TODO: get tests working. tests in the perl-b-keywords are failing
(define-public regpg
  (let ((commit "0329b97"))
    (package
      (name "regpg")
      (version "1.11.6")
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/fanf2/regpg")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "16nvi4c8p9n5snyg9hsh3r9wlxmip9ksnlapjcrv804di5r7v6nd"))))
      (build-system gnu-build-system)
      ;; perl-critic, Text::Markdown and etc are only needed for tests/docs
      ;; (inputs (list perl-critic))
      (inputs (list perl))
      (native-inputs (list ;; perl-critic
                      perl-text-markdown))
      (propagated-inputs '())
      (arguments
       (list
        ;; there's no ./configure script
        ;; #:make-flags
        ;; #~(list (string-append "PREFIX=" #$output))
        #:phases
        #~(modify-phases %standard-phases
            (add-after 'unpack 'patch-Makefile
              (lambda* (#:key inputs outputs #:allow-other-keys)
                (let ((out (assoc-ref outputs "out")))
                  (substitute* "Makefile"
                    ;; TODO: brittle. maybe fix later
                    ;; ("\\^all: ..ALL.$")
                    (("^ALL=.*$") "ALL=${PROGS} ${man1files}")
                    (("^all:.*$") "all: ${PROGS} ${man1files}")
                    (("^prefix.*=.*$")
                     (string-append "prefix = " out "\n"))))))
            (delete 'configure)
            (delete 'check))))
      (home-page "https://github.com/fanf2/regpg")
      (synopsis "Safely store server secrets with GnuPG")
      (description "The regpg program is a thin wrapper around gpg for looking
after secrets that need to be stored engrypted in VCS.  Secrets are encrypted
as ASCII-armored messages to keyring for the project.  The regpg tool is
written in standard perl modules, but can integrate with Ansible Vault.")
      (license license:gpl3))))


;; upgraded to get gpg-card and potentially fix some issues. they're fixed
;; and if the tests are passing ... then you can roll your own crypto?

(define-public gnupg2.3
  (package
    (inherit gnupg)
    (name "gnupg2.3")
    (version "2.3.8")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnupg/gnupg/gnupg-" version
                                  ".tar.bz2"))
              (patches (search-patches "gnupg-default-pinentry.patch"))
              (sha256
               (base32
                "1vb99657wxbizdskw0pxh0m568805ql1llpg23xn38kxwm07l2sl"))))
    (inputs
     (modify-inputs (package-inputs gnupg)
       (append
        ;; libldap
        bzip2
        libusb)))
    (arguments
     (substitute-keyword-arguments
         (package-arguments gnupg)
       ((#:phases phases)
        #~(modify-phases #$phases
            (replace 'patch-test-paths
              (lambda _
                (substitute*
                    '("tests/Makefile"
                      "tests/cms/inittests"
                      "tests/cms/Makefile"
                      "tests/pkits/inittests"
                      "tests/pkits/common.sh"
                      "tests/pkits/Makefile")
                  ;; (("/bin/pwd") (which "pwd"))
                  (("/bin/pwd") (string-append #$output "/bin/pwd")))
                (substitute* "common/t-exectool.c"
                  ;; (("/bin/cat") (which "cat"))
                  ;; (("/bin/true") (which "true"))
                  ;; (("/bin/false") (which "false"))
                  (("/bin/cat") (string-append #$output "/bin/cat"))
                  (("/bin/true") (string-append #$output "/bin/true"))
                  (("/bin/false") (string-append #$output "/bin/false")))))))))))

;; X: has package
;; Guix version (req. version)

;; (list gnutls       ;; x 3.7.7  (3.0)
;;       libassuan    ;; x 2.5.5  (2.5)
;;       libgcrypt    ;; x 1.10.1 (1.9.1)
;;       libgpg-error ;; x
;;       libksba      ;; x 1.6.3 (1.3.4)
;;       npth         ;; x 1.6   (1.2)
;;       openldap     ;; libldap
;;       pcsc-lite    ;; x
;;       readline     ;; x
;;       sqlite       ;; x 3.42.0 (3.27)
;;       zlib)        ;; x

;; + bzip2?           ;; 1.0.8 (>1.0?)
;; + pinentry? ...    ;; runtime
;; + tpm2-tss         ;; O >2.4.0? (3.0.3)
;; + libusb           ;; x 1.0.25 (1.0)

;; TPM2 support may be complicated (build outputs may suffice)
;; LDAP: may cause problems? maybe?

;; - https://dev.gnupg.org/source/gnupg/browse/master/configure.ac ;gnupg-2.3.8$1216-1247
;; - https://dev.gnupg.org/source/gnupg/browse/master/configure.ac;gnupg-2.3.8$1104-1111

;;  --enable-maintainer-mode?

;; phases:
;; - patches: these are fine. common/homedir.c last changed in 2016
;; - patch-paths: fix scdaemon references to pcscd dylib path
