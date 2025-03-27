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
    (version "1.12.3")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://dotat.at/prog/regpg/regpg"))
              (sha256
               (base32 "0828cx4i2xfksx4fl4abb8d47q5jfh18mzd1vxhdx0sgj832kscv"))))
    (build-system copy-build-system)

    (arguments
     (list
      #:install-plan ''(("." "bin/"
                         #:include-regexp ("regpg.*$")))
      #:modules '((guix build copy-build-system)
                  (guix build utils)    ; for find-file
                  (srfi srfi-26))
                                        ; for cut, a swappier curry

      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'make-executable
            (lambda _
              (for-each (cut chmod <> #o555)
                        (find-files "." "regpg$")))))))

    (home-page "https://dotat.at/proj/regpg")
    (synopsis "Safely store server secrets with GnuPG")
    (description
     "The regpg program is a thin wrapper around gpg for looking after secrets that need to be stored engrypted in VCS. Secrets are encrypted as ASCII-armored messages to keyring for the project. The regpg tool is written in standard perl modules, but can integrate with Ansible Vault.")
    (license license:gpl3+)))

;; the main difference between 1.11 and 1.11.6 (from git) is that the latter
;; provides a getdnssec subcommand
;; TODO: get tests working. tests in the perl-b-keywords are failing
(define-public regpg
  (let ((commit "0329b97"))
    (package
      (name "regpg")
      (version "1.12")
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
