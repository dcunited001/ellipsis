(define-module (ellipsis packages gnupg)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix gexp)
  #:use-module (guix utils)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix packages)

  ;; TODO: maybe remove
  #:use-module (guix build utils)

  #:use-module (guix build-system gnu)
  #:use-module (guix build-system copy)
  #:use-module (guix build-system perl)

  #:use-module (gnu packages base)
  #:use-module (gnu packages perl)

  #:use-module (ellipsis packages)
  #:use-module (ellipsis packages perl)
  #:use-module (srfi srfi-1))

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
