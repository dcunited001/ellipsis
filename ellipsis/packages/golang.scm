(define-module (ellipsis packages golang)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix gexp)
  #:use-module (guix utils)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix build-system go)
  #:use-module (guix build-system copy)

  #:use-module (gnu packages base)
  #:use-module (gnu packages golang)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages security-token)
  ;; #:use-module (gnu packages gcc)

  #:use-module (srfi srfi-1))

(define-public d2-bin
  (let* ((bin-platform "linux-amd64")
         (bin-version "0.6.5")
         (bin-name (string-append "d2-v" bin-version "-" bin-platform ".tar.gz")))
    (package
      (name "d2-bin")
      (version "0.6.5")
      (source (origin
                (method url-fetch)
                (uri (string-append
                      "https://github.com/terrastruct/d2/releases/download/"
                      "v" version "/" bin-name))
                (sha256
                 (base32
                  "0d57h4r06q5jsl8mwb02zpjkdwdj12gfrivqafv3fcr6mwcjjw0p"))))

      (build-system copy-build-system)
      (arguments
       (list
        #:substitutable? #f
        #:install-plan
        #~'(("bin/d2" "bin/")
            ("man/d2.1" "share/man/man1/"))))

      (home-page "https://oss.terrastruct.com/d2")
      (synopsis "Table of Contents")
      (description
       "The most convenient way to use D2 is to just run it as a CLI executable to
produce SVGs from @@code{.d2} files.")
      (license license:mpl2.0))))

