(define-module (dc packages rust-apps ts-query-ls)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages)
  #:use-module (gnu packages gcc)
  #:use-module (nonguix build-system binary)

  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix transformations) ;; for python-requests=2.30
  #:use-module (guix utils)
  #:use-module (srfi srfi-1))

(define-public ts-query-ls-bin
  (let* ((bin-platform "x86_64-unknown-linux-gnu")
         (bin-version "3.16.0")
         (bin-name (string-append "ts_query_ls-" bin-platform)))

    (package
      (name "ts-query-ls-bin")
      (version bin-version)
      (source (origin
                (method url-fetch)
                (uri (string-append
                      "https://github.com/ribru17/ts_query_ls/releases/download/v"
                      version "/" bin-name ".tar.gz"))
                (sha256
                 (base32
                  "0ga4dmjkzhxac8b0lgwn4sycc50asnbgs00h4fnblghw2iv9319m"))))

      (build-system binary-build-system)
      (arguments
       (list

        #:patchelf-plan #~`(("ts_query_ls" ("gcc" "libc")))
        #:install-plan ''(("." "bin/"
                           #:include-regexp ("ts_query_ls.*$")))))

      (inputs `((,gcc "lib")))
      (propagated-inputs '())
      (home-page "https://github.com/ribru17/ts_query_ls")
      (synopsis "Tree sitter query write good")
      (description "... TODO update")
      (license license:expat))))
