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

;;** Codesearch

;; TODO: these are technically separate packages (and leaf-level CLI, almost
;; self-contained). Do they need to be bundled separately? Do they need to
;; follow the Guix go package naming conventions?

;; #:go go-1.17 ;; earlier, specifying go-1.14 used 1.17 anyways
;;
;; this changed after adding #:unpack-path, which probably means it wasn't
;; properly discovering the configuration/metadata for the main package

(define-public go-github-com-google-codesearch-cmd-cgrep
  (package
    (name "go-github-com-google-codesearch-cmd-cgrep")
    (version "1.2.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/google/codesearch")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1jzgq0rf9qzng1mqlpx0ib7zwz5hk6lnj6fmp923xmkpyvqg0kcb"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.14 ;; go.mod: 1.13
      #:import-path "github.com/google/codesearch/cmd/cgrep"
      #:unpack-path "github.com/google/codesearch"))
    (home-page "https://github.com/google/codesearch")
    (synopsis #f)
    (description #f)
    (license license:bsd-3)))

(define-public go-github-com-google-codesearch-cmd-csearch
  (package
    (name "go-github-com-google-codesearch-cmd-csearch")
    (version "1.2.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/google/codesearch")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1jzgq0rf9qzng1mqlpx0ib7zwz5hk6lnj6fmp923xmkpyvqg0kcb"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.14 ;; go.mod: 1.13
      #:import-path "github.com/google/codesearch/cmd/csearch"
      #:unpack-path "github.com/google/codesearch"))
    (home-page "https://github.com/google/codesearch")
    (synopsis #f)
    (description #f)
    (license license:bsd-3)))

(define-public go-github-com-google-codesearch-cmd-cindex
  (package
    (name "go-github-com-google-codesearch-cmd-cindex")
    (version "1.2.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/google/codesearch")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1jzgq0rf9qzng1mqlpx0ib7zwz5hk6lnj6fmp923xmkpyvqg0kcb"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.14 ;; go.mod: 1.13
      #:import-path "github.com/google/codesearch/cmd/cindex"
      #:unpack-path "github.com/google/codesearch"))
    (home-page "https://github.com/google/codesearch")
    (synopsis #f)
    (description #f)
    (license license:bsd-3)))
