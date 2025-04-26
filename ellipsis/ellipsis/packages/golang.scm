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
  #:use-module (gnu packages golang-xyz)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages security-token)
  ;; #:use-module (gnu packages gcc)

  #:use-module (srfi srfi-1))

(define-public d2-bin
  (let* ((bin-platform "linux-amd64")
         (bin-version "0.6.9")
         (bin-name (string-append "d2-v" bin-version "-" bin-platform ".tar.gz")))
    (package
      (name "d2-bin")
      (version "0.6.9")
      (source (origin
                (method url-fetch)
                (uri (string-append
                      "https://github.com/terrastruct/d2/releases/download/"
                      "v" version "/" bin-name))
                (sha256
                 (base32
                  "0hzlyyjzzdv8fna0z6c6jska81a27bn496namij0bnm2bc3afwjv"))))

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

;;*** Setup

;; heard about this on a podcast and i was curious

;; -----------------------
;; cd /data/ecto/finance/openbb
;; export CSEARCHINDEX=$PWD/.csearchindex
;; cindex .
;; find . -name '*.ts*' -type f -print | xargs cgrep -n -i "atom"  | wc -l
;; -----------------------

;; it does seem fairly fast, but code changes, so you need a background
;; service (or a git hook), probably with multiple checkouts.

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
      #:install-source? #f
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
      #:install-source? #f
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
      #:install-source? #f
      #:import-path "github.com/google/codesearch/cmd/cindex"
      #:unpack-path "github.com/google/codesearch"))
    (home-page "https://github.com/google/codesearch")
    (synopsis #f)
    (description #f)
    (license license:bsd-3)))
