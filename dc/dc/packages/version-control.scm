(define-module (dc packages version-control)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix gexp)
  #:use-module (guix utils)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix build-system copy)

  #:use-module (gnu packages base)

  #:use-module (srfi srfi-1))

(define-public git-stack-bin
  (let* ((bin-platform "x86_64-unknown-linux-musl")
         (bin-version "0.10.17")
         (bin-name
          (string-append "git-stack-v" bin-version "-" bin-platform ".tar.gz")))
    (package
      (name "git-stack-bin")
      (version bin-version)
      (source (origin
                (method url-fetch)
                (uri (string-append
                      "https://github.com/gitext-rs/git-stack/releases/download/"
                      "v" version "/" bin-name))
                (sha256
                 (base32
                  "13lxr136mb8i85xg91l6gb3q1x80gf440cy5n1ib90p8xh5dwjz4"))))
      (build-system copy-build-system)
      (arguments
       ;; why ".." ?
       `(#:install-plan '((".." "bin/" #:include ("git-stack")))))
      (inputs '())
      (native-inputs '())
      (home-page "https://github.com/gitext-rs/git-stack")
      (synopsis "Stacked branch management for Git")
      (description
       "Another approach to bringing the Stacked Diff workflow to PRs/branches that aims to be unintrusive to a project's workflow. Branches are the unit of work and review in git-stack. As you create stacked branches on top of each other, git-stack will takes care of all of the micromanagement for you.")
      (license license:asl2.0))))
