(define-module (ellipsis packages password-utils)
  #:use-module (guix gexp)
  #:use-module (guix download)
  #:use-module (guix git-download)

  #:use-module (guix build-system trivial)
  #:use-module (guix build-system copy)

  #:use-module ((guix licenses) #:prefix license:)

  #:use-module (gnu packages)
  #:use-module (gnu packages rust))

(define-public shroud-nox
  (package
    (inherit shroud)
    (name "shroud-nox")
    (version "0f5a58da63cacce3bb839427fd1d87c9230800ad")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://git.dthompson.us/shroud.git")
                    (commit version)))
              (sha256
               (base32
                "0xn916i8ma9xinavq9avfi4rg9770c0hzimbwglqq1slfwa8c3nq"))
              (file-name (git-file-name name version))))
    (inputs
     (list guile-2.2 gnupg))

    (native-inputs
     (list pkg-config autoconf automake))))
