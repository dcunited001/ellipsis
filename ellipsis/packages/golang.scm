(define-module (ellipsis packages golang)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix gexp)
  #:use-module (guix utils)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix build-system go)

  #:use-module (gnu packages base)
  #:use-module (gnu packages golang)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages security-token)
  ;; #:use-module (gnu packages gcc)

  #:use-module (srfi srfi-1)

  )

;; guix import go github.com/smallstep/truststore (doesn't install)
;; guix import go -r github.com/smallstep/truststore (errors)
(define-public go-github-com-smallstep-truststore
  (package
    (name "go-github-com-smallstep-truststore")
    (version "0.11.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/smallstep/truststore")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "1cbn6jxfkxzf0nyh86261r1cbjsdkfq5g2am2hfsb3fnak8vi8nm"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/smallstep/truststore"))
    (propagated-inputs
     `(("go-howett-net-plist" ,go-howett-net-plist)))
    (home-page
     "https://github.com/smallstep/truststore")
    (synopsis "truststore")
    (description
     "Package to locally install development certificates.")
    (license
     (list
      license:asl2.0))))

;; guix import go -r gopkg.in/cheggaaa/pb.v1
(define-public go-gopkg-in-cheggaaa-pb-v1
  (package
    (name "go-gopkg-in-cheggaaa-pb-v1")
    (version "1.0.28")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://gopkg.in/cheggaaa/pb.v1")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "13a66cqbpdif804qj12z9ad8r24va9q41gfk71qbc4zg1wsxs3rh"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "gopkg.in/cheggaaa/pb.v1"))
    (home-page "https://gopkg.in/cheggaaa/pb.v1")
    (synopsis "Terminal progress bar for Go")
    (description "Simple console progress bars
")
    (license
     (list unknown-license!
           unknown-license!
           unknown-license!
           unknown-license!
           license:bsd-3))))

;; guix import go -r gopkg.in/natefinch/lumberjack.v2
(define-public go-gopkg-in-natefinch-lumberjack-v2
  (package
    (name "go-gopkg-in-natefinch-lumberjack-v2")
    (version "2.0.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://gopkg.in/natefinch/lumberjack.v2")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "1m2sxypk7p805jvc68padvylyx5v7cwkh5klnnxxr0340kgspf08"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path
       "gopkg.in/natefinch/lumberjack.v2"))
    (home-page
     "https://gopkg.in/natefinch/lumberjack.v2")
    (synopsis "lumberjack")
    (description
     "Package lumberjack provides a rolling logger.
")
    (license
     (list unknown-license!
           unknown-license!
           unknown-license!
           unknown-license!
           license:expat))))
