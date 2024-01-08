(define-module (ellipsis packages k8s)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix gexp)
  #:use-module (guix utils)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix packages)

  #:use-module (guix build-system trivial)
  #:use-module (guix build-system copy)

  #:use-module (gnu packages base)
  #:use-module (gnu packages perl)

  #:use-module (srfi srfi-1))

;; TODO: copy the bin to bin/, set executable (not intended to run as user)
;; TODO: write a service?

(define-public k0s-bin
  (let* ((bin-platform "amd64")
         (bin-version "1.28.4")
         (bin-name (string-append "k0s-v" bin-version
                                  "+k0s.0-"
                                  bin-platform)))

    ;; bin-name: k0s-v1.28.4+k0s.0-amd64
    ;; url: v1.28.4+k0s.0/k0s-v1.28.4+k0s.0-amd64

    (package
      (name "k0s-bin")
      (version bin-version)
      (source (origin
                (method url-fetch)
                (uri (string-append
                      "https://github.com/k0sproject/k0s/releases/download/"
                      "v" version "+k0s.0/" bin-name))
                (sha256
                 (base32
                  "0xd9spyp3pzdd99ryjzjv5326414jp9h3gvrhjxva3lg8xlcnswj"))))
      ;; (build-system trivial-build-system)
      (build-system copy-build-system)
      (inputs '())
      (propagated-inputs '())
      (home-page "")
      (synopsis "")
      (description "")
      (license license:asl2.0))))
