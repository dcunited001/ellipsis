(define-module (ellipsis packages protobuf)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix gexp)
  #:use-module (guix utils)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix packages)

  #:use-module (guix build-system go)

  #:use-module (gnu packages base)

  ;; #:use-module (guix import go)
  #:use-module (gnu packages golang)
  #:use-module (gnu packages golang-check)

  #:use-module (srfi srfi-1))

;; go                1.21.3 (1.21.5)
;; spew              1.1.1  (1.1.1)
;; testify           1.8.4  (1.9.0)
;; go-difflib        1.0.0  (1.0.0)
;;
;; stretcher/objx    0.5.1 (0.4.0)

;; including these was apparently unnecessary

;; go-github-com-stretchr-objx ;; 0.5.1 <= 0.4.0
;; go-gpkg-in-yaml-v3 ;; 3.0.1 (3.0.1)

(define-public proto-gen-md-diagrams
  (let* ((pkg-version "0.0.0-20240418192143-fbd6392ed2fd"))
    (package
      ;; (name "go-github-com-googlecloudplatform-proto-gen-md-diagrams")
      (name "proto-gen-md-diagrams")
      (version pkg-version)
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url
                "https://github.com/GoogleCloudPlatform/proto-gen-md-diagrams")
               (commit (go-version->git-ref version))))
         (file-name (git-file-name name version))
         (sha256
          (base32 "0l1p2aqh5av2lp4g0f6x2d9k8mpd2fhz8dc6aig127iz406r3j7c"))))
      (build-system go-build-system)
      (arguments
       (list
        #:go go-1.21 ;; go.mod: 1.21.3
        #:import-path "github.com/GoogleCloudPlatform/proto-gen-md-diagrams"))
      (propagated-inputs
       `(("go-github-com-stretchr-testify" ,go-github-com-stretchr-testify)))
      (home-page "https://github.com/GoogleCloudPlatform/proto-gen-md-diagrams")
      (synopsis "Proto Gen MD Diagrams")
      (description
       "This utility package is a compiled Go program that reads a protobuf source
directory and generates Mermaid Diagrams in .md files in each directory, or the
output directory with the given tree structure.")
      (license license:asl2.0))))
