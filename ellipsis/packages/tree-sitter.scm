(define-module (ellipsis packages tree-sitter)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages)
  #:use-module (gnu packages crates-graphics)
  #:use-module (gnu packages crates-io)
  ;; #:use-module (gnu packages icu4c)
  #:use-module (gnu packages node)
  #:use-module (gnu packages tree-sitter)
  #:use-module (guix build-system cargo)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system tree-sitter)
  #:use-module (guix gexp)
  #:use-module (guix hg-download)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix utils))

;; TODO: https://github.com/shopify/tree-sitter-liquid

;; NOTE these need (define-public ...) to work with guix-devel-mode
(define tree-sitter-javascript-0.20.3
  (package
    (inherit tree-sitter-javascript)
    (version "0.20.3")))

(define tree-sitter-typescript-0.20.5
  (package
    (inherit tree-sitter-typescript)
    (version "0.20.5")
    (inputs (modify-inputs
                (package-inputs tree-sitter-typescript)
              (replace "tree-sitter-javascript" tree-sitter-javascript-0.20.3)))))

(define-public tree-sitter-qmljs
  (let* ((commit "9fa49ff3315987f715ce5666ff979a7742fa8a98")
         (github-repo "https://github.com/yuja/tree-sitter-qmljs")
         (revision "1"))

    (package
      (name "tree-sitter-qmljs")
      (version (git-version "0.9" revision commit))
      (home-page github-repo)
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url github-repo)
                      (commit commit)))
                (sha256 (base32 "18sgh27wfvndijcja871cmnk3w8da6izwa4q8rmbml7ca8nj0vdb"))
                (file-name (git-file-name name version))))
      (build-system tree-sitter-build-system)
      (arguments
       (list #:grammar-directories '(".")
             #:phases #~(modify-phases %standard-phases
                          ;; some tests fail, see yuja/tree-sitter-qmljs#7,
                          ;; may file issue/PR to fix tests but idk js or qml
                          (delete 'check))))
      (inputs (modify-inputs (package-inputs tree-sitter-typescript-0.20.5)
                (prepend tree-sitter-typescript-0.20.5)))
      (synopsis "Tree-sitter QML")
      (description "This library provides QML grammar for the tree-sitter
parsing library.")
      (license license:expat))))

;; From Guix ./gnu/packages/tree-sitter.scm
(define (tree-sitter-delete-generated-files grammar-directories)
  #~(begin
      (use-modules (guix build utils))
      (delete-file "binding.gyp")
      (delete-file-recursively "bindings")
      (for-each
       (lambda (lang)
         (with-directory-excursion lang
           (delete-file "src/grammar.json")
           (delete-file "src/node-types.json")
           (delete-file "src/parser.c")
           (delete-file-recursively "src/tree_sitter")))
       '#$grammar-directories)))

(define-public tree-sitter-tcl
  (let* ((rev-base "0.1.0")
         (revision "112")
         (changeset-full "8b37d53b9bde1efe75679e156347abd02d264667")
         (changeset-short (substring changeset-full 0 7))
         (version "0.1.0")
         (guix-revision "0")
         (guix-version (string-concatenate (list version "-" guix-revision "." changeset-short)))
         (grammar-directories '("tcl" "tclsh")))

    (package
      (name "tree-sitter-tcl")
      (version guix-version)
      (source (origin
                (method hg-fetch)
                (uri (hg-reference
                      (url "https://hg.sr.ht/~cdrozak/tree-sitter-tcl")
                      (changeset changeset-full)))
                (file-name (hg-file-name name version))
                (sha256
                 (base32
                  "1skixfrk3rznnc8a3kj7l5188f357y0xymiam5yrmcdfkjg2crp0"))
                ;; (snippet
                ;;  (tree-sitter-delete-generated-files grammar-directories))
                ))
      ;; (build-system gnu-build-system)
      (build-system tree-sitter-build-system)
      (arguments (list #:grammar-directories grammar-directories))
      (home-page "https://hg.sr.ht/~cdrozak/tree-sitter-tcl")
      (synopsis "Tree-sitter TCL grammar")
      (description "This package provides a TCL grammar for the Tree-sitter library.")
      (license license:asl2.0))))
