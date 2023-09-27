(define-module (ellipsis packages tree-sitter)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages)
  #:use-module (gnu packages crates-graphics)
  #:use-module (gnu packages crates-io)
  ;; #:use-module (gnu packages icu4c)
  #:use-module (gnu packages node)
  #:use-module (guix build-system cargo)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system tree-sitter)
  #:use-module (guix gexp)
  #:use-module (guix hg-download)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix utils))

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
