(define-module (ellipsis packages tree-sitter)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages)
  #:use-module (gnu packages base)
  ;;  #:use-module (gnu packages crates-graphics)
  ;;  #:use-module (gnu packages crates-io)
  #:use-module (gnu packages node)
  #:use-module (gnu packages tree-sitter)
  ;; #:use-module (guix build-system cargo)
  ;; #:use-module (guix build-system gnu)
  #:use-module (guix build-system tree-sitter)
  #:use-module (guix gexp)
  #:use-module (guix download)
  #:use-module (guix git-download)
  ;; #:use-module (guix hg-download)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (srfi srfi-1))

(define-public tree-sitter-hyprlang
  ((@@ (gnu packages tree-sitter) tree-sitter-grammar)
   "hyprlang" "Hyprlang"
   "171p3hj36a1jqflg9xv138445j4m4m16na6bgpm1km3l67jhvl54"
   "3.1.0"
   #:repository-url
   "https://github.com/tree-sitter-grammars/tree-sitter-hyprlang"))

