(define-module (ellipsis packages tree-sitter)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages)
  #:use-module (gnu packages base)
  #:use-module (gnu packages crates-graphics)
  #:use-module (gnu packages crates-io)
  #:use-module (gnu packages node)
  #:use-module (gnu packages tree-sitter)
  #:use-module (guix build-system cargo)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system tree-sitter)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix hg-download)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (srfi srfi-1))
;; #:use-module (gnu packages icu4c)

;; TODO: https://github.com/shopify/tree-sitter-liquid

;; does (hidden-package ...) suppress the package-name even if redefined?

;; ... or is there some problem with @@ syntax?
;;
;; ... or do I need (package/inherit ...)?
;;
;; ... or do I need (let ((base ts-hyprlang)) (package ...))
;; - doesn't seem to matter...
;;
;; damit geiser is a deadlock

;; ................. ok, you can't simply rename the package. I've screwed
;; around with this a bunch, but never explicitly answered that (smh...)
;;
;; - so rename the base package according to convention (it's just
;;   easier that way) and name the package override to ts-hyprlang
;;
;; like i would try to push some of this to upstream, but i just
;; get burnt out from screwing around spinning my tires & I don't
;; have time.

;; this one would build from the package-name tree-sitter-hyprlang
;; ... but would fail on check

(define tree-sitter-hyprlang
  ;; ... hidden-package does not do what I assumed it would. it seems to
  ;; prevent the inheriting package from building ... but `-e (@@ ...)` syntax
  ;; still works. a bit confusing
  ;;
  ;; (hidden-package
  ((@@ (gnu packages tree-sitter) tree-sitter-grammar)
   "hyprlang" "Hyprlang"
   "171p3hj36a1jqflg9xv138445j4m4m16na6bgpm1km3l67jhvl54"
   "3.1.0"
   #:repository-url
   "https://github.com/tree-sitter-grammars/tree-sitter-hyprlang")
  ;;)
  )

;; ;; keywords and defaults
;; #:commit v$Tag
;; #:repository-url (...formatted-github-ref...)
;; #:grammar-directories '(".")
;; #:article "a"
;; #:inputs (list)
;; #:get-cleanup-snippet tree-sitter-delete-generated-files
;; #:license license:expat


;; so now I can build with this, but I can't name the package in a manifest
;;
;; guix build -L ellipsis \
;; -e (@@ (ellipsis packages tree-sitter) ts-hyprlang)
;;
;; or with one @
;;
;; guix build -L ellipsis \
;; -e (@ (ellipsis packages tree-sitter) ts-hyprlang)

(define-public ts-hyprlang
  (let ((base tree-sitter-hyprlang))
    (package
      (inherit base)
      (name "ts-hyprlang")
      (arguments
       (substitute-keyword-arguments (package-arguments base)
         ;; (delete 'check) from phases doesn't work
         ((#:tests? _ #f) #f))))))

;; requires npm peerDependencies "tree-sitter": "^0.22.4"
;;
;; (define-public tree-sitter-tcl
;;   (let ((commit "8f11ac7206a54ed11210491cee1e0657e2962c47")
;;         (revision "0"))
;;   ((@@ (gnu packages tree-sitter) tree-sitter-grammar)
;;    "tcl" "TCL"
;;    "0dkxh6bn0kqn1gbyd5qwkg21fm634mxvas3w4ahv6zr5d8f95c96"
;;    (git-version "1.1.0" revision commit)
;;    #:commit commit
;;    #:repository-url
;;    "https://github.com/tree-sitter-grammars/tree-sitter-tcl")))
