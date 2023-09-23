;; -*- guix-use-substitutes: nil -*-
(define-module (ellipsis packages emacs)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system glib-or-gtk)
  #:use-module (gnu packages)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages base)
  #:use-module (gnu packages emacs)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages image)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages texinfo)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages webkit)
  #:use-module (gnu packages xml)
  #:use-module (guix utils)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1))

;; build-system: glib-or-gtk-build-system (or gnu-build-system if emacs-no-x ,etc)
;;
;; this build system extends gnu-build-system to add two phases executed after
;; install (glib-or-gtk-wrap and glib-or-gtk-compile-schemas)
;;
;; add-after 'glib-or-gtk-wrap 'restore-emacs-pdmp
;; add-after 'glib-or-gtk-wrap 'strip-double-wrap

;; configure flags ==================================================
;; "--with-modules"
;; "--with-cairo"
;; "--with-native-compilation"
;; "--disable-build-details"
;;
;; --with-pgtk
;;
;; --with-xwidgets (if extending from emacs-next-pgtk-xwidgets)

;; make flags ==================================================
;; "NATIVE_FULL_AOT=1"
;;

;; CFLAGS ==================================================
;;
;; from etc/DEBUG: "CFLAGS='-O0 -g3'"
;;
;; recommended: "CFLAGS='-O0 -g"
;;
;; -Wall will show all warnings
;; -Werror will make all warnings errors
;; -w will suppress all compile-time warnings

;; (package/inherit)

(define-public emacs-next-pgtk-debug
  (package
    (inherit emacs-next-pgtk)
    (name "emacs-next-pgtk-debug")
    (source
     (origin
       (inherit (package-source emacs-next-pgtk))
       (patches
        (append (search-patches "emacs-pgtk-super-key-fix.patch")
                (origin-patches (package-source emacs-next-tree-sitter))))))
    (arguments
     (substitute-keyword-arguments
         ;; I think it's just easier to do it this way for the scalar params
         ;; that's how the guile-emacs package uses it.
         ;;
         ;; I'm still not fully grokking the (sub-kw-args _ ...) source
         ;;
         ;; too much _ from clojure in ... scheme like ??? hmm wut recursion?
         ;;
         ;; it does make for very readable implementations though.
         `(#:strip-binaries? #f
           ,@(package-arguments emacs-next-pgtk))
       ((#:make-flags flags #~'())
        #~(cons* "CFLAGS=-O0 -g -w" #$flags))
       ((#:configure-flags flags #~'())
        #~(cons* "--enable-checking=yes,glyphs"
                 "--enable-check-lisp-object-type"
                 #$flags))))
    (synopsis "Emacs text editor with @code{pgtk} and @code{tree-sitter} support")
    (description "This Emacs build implements graphical UI purely in terms
of GTK and supports tree-sitter.")))

;; TODO remove strip-double-wrap (without affecting wrap-emacs-paths?)
;;  these are not compiliation/linking phases. the are install phases.
