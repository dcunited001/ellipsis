(define-module (ellipsis packages emacs-xyz)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix gexp)
  #:use-module (guix utils)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix build-system emacs)
  #:use-module (gnu packages)
  #:use-module (gnu packages emacs-xyz)
  #:use-module (srfi srfi-1))

(define-public emacs-citar
  (package
    (name "emacs-citar")
    (version "1.4.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/bdarcus/citar")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "07q94iplkx29lggrs5xfzj42rxfcn2cnbr90jgifk29jshcz30pv"))))
    (build-system emacs-build-system)
    (arguments
     (list
      #:tests? #true
      #:test-command #~(list "emacs" "--batch" "-L" "."
                             "-l" "test/citar-test.el"
                             "-l" "test/citar-file-test.el"
                             "-l" "test/citar-format-test.el"
                             "-f" "ert-run-tests-batch-and-exit")
      #:phases #~(modify-phases %standard-phases
                   (add-before 'build 'set-home
                     (lambda _
                       (setenv "HOME" "/tmp")))
                   ;;;; XXX: The following phase reports bogus errors. Suppress
                   ;;;; it for now.
                   ;;(delete 'validate-compiled-autoloads)

		   )))
    (propagated-inputs (list emacs-auctex
                             emacs-citeproc-el
                             emacs-embark
                             emacs-org
                             emacs-parsebib
                             emacs-s))
    (home-page "https://github.com/bdarcus/citar")
    (synopsis "Emacs package to quickly find and act on bibliographic entries")
    (description
     "This package provides a completing-read front-end to browse and
act on BibTeX, BibLaTeX, and CSL JSON bibliographic data, and LaTeX,
markdown, and Org cite editing support.

When used with Vertico (or Selectrum), Embark, and Marginalia, it
provides similar functionality to helm-bibtex and ivy-bibtex: quick
filtering and selecting of bibliographic entries from the minibuffer,
and the option to run different commands against them.

With Embark, it also makes available at-point actions in Org
citations.")
    (license license:gpl3+)))

;; TODO: maybe implement an ox-ssh package?

;; https://github.com/jobbflykt/x509-mode
;; https://guix.gnu.org/manual/en/guix.html#emacs_002dbuild_002dsystem

;; `(#:phases
;;   (modify-phases %standard-phases
;;     (add-after 'install 'install-keywords
;;       (lambda* (#:key inputs outputs #:allow-other-keys)
;;         (let* ((out (assoc-ref outputs "out"))
;;                (site-lisp (string-append out "/share/emacs/site-lisp"))
;;                (libdir (string-append site-lisp "/x509-mode-" version)))
;;           (copy-file "keyword.txt"))))))
