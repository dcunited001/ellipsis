(define-module (dc packages emacs-xyz)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix gexp)
  #:use-module (guix utils)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix build-system emacs)
  #:use-module (gnu packages)
  #:use-module (gnu packages emacs-build)
  #:use-module (gnu packages emacs-xyz)
  #:use-module (srfi srfi-1))

(define-public emacs-qml-ts-mode
  (let* ((github-repo "https://github.com/xhcoding/qml-ts-mode")
         ;; update version once next release is ready
         (version "0.0.1")
         (revision "2")
         (commit "b80c6663521b4d0083e416e6712ebc02d37b7aec"))
    (package
      (name "emacs-qml-ts-mode")
      (version (git-version version revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                       (url github-repo)
                       (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32 "079fj4vm8pyjfm62yba8r089rlhy725qm27b3fj4vx25s44vywjr"))))
      (build-system emacs-build-system)
      (home-page github-repo)
      (synopsis "QML major mode using treesit")
      (description
       "qml-ts-mode is major-mode for editing Qt Declarative (QML) code.")
      ;; TODO: correct the license (project has none)
      (license license:expat))))

(define-public emacs-browser-hist
  (let ((commit "aab0a364077bfbf5559085086545d30bbaf7ac5e")
        (revision "0"))
    (package
      (name "emacs-browser-hist")
      (version (git-version "0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
                (url "https://github.com/agzam/browser-hist.el")
                (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "04xmn00pvnzralw4y8j3ilf7lprv5h01kmasyxnnr99ndphs8q62"))))
      (build-system emacs-build-system)
      (arguments
       (list
        #:tests? #f                     ; No tests upstream.
        #:phases
        #~(modify-phases %standard-phases
            ;;  NOTE: it builds, but sqlite-{query,init} aren't found
            (add-after 'unpack 'patch-obsolete-functions
              (lambda _
                ;; Replace obsolete functions from 'cl.
                (substitute* "browser-hist.el"
                  (("\\(eval-when-compile \\(require 'cl-lib\\)\\)")
                   (string-append
                    "\n(declare-function sqlite-init \"sqlite\")\n(declare-function sqlite-query \"sqlite\")"))))))))
      (propagated-inputs (list emacs-request))
      (home-page "https://github.com/agzam/browser-hist.el")
      (synopsis "Search through browser history, in Emacs")
      (description
       "Browsers usually keep their history in a sqlite database, and it’s trivial to extract it. This package allows you to search through your browser history by URL and the Page Title.")
      (license license:gpl3))))
