(define-module (ellipsis packages emacs-xyz)
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
