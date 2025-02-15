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

;; TODO: PURGE: emacs-docker-tmp
(define-public emacs-docker-tmp
  (package
    (inherit emacs-docker)
    ;; (name "emacs-docker")
    (propagated-inputs
     (modify-inputs (package-propagated-inputs emacs-docker)
       (delete "emacs-docker-tramp")))))

;; for now on guix, this requires shimming js--treesit-sentence-nodes
;;
;; https://github.com/xhcoding/qml-ts-mode/issues/1
;;
;; https://github.com/emacs-mirror/emacs/blame/5c43ef86bf169a79b87bd082d2f884757f7c2efc/lisp/progmodes/js.el#L3805-L3827
;;
;; NOTE: my version of emacs is missing this line (and likely other js
;; keywords for treesitter
(define-public emacs-qml-ts-mode
  (let* ((github-repo "https://github.com/xhcoding/qml-ts-mode")
         ;; update version once next release is ready
         (version "0.0.1")
         (revision "1")
         (commit "2db1a798cd320d37c6031bc4583199524e24cc0b"))
    (package
      (name "emacs-qml-ts-mode")
      (version (git-version version revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url github-repo)
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256 (base32 "1rx51ndpllgrkm291qhvnsgz1rj5wjkbr9pvcm1lnw3kb0kw1qbc"))))
      (build-system emacs-build-system)
      (home-page github-repo)
      (synopsis "QML major mode using treesit")
      (description "qml-ts-mode is major-mode for editing Qt Declarative (QML) code.")
      ;; TODO: correct the license (project has none)
      (license license:expat))))

;; (define-public emacs-docker-tmp
;;   (package
;;     (inherit emacs-docker)
;;     (name "emacs-docker-tmp")
;;     (propagated-inputs
;;      (modify-inputs (package-propagated-inputs emacs-docker)
;;        (delete "emacs-docker-tramp")))))
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
