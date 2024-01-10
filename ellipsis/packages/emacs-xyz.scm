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

(define-public emacs-docker-tmp
  (package
    (inherit emacs-docker)
    ;; (name "emacs-docker")
    (propagated-inputs
     (modify-inputs (package-propagated-inputs emacs-docker)
       (delete "emacs-docker-tramp")))))

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
