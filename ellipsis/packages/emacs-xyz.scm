(define-module (ellipsis packages emacs-xyz)
  #:use-module ((guix licenses) #:prefix license:)

  #:use-module (guix gexp)
  #:use-module (guix utils)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix build-system emacs)

  #:use-module (gnu packages base)
  #:use-module (gnu packages tls)

  #:use-module (srfi srfi-1))

;; open /etc/ssl/certs to test
(define-public emacs-x509-mode
  (package
    (name "emacs-x509-mode")
    (version "20230119.838")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://melpa.org/packages/x509-mode-" version ".tar"))

              (sha256
               (base32
                "13l6arxg1igwsv8d2f9ihpcys7wm8vl4vqbllf3fqd1zyvfanx9l"))))
    (build-system emacs-build-system)
    (propagated-inputs `(("openssl" ,openssl)))
    (arguments
     `(#:include (cons* "^keywords.txt"
                        "^keyword\\+constant.txt"
                        "^constants.txt"
                        "^long-name.txt"
                        %default-include)))
    (home-page "https://github.com/jobbflykt/x509-mode")
    (synopsis "Major mode for viewing certificates, CRLs, and other PKI-related files")
    (description "Major mode for viewing certificates, CRLs, and other PKI-related files.

Uses OpenSSL for viewing PEM and DER encoded PKI entities.")
    (license license:expat)))

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
