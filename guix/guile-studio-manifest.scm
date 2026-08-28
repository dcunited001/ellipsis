;;; Welcome to Guile Studio!
;;; Type your Guile program here and evaluate it.
(use-modules (guix packages)
             (guix profiles)
             (guix transformations)
             (gnu packages base)
             (gnu packages guile)
             (gnu packages emacs)
             (gnu packages emacs-xyz)
             (gnu packages guile-xyz))

(define (rewrite-emacs-inputs-to-pgtk)
  ;; Ensure guile-studio matches its build
  (package-input-rewriting/spec
   `(("emacs" . ,(const emacs-pgtk)))))

;; (let ((gstudio (const guile-studio))))

(packages->manifest
 (list emacs-pgtk
       ((rewrite-emacs-inputs-to-pgtk) guile-studio))) 

;; packages->development-manifest
;; https://guix.gnu.org/manual/devel/en/guix.html#index-package_002d_003edevelopment_002dmanifest
;; packages->build-system-manifest
;; https://guix.gnu.org/manual/devel/en/guix.html#index-build_002dsystem_002d_003emanifest
