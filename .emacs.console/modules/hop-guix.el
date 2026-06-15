;;;; Guile

(use-package scheme-mode
  :mode ((rx "." (| "sld") eos) . scheme-mode))

(use-package geiser
  :demand t
  :custom
  ((geiser-default-implementation 'guile)
   (geiser-implementations-alist '(((regexp "\\.scm$") guile)))

   (geiser-active-implementations
    '(guile racket) "lispy-eval on scheme req. emacs-guile-racket loaded")

   ;; these need to be correct
   (geiser-repl-per-project-p t)
   (geiser-repl-add-project-paths t)

   (geiser-debug-always-display-sexp-after t)
   ;; (geiser-debug-long-sexp-lines 6)

   (geiser-debug-treat-ansi-colors
    'colors "Requires guile-colorized (ice-9 colorized)")

   (geiser-repl-highlight-output-p t)))

(use-package geiser-guile
  :demand t
  :after geiser
  :custom
  ((geiser-guile-manual-lookup-other-window t) ;; default: nil
   ;; geiser-guile-extra-keywords nil
   (geiser-guile-show-debug-help t)
   (geiser-guile-warning-level 'medium))

  ;; The Paths are added to both %`load-path' and %load-compiled path,
  ;; and only if they are not already present. (in .dir-locals.el)
  ;; geiser-guile-load-path

  ;; NOTE: it loads geiser-guile, even _without_ the emacs-geiser-guile package
  ;; i had compat. issues with this about 6 months ago (just in case)

  :config
  (add-to-list 'geiser-guile-manual-lookup-nodes "Geiser")
  (add-to-list 'geiser-guile-manual-lookup-nodes "Guile Reference")
  (add-to-list 'geiser-guile-manual-lookup-nodes "Guile Library")
  (add-to-list 'geiser-guile-manual-lookup-nodes "Guix"))

;; (add-to-list 'dc/org-babel-load-languages '(scheme . t))

;; fix for lispy
(defun geiser-racket--language () 'racket)

;;;; Guix
(use-package guix
  ;; demand for now, for `guix-pulled-profile' (not `guix-scheme-mode') this
  ;; will load geiser/guile at start. (require 'guix-ui) is necessary, otherwise
  ;; guix-pulled-profile doesn't exist idk....

  :demand t
  :init (require 'ffap)
  :bind ((:map dc/quick-map
               ("g <SPC>" . #'guix)
               ("gX" . #'guix-extended-command)
               ("gh" . #'guix-hash)
               ("gsb" . #'guix-switch-to-buffer)
               ("gsr" . #'guix-switch-to-repl)))
  ;; (setopt guix-devel-ffap-patch-directories (flatten-list (list guix-pulled-profile "patches")))
  :config
  (require 'guix-ui))

(provide 'config-guix)
