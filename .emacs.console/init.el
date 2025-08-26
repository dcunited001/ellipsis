;; This profile is intended to run with the nonguix-install-amd image, where
;; they install by default to root's system profile.
;;
;; They should then be available whether the root or home-user runs:
;; emacs --init-directory ~/.dotfiles/.emacs.console
;;
;; ... before running it as root, copy to /root/.emacs.console, otherwise it
;; will clutter with root-owned cache files.
;;
;; Requires these packages (everything is :demand t ... bc it's just simpler)
;;
;; (define emacs-packages
;;   (list emacs-x509-mode
;;         emacs-better-defaults
;;         ;; emacs-with-profile
;;         emacs-auto-complete
;;         emacs-a
;;         emacs-hydra
;;         emacs-modus-themes
;;         emacs-dash
;;         emacs-lispy
;;         emacs-geiser
;;         emacs-geiser-guile
;;         emacs-ac-geiser
;;         emacs-guix
;;         emacs-yasnippet
;;         emacs-yasnippet-snippets

;;         ;; added "nice to have" packages, which should not normally be
;;         ;; installed for root
;;         emacs-cape
;;         emacs-consult
;;         emacs-consult-dir
;;         ;; emacs-consult-flycheck
;;         emacs-corfu
;;         emacs-corfu-terminal
;;         emacs-embark
;;         emacs-marginalia
;;         emacs-orderless
;;         emacs-vertico))

(use-package a :demand t)
(require 'a)

;;; User
(setopt user-mail-address (or (getenv "EMAIL") "aionfork@gmail.com"))
(setopt epg-user-id user-mail-address)


;;; Use Package
(setopt use-package-enable-imenu-support t)

;;; UI
(setopt global-auto-revert-non-file-buffers t
        auto-revert-verbose nil
        display-line-numbers-type nil)

(setq dired-omit-files "^.DS_Store\\'\\|^.project\\(?:ile\\)?\\'\\|^.\\(svn\\)\\'\\|^.ccls-cache\\'\\|\\(?:\\.js\\)?\\.meta\\'\\|\\.\\(?:elc\\|o\\|pyo\\|swp\\|class\\)\\'")

;;;; Confirmations

(setq dired-deletion-confirmer 'y-or-n-p
      dired-confirm-shell-command 'y-or-n-p

      ;; dired-no-confirm '()

      ;; files/urs
      url-confirmation-func 'y-or-n-p
      url-cookie-confirmation 'y-or-n-p
      log-edit-confirm  'changed
      ;; log-edit-confirm 'yes-or-no-p ; defaults to 'changed

      ;; emacs
      confirm-kill-processes 'y-or-n-p
      confirm-kill-emacs 'yes-or-no-p

      ;; email
      message-confirm-send 'y-or-n-p

      ;; org
      org-table-fix-formulas-confirm nil ;; 'y-or-n-p ; no default is no

      ;; eglot

      ;; smerge-mode is lazy loaded, default: t
      smerge-change-buffer-confirm t)

;;;; Completion
;; see consult-preview-key (arrow keys suck in console)
(use-package consult
  :demand t
  :custom
  ((consult-async-min-input 3)
   (consult-async-input-debounce 0.3)
   (consult-async-input-throttle 0.5)
   (consult-grep-max-columns 116))      ; grep: < 1.5 * 80
  :config
  (consult-customize
   consult-theme :preview-key nil
   ;; Hide full buffer list by default (use "b" prefix)
   consult--source-buffer :hidden t :default nil)
  (setq consult-project-function #'consult--default-project-function
        consult-ripgrep-args
        (string-join (list consult-ripgrep-args "--hidden -g \"!/po\"") " "))
  (require 'consult-xref)
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref))

(setq-default consult-dir-sources
              '(consult-dir--source-bookmark
                consult-dir--source-default
                consult-dir--source-project
                consult-dir--source-recentf))
(use-package consult-dir
  :demand t
  :after consult

  :config
  (setq consult-dir-project-list-function #'consult-dir-project-dirs))

(use-package cape
  :demand t
  :bind (("C-c p" . cape-prefix-map))
  :config
  (add-hook 'completion-at-point-functions #'cape-file)
  (add-hook 'completion-at-point-functions #'cape-elisp-block))

(use-package orderless
  :demand t
  :config
  (orderless-define-completion-style orderless+initialism
    (orderless-matching-styles '(orderless-initialism
                                 orderless-literal
                                 orderless-regexp)))
  :custom
  ((read-buffer-completion-ignore-case t)
   (completion-styles '(orderless basic))
   (orderless-matching-styles '(orderless-prefixes orderless-regexp))
   (completion-ignore-case nil)
   (read-file-name-completion-ignore-case nil)
   (completion-category-overrides
    '((command (styles orderless+initialism))
      (function (styles orderless+initialism))
      (symbol (styles orderless+initialism))
      (variable (styles orderless+initialism))))))

(use-package corfu
  :demand t
  :bind ((:map corfu-map
			   ("C-f" . #'corfu-insert-separator)
			   ("'" . #'corfu-quick-complete)))
  :custom
  ((tab-always-indent 'complete)
   (text-mode-ispell-word-completion nil))
  :config (global-corfu-mode))

(use-package corfu-terminal
  :demand t
  :after corfu
  :config (corfu-terminal-mode +1))

(setq corfu-auto-delay 0.25
      corfu-auto-prefix 3
      corfu-cycle t
      corfu-auto t
      corfu-quit-no-match 'separator
      corfu-quit-at-boundary 'separator
      corfu-count 15
      corfu-min-width 15
      corfu-terminal-disable-on-gui t   ; default

      ;; the doom defaults
      global-corfu-modes '((not erc-mode circe-mode help-mode gud-mode vterm-mode) t)
      corfu-popupinfo-min-height 5
      corfu-popupinfo-max-height 15
      corfu-popupinfo-direction 'right  ; default list: '(right left down)
      corfu-popupinfo-delay '(1.0 0.5)

      corfu-preview-current nil)

(use-package vertico
  :demand t
  :custom
  ((vertico-cycle t)
   ;; resize-mini-frames t
   (resize-mini-windows t))
  :config
  (require 'vertico-buffer)
  (vertico-mode)

  ;; TODO: this is still purple
  ;; (custom-set-faces '(vertico-current ((t (:background "#3a3f5a")))))

  ;; To check whether the minor mode is enabled in the current buffer,
  ;; evaluate ‘(default-value 'vertico-multiform-mode)’.

  :hook
  (vertico-directory-tidy . rfn-eshadow-update-overlay-hook)
  ((vertico-mode vertico-multiform-mode vertico-mouse-mode) . emacs-startup-hook)
  (vertico-indexed-mode . vertico-mode))

(setq-default vertico-multiform-categories
              '((bookmark reverse grid)
                (buffer reverse grid)   ; works for ido
                (command reverse)
                (consult-compile-error buffer)
                (consult-compile-multi grid (vertico-grid-annotate . 20))
                ;; (consult-flymake-error)
                (consult-grep buffer)
                (consult-git-log-grep-result buffer)
                (consult-info reverse)
                ;; (consult-kmacro)
                (consult-location buffer)
                ;; (consult-imenu buffer)
                (consult-man reverse grid (vertigo-cycle . t))
                (consult-xref buffer)
                (environment-variable reverse grid)
                (expression reverse)    ; for repeat-complex-command

                (file reverse grid (vertico-grid-annotate . 20))
                ;; (file reverse grid)
                (imenu buffer)
                (info-menu reverse grid)
                (kill-ring reverse grid)
                (minor-mode reverse)
                (consult-org-heading reverse grid)
                ;; (symbol)
                ;; not sure what symbol-help category refers to
                (symbol-help reverse grid (vertico-grid-annotate . 20))
                (theme reverse grid)
                (unicode-name grid reverse)
                (t)))

(setq-default vertico-multiform-commands
              '(("flyspell-correct-*" grid reverse (vertico-grid-annotate . 20))
                (org-refile grid reverse indexed)
                (consult-yank-pop indexed)
                ;; (consult-lsp-diagnostics)
                ;; (consult-flycheck reverse)
                (consult-flymake reverse)))

(use-package marginalia
  :demand t
  :custom
  ((marginalia-annotators '(marginalia-annotators-heavy
                            marginalia-annotators-light
                            nil)))
  :config
  (marginalia-mode))

(use-package embark
  :demand t
  :after consult
  :bind (("C-." . embark-act)         ;; pick some comfortable binding
         ("C-;" . embark-dwim)        ;; good alternative: M-.
         ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'
  :config
  ;; Use Embark to show command prefix help
  (setq prefix-help-command #'embark-prefix-help-command))

(use-package embark-consult
  :demand t
  :after (embark consult)
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

;;;; Misc UI

(use-package ace-window
  :demand t
  :bind (("C-x o" . 'ace-window))
  :custom ((aw-background nil)))

(use-package follow
  :demand t
  :bind ((:map ctl-x-map ("M-f" . 'follow-mode))))

;;; Lisp

(use-package lispy
  :hook '((emacs-lisp-mode scheme-mode geiser-mode)))

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

;;; Org
(setq org-use-property-inheritance t
      org-log-done t
      org-list-allow-alphabetical t
      org-catch-invisible-edits 'smart
      org-export-with-sub-superscripts '{}
      ;; org-export-allow-bind-keywords t
      org-image-actual-width '(0.9)

      ;; from .emacs.guix
      org-capture-bookmark t ;; nil
      org-cycle-separator-lines 2
      org-eldoc-breadcrumb-separator " → "
      org-enforce-todo-dependencies t
      ;; startup
      org-startup-folded 'content
      org-startup-indented nil)

;;; Tools
(use-package tramp
  :demand t
  :config
  (require 'tramp-container)
  (cl-dolist (p '("~/.guix-profile/bin"
                  "~/.guix-profile"
                  "/run/current-system/profile/bin"
                  "/run/current-system/profile/sbin"))
    (add-to-list 'tramp-remote-path p))
  :custom ((tramp-default-method "ssh")))

;;; Final Setup

;;;; Keys

(defun dc/unbind-keys (key-names &optional keymap)
  (seq-do (lambda (key)
            (if keymap
                (unbind-key key keymap)
              (unbind-key key)))
          key-names))
(dc/unbind-keys
 '("<f2> 2" "<f2> b" "<f2> s" "<f2> <f2>"
   "<f10>" "M-<f10>" "<f11>"
   "<f3>" "<f4>"))

(defun dc/set-bars ()
  (interactive)
  (progn
    (menu-bar-mode -1)
    (scroll-bar-mode -1)
    (tool-bar-mode -1)))

;; (use-package tab-bar) ;; just forget compiled lambdas
(global-set-key (kbd "C-<prior>") 'tab-bar-switch-to-tab)
(global-set-key (kbd "C-<next>") 'tab-bar-switch-to-recent-tab)
(global-set-key (kbd "C-M-S-<return>") 'duplicate-line)
(global-set-key (kbd "C-x M-f") 'find-file-at-point)
