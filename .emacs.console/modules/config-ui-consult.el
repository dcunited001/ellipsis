(use-package consult
  :demand t
  :custom
  ((enable-recursive-minibuffers t)
   (consult-async-min-input 3)
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

;;;;; Cape
(use-package cape
  :demand t
  :bind (("C-c p" . cape-prefix-map))
  :config
  (add-hook 'completion-at-point-functions #'cape-file)
  (add-hook 'completion-at-point-functions #'cape-elisp-block))

;;;;; Orderless
(use-package orderless
  :demand t
  :config
  (orderless-define-completion-style
      orderless+initialism
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

;;;;; Corfu
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

;;;;; Vertico
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

;;;;; Marginalia
(use-package marginalia
  :demand t
  :custom
  ((marginalia-annotators '(marginalia-annotators-heavy
                            marginalia-annotators-light
                            nil)))
  :config
  (marginalia-mode))

;;;;; Embark
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

(provide 'config-ui-consult)
