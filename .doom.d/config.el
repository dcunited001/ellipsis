;;; $DOOMDIR/config.el -*- lexical-binding: t; mode: emacs-lisp -*-



;;* Basics

(defvar dw/is-guix-system (and (eq system-type 'gnu/linux)
                               (with-temp-buffer
                                 (insert-file-contents "/etc/os-release")
                                 (search-forward "ID=guix" nil t))
                               t))

;;** UI

(menu-bar-mode)

(setq doom-theme 'modus-vivendi
      display-line-numbers-type nil)

;;*** Font 

(if dw/is-guix-system
    (setq doom-font (font-spec :family "Iosevka Nerd Font Mono" :size 14 :weight 'normal)
          doom-serif-font (font-spec :family "Iosevka Nerd Font Mono" :size 14 :weight 'normal))
  ;; (setq doom-font (font-spec :family "Noto Sans Mono" :size 12 :weight 'normal)
  ;;       doom-serif-font (font-spec :family "Noto Serif" :size 12 :weight 'normal))
  (setq doom-font (font-spec :family "FiraCode Nerd Font Mono" :size 12 :weight 'normal)
        doom-serif-font (font-spec :family "SourceCodeVF" :size 12)))

;; doom-symbol-font
;; doom-big-font
;; doom-variable-pitch-font

;;*** Dired

(setq dired-omit-files "^.DS_Store\\'\\|^.project\\(?:ile\\)?\\'\\|^.\\(svn\\)\\'\\|^.ccls-cache\\'\\|\\(?:\\.js\\)?\\.meta\\'\\|\\.\\(?:elc\\|o\\|pyo\\|swp\\|class\\)\\'")

;;** Org
;; Dear god no

;;* Dev
;;** Tree Sitter

(setq treesit-extra-load-path
      (list (expand-file-name ".local/lib/tree-sitter" (getenv "HOME")))
      treesit-language-source-alist
      '((yaml . ("https://github.com/ikatyang/tree-sitter-yaml"))
        (astro . ("https://github.com/virchau13/tree-sitter-astro"))))

(after! treesit
  (cl-loop for lang-key
           in (a-keys treesit-language-source-alist)
           unless (treesit-language-available-p lang-key)
           do (treesit-install-language-grammar lang-key)))

;;** LSP

;; M-x lsp-describe-sessions
;; M-x lsp-workspace-show-log

(setq-default lsp-keep-workspace-alive nil)

;; (after! lsp-ui
;;         (setq lsp-ui-doc-enable t))

;;** Emacs Lisp

(add-hook 'lisp-data-mode-hook 'lispy-mode)

;;** Web

;;*** HTML

(use-package! html-ts-mode
              :mode "\\.html?\\'"
              :config (add-to-list 'major-mode-remap-alist
                                   '(mhtml-mode . html-ts-mode)))

(use-package apheleia
  :config
  (dolist (webml '(html-mode html-ts-mode mhtml-mode web-mode))
    ;; aphelia: prettier instead of prettier-html, so it relies on .prettierrc
    (add-to-list 'apheleia-mode-alist `(,webml . prettier))))

;;*** Tailwind

;; see https://emacs-lsp.github.io/lsp-mode/page/faq/#i-have-multiple-language-servers-registered-for-language-foo-which-one-will-be-used-when-opening-a-project

(use-package! lsp-tailwindcss
              :init (setq lsp-tailwindcss-add-on-mode t
                          ;; set in .dir-locals.el, along with lsp-disabled-servers
                          ;; ... or just use it for rustywind (i can 't get it to work)
                          lsp-tailwindcss-major-modes nil))

;;** Langs

(setq-default treesit-extra-load-path
              (list (expand-file-name ".local/lib/tree-sitter" (getenv "HOME"))))

;;*** Astro

(use-package astro-ts-mode
  :mode "\\.astro\\'")

;;* Keys

;;** Global Remaps
(global-set-key (kbd "C-<prior>") #'tab-bar-switch-to-tab)
(global-set-key (kbd "C-<next>") #'tab-bar-switch-to-recent-tab)

;;** Search Map

(map! :map 'search-map
      "d" #'consult-find
      "D" #'consult-locate
      ;; "M-d" #'consult-dir-jump-file
      "g" #'consult-grep
      ;; "G" #'consult-git-log-grep
      "M-g" #'consult-git-grep
      "i" #'consult-info
      "k" #'consult-keep-lines
      "m" #'consult-man
      "r" #'consult-ripgrep
      "s" #'consult-line-multi          ; "L"
      "S" #'swiper
      ;; "M-s" #'consult-yasnippet
      ;; "M-S" #'consult-yasnippet-visit-snippet-file
      "u" #'consult-focus-lines

      ;; Isearch integration
      "e" #'consult-isearch-history)

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!

;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
;; (setq user-full-name "John Doe"
;;       user-mail-address "john@doe.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-symbol-font' -- for symbols
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
;;
;;(setq doom-font (font-spec :family "Fira Code" :size 12 :weight 'semi-light)
;;      doom-variable-pitch-font (font-spec :family "Fira Sans" :size 13))
;;
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!
;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")


;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.
