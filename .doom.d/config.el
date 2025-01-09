;;; $DOOMDIR/config.el -*- lexical-binding: t; mode: emacs-lisp -*-
;;* Basics

(require 'a)

;;** System Identification
(defvar dw/is-guix-system (and (eq system-type 'gnu/linux)
                               (with-temp-buffer
                                 (insert-file-contents "/etc/os-release")
                                 (search-forward "ID=guix" nil t))
                               t))

(defconst IS-MAC      (eq system-type 'darwin))
(defconst IS-LINUX    (memq system-type '(gnu gnu/linux gnu/kfreebsd berkeley-unix)))
(defconst IS-WINDOWS  (memq system-type '(cygwin windows-nt ms-dos)))
(defconst IS-BSD      (memq system-type '(darwin berkeley-unix gnu/kfreebsd)))
(defconst EMACS28+    (> emacs-major-version 27))
(defconst EMACS29+    (> emacs-major-version 28))
(defconst MODULES     (featurep 'dynamic-modules))
(defconst NATIVECOMP  (featurep 'native-compile))
(defconst DBUS_FOUND (not (null (getenv "DBUS_SESSION"))))

;;** Essential Vars

;;*** User

(setq-default user-full-name "David Conner"
              user-mail-address (or (getenv "EMAIL") "noreply@te.xel.io"))

;;*** Emacs Config

;; (defvar dc/emacs-d (expand-file-name "~/.emacs.g/") "TODO: docs.")
;; (defvar dc/emacs-cache (expand-file-name "~/.cache/emacs/") "TODO: docs.")
;; (defvar dc/emacs-dw (expand-file-name "dw" dc/emacs-d) "TODO: docs.")
;; (defvar dc/emacs-doom-modules (expand-file-name "doom/modules" dc/emacs-d))
;; (defvar dc/emacs-modules (expand-file-name "modules" dc/emacs-d) "TODO: docs.")

(setopt use-package-enable-imenu-support t)

;;*** Paths

(add-to-list 'load-path (expand-file-name ".dotfiles/.emacs.d/lisp" (getenv "HOME")))
(require 'dw-settings)

;; TODO no-littering?
;;\\(?:;[^#]\\|\\*+\\)
;;** UI

(add-hook! 'doom-init-ui-hook
  (lambda ()
    (menu-bar-mode +1)
    (scroll-bar-mode -1)
    (tool-bar-mode -1)))

(setq display-line-numbers-type nil)

;; doom--setq-outline-level-for-emacs-lisp-mode-h
;; doom--setq-outline-regexp-for-emacs-lisp-mode-h
;; doom--setq-tab-width-for-emacs-lisp-mode-h

;;*** Font 

;; (setopt doom-font (font-spec :family "Noto Sans Mono" :size 12 :weight 'normal)
;;       doom-serif-font (font-spec :family "Noto Serif" :size 12 :weight 'normal))
(if dw/is-guix-system
    (setopt doom-font (font-spec :family "Iosevka Nerd Font Mono" :size 14 :weight 'normal)
            doom-serif-font (font-spec :family "Iosevka Nerd Font Mono" :size 14 :weight 'normal))
  (setopt doom-font (font-spec :family "FiraCode Nerd Font Mono" :size 12 :weight 'normal)
          doom-serif-font (font-spec :family "SourceCodeVF" :size 12)))

;; doom-symbol-font
;; doom-big-font
;; doom-variable-pitch-font

;;*** Dired

(setq dired-omit-files "^.DS_Store\\'\\|^.project\\(?:ile\\)?\\'\\|^.\\(svn\\)\\'\\|^.ccls-cache\\'\\|\\(?:\\.js\\)?\\.meta\\'\\|\\.\\(?:elc\\|o\\|pyo\\|swp\\|class\\)\\'")

(dirvish-override-dired-mode -1)

;; TODO: get Doom to; not close all the direds(advice-add #'+doom-)

;;*** Theme
(use-package ef-themes :demand t :init (setopt doom-theme nil) :config (ef-themes-load-random 'dark))

;;* Org

(setq org-directory "/data/org")

;;** Bibtex

(after! ob
  (use-package google-translate
    :custom

    (google-translate-backend-method 'curl)

    ;; (:bind "C-T" #'my-google-translate-at-point)
    (defun google-translate--search-tkk ()
      (list 430675 2721866130))
    (defun my-google-translate-at-point ()
      "reverse translate if prefix"
      (interactive)
      (if current-prefix-arg
          (google-translate-at-point)
        (google-translate-at-point-reverse))))
  (use-package ob-translate)

  (use-package ob-mermaid
    ;; TODO :custom ob-mermaid-cli-path "~/.nix-profile/bin/mmdc"
    ))

;;** Roam
(after! org
  (use-package org-roam
    :custom
    (org-roam-directory (expand-file-name "roam" org-directory))))

;;* Dev
;;
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

;;** Lisp

;; fixes issues navigating by lispy outline, but with advice below,
;; +el-outline-regexp no longer overrides lispy-outline-level
(setq-hook! 'emacs-lisp-mode-hook
  +emacs-lisp-outline-regexp ";;\\(?:;[^#]\\|\\*+\\)"
  outline-regexp +emacs-lisp-outline-regexp)

;; lispy-outline: "^[ 	]*;;;\\(;*\\**\\) [^ 	\n]"
;;   doom: "^;;\\(?:;[^#]\\|\\*+\\)"
(use-package! lispy
  :defer t
  :config
  (setq lispy-outline "^;;\\(?:;[^#]\\|\\*+\\)")
  ;; removing advice fixes consult-outline
  (advice-remove #'lispy-outline-level #'+emacs-lisp-outline-level)
  :hook (lisp-data-mode . lispy-mode))

;; TODO: to implement for arel:
;;
;; + patch lispy-describe-inline to avoid the direct geiser reference
;; + add le-arel (or casify the logic in le-scheme).
;;   + implement behavior in lispy--eval-scheme-mode
;;   + casify lispy-goto-symbol-scheme
;; + customize lispy-goto-symbol-alist: add arel's repl mode & decouple scheme mode
;;
;; this should get you between 80% to -20% there.

;;*** Emacs Lisp

;;** Web

;;*** HTML

;;TODO Web: ensure emmet-mode is configured

(use-package html-ts-mode
  :mode "\\.html?\\'"
  :config (add-to-list 'major-mode-remap-alist
                       '(mhtml-mode . html-ts-mode)))

(use-package! apheleia
  :config
  (dolist (webml '(html-mode html-ts-mode mhtml-mode web-mode))
    ;; aphelia: prettier instead of prettier-html, so it relies on .prettierrc
    (add-to-list 'apheleia-mode-alist `(,webml . prettier))))

;;*** Tailwind

;; see https://emacs-lsp.github.io/lsp-mode/page/faq/#i-have-multiple-language-servers-registered-for-language-foo-which-one-will-be-used-when-opening-a-project

(use-package lsp-tailwindcss
  :init (setq lsp-tailwindcss-add-on-mode t
              ;; set in .dir-locals.el, along with lsp-disabled-servers
              ;; ... or just use it for rustywind (i can 't get it to work)
              lsp-tailwindcss-major-modes nil))


;;*** Astro

(use-package astro-ts-mode
  :mode "\\.astro\\'")

;;** Langs

;;*** Shell

(use-package flymake-shellcheck
  :commands flymake-shellcheck-load
  :config (add-hook 'sh-mode-hook 'flymake-shellcheck-load))

;; TODO: per-project vterm
;; https://github.com/doomemacs/doomemacs/blob/master/modules/term/vterm/autoload.el

(use-package vterm :defer t
  :custom
  (vterm-max-scrollback 1000)
  (vterm-set-bold-hightbright t))

;;* Tools

;;** Auth
;;
;;*** epa
(require 'epg)

;;*** pinentry
(setopt epg-pinentry-mode 'loopback)    ;; cancel/ask
(setopt epg-user-id user-mail-address)
;; (setq epg-debug t)

;;*** auth-source-pass
(use-package auth-source-pass
  :demand t
  :config (auth-source-pass-enable))

;;** Docs
;; (external docs: dash/tldr)
;;** Systems
;;*** Arch
;;*** Guix
;;**** recutils: manipulate guix cli output
(use-package! rec-mode
  :defer t
  :config
  (add-to-list 'dc/org-babel-load-languages '(rec . t))
  (require 'ob-rec)
  (dc/org-babel-do-load-languages))
;;*** Nix, ContainerD
;;** Unix
;; elf-mode, crontab-mode
(use-package syslog-mode
  :custom (syslog-setup-on-load t)
  :mode "\\(messages\\(\\.[0-9]\\)?\\|SYSLOG\\|\\.s?trace\\)\\'")

;; ("\\(messages\\(\\.[0-9]\\)?\\|SYSLOG\\|\\.s?trace\\)\\'" . syslog-mode)

;;** Linux
;;
;;** Network
;; password-store.el? pass.el?
;; terraform
;;*** SSH
;;**** Tramp
;;**** x509-mode
(use-package x509-mode
  ;; TODO x509-mode-hook: jumping straight to x509 doesn't work well
  ;; :hook (x509-mode-hook . (lambda () (call-interactively 'x509-dwim)))
  :mode (rx "." (| "pem" "cer" "der" "crt" "crl") eos))

;; ((rx "." (| "pem" "cer" "der" "crt" "crl") eos) . x509-mode)

;;** Services
;;
;;** Data
;;*** Structure
;;*** Database
;;*** Visualization
;; gnu plot, graphviz/plot, d2, mermaid, plantuml
;;** Misc

;;* Applications

(require 'dom)
(require 'esxml-query)

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

;; (server-start)

;;* GC Settings
;; updating tables in org mode is extremely slow with vc-gutter and gcmh
;; defaults
;; (setq gcmh-low-cons-threshold (* 4 (expt 2 20))
;;       gc-cons-threshold (* 16 (expt 2 20)))
;;
;; + doom doesn't set gc-cons-threshold, but low/high instead
;;
;; + trying to force it here, it helps some, but large tables still slow
;;
;; + The +lsp-optimization-mode will
