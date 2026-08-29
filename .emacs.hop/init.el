;; -*- lexical-binding: t; -*-

;;; Emacs

;;;; Config and Load Path
(setopt dc/emacs-modules (expand-file-name "modules" user-emacs-directory))
(add-to-list 'load-path dc/emacs-modules)

;; if the elpa gpg-agent interferes with smartcard, this may fix it, but will
;; require re-syncing the ~/.emacs.d/.local/elpa/gpg keyring
;; (setq package-gnupghome-dir nil)

;;;; Custom.el
(setq use-package-enable-imenu-support t
      custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)

;;;; package.el

;; maybe prevent from loading
(when package-enable-at-startup
  (require 'package)
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/")))

;;;; Ensure packages are available

;; otherwise stop loading: when some of these are half-loaded, it causes
;; issues with emacs-lisp compilation (lispy somehow does this, as do the
;; symbols scheme-mode and geiser-mode if they don't exist yet
(require 'find-func)
(unless (find-library-name "a")
  (user-error "Fix emacs packages."))

(unless (and (find-library-name "nix-ts-mode") (find-library-name "magit"))
  (user-error "Fix emacs packages: still missing magit, nix-ts-mode or tree-sitter-nix."))

;;;; Essential Libs
(use-package a :demand t)

;;;; No Littering
(use-package no-littering :demand t)

;;;; Editor

(indent-tabs-mode -1)

(use-package ibuffer
  :config
  (global-set-key [remap list-buffers] #'ibuffer))

(global-set-key (kbd "C-M-S-<return>") #'duplicate-line)
(global-set-key (kbd "C-x M-f") #'find-file-at-point)

;;;; Tabs

(use-package tab-bar :defer t
  :config
  (global-set-key (kbd "C-<prior>") #'tab-bar-switch-to-tab)
  (global-set-key (kbd "C-<next>") #'tab-bar-switch-to-recent-tab))

;;;; Look & Feel

(add-to-list 'emacs-startup-hook (lambda () (load-theme 'modus-vivendi)))

;;; Lang
(use-package x509-mode :defer t)
(use-package xkb-mode :defer t)
(use-package nix-mode :defer t)
(use-package json-mode :defer t)
;; (use-package yuck-mode :defer t)
(use-package toml-mode :defer t)
(use-package ace-window
  :demand t
  :custom
  (aw-background . nil)
  :config
  (setopt aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  (define-key global-map [remap other-window] #'ace-window))

;;;; Make

(setq-default tab-width 4)
(use-package make-mode
  :defer t
  :hook (makefile-mode . (lambda () (setq-local tab-width 4))))

;; (use-package hyprlang-ts-mode)

(winner-mode)

;;;; LSP

(use-package eglot
  :defer t
  :config
  (unless (member '(nix-mode "nixd") eglot-server-programs)
    (setq eglot-server-programs (delq 'nix-mode eglot-server-programs))
    (add-to-list 'eglot-server-programs '(nix-mode "nixd")))
  ;; (pop nix-mode-hook)
  :hook (nix-mode . eglot-ensure))


;; =============================================
;; MIRRORS .EMACS.CONSOLE (... mostly)

;;; User
(setopt user-mail-address (or (getenv "EMAIL") "aionfork@gmail.com"))
(setopt epg-user-id user-mail-address)

;;; Use Package
(setopt use-package-enable-imenu-support t)

;;; Files

(setq backup-by-copying nil
      make-backup-files nil
      ;; create-lockfiles nil
      auto-save-default nil )

;;; UI
(setopt global-auto-revert-non-file-buffers t
	auto-revert-verbose nil
	display-line-numbers-type nil)

(setq dired-omit-files "^.DS_Store\\'\\|^.project\\(?:ile\\)?\\'\\|^.\\(svn\\)\\'\\|^.ccls-cache\\'\\|\\(?:\\.js\\)?\\.meta\\'\\|\\.\\(?:elc\\|o\\|pyo\\|swp\\|class\\)\\'")

;;;; Confirmations
(require 'hop-ui-confirm)

;;;; Completion
;; see consult-preview-key (arrow keys suck in console)

;;;;; Consult
(require 'hop-ui-consult)

;;;; Follow Mode
(use-package follow
  :demand t
  :bind ((:map ctl-x-map ("M-f" . 'follow-mode))))

;;; Lisp

(require 'scheme)
(use-package geiser :defer t)
(use-package lispy
  :defer t
  :after (scheme-mode geiser-mode)
  :hook '((emacs-lisp-mode scheme-mode geiser-mode)))

;;; Org
(require 'hop-org)

;;; Tool
(require 'hop-vcs)
(require 'hop-tramp)

;;; Final Setup

;;;; Keys
(require 'hop-keys-unbind)
(require 'hop-keys)

(defun dc/set-bars ()
  (interactive)
  (progn
    (menu-bar-mode -1)
    (scroll-bar-mode -1)
    (tool-bar-mode -1)))

(add-to-list 'emacs-startup-hook #'dc/set-bars)
