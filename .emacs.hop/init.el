(setopt dc/emacs-modules (expand-file-name "modules" user-emacs-directory))
(add-to-list 'load-path dc/emacs-modules)

(setq use-package-enable-imenu-support t
	  org-src-preserve-indentation t
	  backup-by-copying nil
	  make-backup-files nil
	  custom-file (expand-file-name "custom.el" "~/.emacs.d"))
(load custom-file)

(indent-tabs-mode -1)

(require 'package)

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))

(use-package no-littering :demand t)
(use-package a :demand t)

(use-package ibuffer
  :config
  (global-set-key [remap list-buffers] #'ibuffer))

(setq-default tab-width 4)
(use-package make-mode
  :defer t
  :hook (makefile-mode . (lambda () (setq-local tab-width 4))))

;;; Lang
(use-package x509-mode :defer t)
(use-package xkb-mode :defer t)
(use-package nix-mode :defer t)
(use-package json-mode :defer t)
(use-package yuck-mode :defer t)
(use-package toml-mode :defer t)
(use-package ace-window
  :demand t
  :config
  (setopt aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  (define-key global-map [remap other-window] #'ace-window))

(winner-mode)

(use-package eglot
  :defer t
  ;; :config
  ;; already included
  ;; (add-to-list 'eglot-server-programs '(nix-mode . '("nil")))

  ;; (pop nix-mode-hook)
  :hook (nix-mode . eglot-ensure))

(use-package tab-bar :defer t
  :config
  (global-set-key (kbd "C-<prior>") #'tab-bar-switch-to-tab)
  (global-set-key (kbd "C-<next>") #'tab-bar-switch-to-recent-tab))

(global-set-key (kbd "C-M-S-<return>") #'duplicate-line)
(global-set-key (kbd "C-x M-f") #'find-file-at-point)

;; =============================================
;; MIRRORS .EMACS.CONSOLE (... mostly)

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
(require 'config-ui-confirm)

;;;; Completion
;; see consult-preview-key (arrow keys suck in console)

;;;;; Consult
(require 'config-ui-consult)

;;;; Follow Mode
(use-package follow
  :demand t
  :bind ((:map ctl-x-map ("M-f" . 'follow-mode))))

;;; Lisp

(use-package lispy
  :hook '(emacs-lisp-mode))

;;; Org
(require 'config-org)

;;; Tool
(require 'config-vcs)
(require 'config-tramp)

;;; Final Setup

;;;; Keys
(require 'config-keys-unbind)
(require 'config-keys)

(defun dc/set-bars ()
  (interactive)
  (progn
	(menu-bar-mode -1)
	(scroll-bar-mode -1)
	(tool-bar-mode -1)))
