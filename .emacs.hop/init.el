(setq use-package-enable-imenu-support t
	  org-src-preserve-indentation t
      backup-by-copying nil
      make-backup-files nil
	  custom-file (expand-file-name "custom.el" "~/.emacs.d"))
(load custom-file)

(use-package a :demand t)

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))

(use-package no-littering)

(use-package ibuffer
  :config
  (global-set-key [remap list-buffers] #'ibuffer))

(setq-default tab-width 4)
(use-package make-mode
  :defer t
  :hook (makefile-mode . (lambda () (setq-local tab-width 4))))

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

;; (use-package consult
;; (use-package consult-dir
;; (use-package cape
;; (use-package orderless
;; (use-package corfu
;; (use-package corfu-terminal
;; (use-package vertico
;; (use-package marginalia
;; (use-package embark
;; (use-package lispy
;;;; (use-package embark-consult
;;;; (use-package follow
;;;; (use-package scheme-mode
;;;; (use-package geiser
;;;; (use-package geiser-guile
;;;; (use-package guix
;;;; (use-package tramp
