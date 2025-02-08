;;; auto-insert.el --- Doom Emacs auto-insert customization  -*- lexical-binding: t; -*-

;; Copyright (C) 2025  David Conner

;; Author: David Conner <aionfork@gmail.com>
;; Keywords: languages

;; This don't work too good

;;*** Auto Insert

(use-package! autoinsert
  :config
  (setq auto-insert-directory
        (expand-file-name ".dotfiles/.emacs.d/insert" (getenv "HOME")))
  :hook (doom-init-ui-hook . #'auto-insert-mode))

;; TODO: tweak auto insert paths/modules/contents (for packages & guix home)

(defun yas-expand-current-buffer ()
  "Expand all yasnippet snippets in a current buffer."
  (interactive)
  (yas-expand-snippet (buffer-string) (point-min) (point-max)))

;;**** Auto Insert Scheme & Guix

;; NOTE: auto-insert detects the file-path, errors if
;; yas-expand-current-buffer is nil, but fizzles silently if nothing else goes
;; wrong? dammit. this was working in ©1987 .... "well how's that get there?"

;; TODO: "guix/gnu/packages/package"
;; TODO: "guix/gnu/services/service"
;; TODO: "dotfiles/ellipsis/service"
;; TODO: "dotfiles/ellipsis/home/service"

;; (define-auto-insert
;;   (rx "dc/services/" (one-or-more (or alphanumeric "-")) ".scm" line-end)
;;   ["dotfiles/dc/service" yas-expand-current-buffer])

;; (define-auto-insert
;;   (rx "dc/home/services/" (one-or-more (or alphanumeric "-")) ".scm" line-end)
;;   ["dotfiles/dc/home/service" yas-expand-current-buffer])

;; NOTE: not using these for now

;; (define-auto-insert
;;   (rx "gnu/tests/" (one-or-more (or alphanumeric "-")) ".scm" line-end)
;;   ["guix/gnu/tests/test" yas-expand-current-buffer])

;; (define-auto-insert
;;   (rx "vm" (one-or-more (or alphanumeric "-")) ".scm" line-end)
;;   ["guix/gnu/system/examples/vm-inherit-image" yas-expand-current-buffer])
