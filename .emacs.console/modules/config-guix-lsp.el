;;; config-guix-lsp.el --- Configure Guix LSP -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2026 David Conner
;;
;; Author: David Conner <aionfork@gmail.com>
;; Maintainer: David Conner <aionfork@gmail.com>
;; Created: June 15, 2026
;; Modified: June 15, 2026
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex text tools unix vc
;; Homepage: https://github.com/dcunited001/config-guix-lsp
;; Package-Requires: ((emacs "29.1"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Configure Guix LSP
;;
;;; Code:

(require 'find-func)

;; (setq-default
;;  guix-lsp-extra-roots
;;  '(
;;    "~/.dotfiles/.guix-profile-dev/share/guile/site/3.0"
;;    "~/.dotfiles/ellipsis"
;;    "~/.dotfiles/dc"))

;; (add-to-list ' scheme-mode-hook #'guix-scheme-mode)

(defun my/config-guix-lsp ()
  (interactive)
  (unless
      (ignore-errors (find-library-name "guix-lsp-eglot"))
    (if-let* ((some-load-path (read-directory-name "guix-lsp-eglot.el load path: " nil nil t))
              (some-load-path (and (file-exists-p some-load-path) some-load-path)))
        (add-to-list 'load-path some-load-path)
      (warn "guix-lsp-eglot.el load path: some thing hap pen some where.")))
  
  (unless (executable-find "guix-lsp")
    (if-let* ((some-executable (read-file-name "guix-lsp executable: " nil nil t))
              (some-executable (file-exists-p some-executable)))
        (setq-default guix-lsp-executable some-executable)
      (warn "guix-lsp: could not find executable")))

  (message "Remeber to set guix-lsp-extra-roots to list of guile module roots"))

(defun my/add-guix-lsp-extra-roots ()
  (interactive)
  (if-let* ((some-guix-path (read-directory-name "add directory to guix-lsp-eglot-extra-roots: " nil nil t))
            (some-guix-path (and (file-exists-p some-guix-path) some-guix-path))
            (some-guix-path (expand-file-name (directory-file-name some-guix-path))))
      (progn
	(add-to-list 'guix-lsp-extra-roots some-guix-path)
        ;; TODO: get guix-compiled-path?
        (add-to-list 'guix-load-path some-guix-path)
        ;; it doesn't match this for some reason
	(add-to-list 'auto-mode-alist
		     `(,(format "%s.*\\.scm\\'" some-guix-path) . guix-scheme-mode)))
    (user-error "guix-lsp: could not find directory")))

;; https://codeberg.org/trevarj/guix-lsp
;;
;; https://codeberg.org/trevarj/guix-lsp/src/branch/master/editors/emacs/guix-lsp-eglot.el
;;
;; Local checkout setup:
;;   guix shell -m manifest.scm -- make install
;;
;; Emacs setup:
;;   (add-to-list 'load-path "/path/to/guix-lsp/editors/emacs")
;;   (require 'guix-lsp-eglot)
;;   (setq guix-lsp-executable "~/.local/bin/guix-lsp")
;;   (setq guix-lsp-extra-roots '("~/Workspace/guix"))
;;   (add-hook 'guix-scheme-mode-hook #'guix-lsp-eglot-ensure)

(defun my/load-guix-lsp ()
  (interactive)
  (unless (ignore-errors (find-library-name "guix-lsp-eglot"))
    (user-error "guix-lsp-eglot.el not found. run M-x my/config-guix-lsp or set load-path"))
  (unless (executable-find "guix-lsp")
    (user-error "guix-lsp not found. run M-x my/config-guix-lsp or set PATH"))
  
  (use-package guix-lsp-eglot
    :commands (guix-lsp-eglot-ensure guix-lsp-eglot-start guix-lsp-eglot-setup))

  (use-package guix-scheme :ensure nil))

(provide 'config-guix-lsp)
;;; config-guix-lsp.el ends here
