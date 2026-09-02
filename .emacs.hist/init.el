;; -*- lexical-binding: t; -*-

(setopt dc/emacs-modules (expand-file-name "modules" (file-truename user-emacs-directory)))
(add-to-list 'load-path dc/emacs-modules)

(let* ((pardir (file-name-directory
                (directory-file-name (expand-file-name (file-truename user-emacs-directory)))))
       (console (expand-file-name ".emacs.console" pardir)))
  (load-file (expand-file-name "init.el" console)))

(use-package sqlite :demand t)
(use-package browser-hist
  :init (require 'embark)
  :config (setq browser-hist-default-browser 'firefox)
  :commands (browser-hist-search))
