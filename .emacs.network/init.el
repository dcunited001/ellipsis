;;; init.el --- An Emacs Config for Networking -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2022 David Conner
;;
;; Author: David Conner <https://github.com/dc>
;; Maintainer: David Conner <noreply@te.xel.io>
;; Created: February 03, 2022
;; Modified: February 03, 2022
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/dc/init
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;; This is the start to an Emacs config to help manage my local network. It's
;;  sparse for now, but will stay as close to vanilla Emacs as possible.
;;
;;; Code:

;;* Emacs Config

;;(message (concat "CURRENT BUFFER: " (current-buffer)))
;; default-directory

;;** load-path
;; TODO: use something like ruby's __FILE__ (default-directory returns ~/ when invoked by i3 shell & chemacs)
(setq load-path
      (append (list
               (directory-file-name (concat (file-name-as-directory (getenv "HOME")) ".emacs.network/lisp"))
               (directory-file-name (concat (file-name-as-directory (getenv "HOME")) ".emacs.network/lisp/quelpa")))
              load-path))

;;** Main Configs

(setq make-backup-files nil)

;;** Better Defaults

;; TODO: Convert from melpa to quelpa
;; NOTE: i'm already mostly using guix for emacs packages

;;** Discover My Major
(define-key global-map (kbd "C-h <f5>") #'discover-my-major)
;; (define-key global-map (kbd "C-h S-<f5>") #'discover-my-mode)
 

;;** MELPA

(require 'package)
(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/"))
;; TODO determine whether this is necessary
;; (setq package-selected-packages '(ios-config-mode quelpa ansible))

;;** Quelpa
;; NOTE: load-path is set. into ./lisp, curl quelpa or clone it
(require 'quelpa)

;;** Theme
;;TODO: fix this
(require 'modus-themes)
(add-hook 'window-setup-hook
          (lambda () (progn
                       (modus-themes-load-themes)
                       (modus-themes-load-vivendi))
            ))

;;** Lispy
(setq lispy-compat '(cider edebug))     ;; TODO: add geiser later
(add-hook 'emacs-lisp-mode-hook #'lispy-mode)
(add-hook 'clojure-mode-hook #'lispy-mode)
(add-hook 'scheme-mode-hook #'lispy-mode)
;; (add-hook 'emacs-lisp-hook #'lispy-mode)

;;** Cisco IOS Mode
(quelpa '(ios-config-mode :fetcher git :url "https://github.com/mnewt/IOS-config-mode"))

;;** Modus Themes

;;** Guix
(define-key global-map (kbd "<f5>") #'guix)

;;** Geiser

;;** ORG
(setq org-directory "~/org"
      org-adapt-indentation nil
      org-src-preserve-indentation t)

;;** TRAMP

;; TODO: find a better place to define this:

;; faster than default SCP method
;;   (org-babel may be hooking into this)

(setq tramp-default-method "ssh")

(setq tramp-connection-properties '())

(add-to-list 'tramp-connection-properties
	     (list ".*"
		   ;; "remote-shell" "/bin/ash"
		   "tmpdir" "/jffs/tmp")
	     ;; (list "/
	     (list "/ssh.:(charon|hekate)\\.via\\.zxc:"
		   ;; "remote-shell" "/bin/ash"
		   "tmpdir" "/jffs/tmp"))

;;** BABEL

(org-babel-do-load-languages 'org-babel-load-languages
			     '((shell . t)
			       (scheme . t)))

;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages '(quelpa ios-config-mode)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
