;; This profile is intended to run with the nonguix-install-amd image, where
;; they install by default to root's system profile.
;;
;; They should then be available whether the root or home-user runs:
;; emacs --init-directory ~/.dotfiles/.emacs.console
;;
;; ... before running it as root, copy to /root/.emacs.console, otherwise it
;; will clutter with root-owned cache files.
;;
;; Requires these packages (everything is :demand t ... bc it's just simpler)
;;
;; (define emacs-packages
;;   (list emacs-x509-mode
;;         emacs-better-defaults
;;         ;; emacs-with-profile
;;         emacs-auto-complete
;;         emacs-a
;;         emacs-no-littering
;;         emacs-magit
;;         emacs-hydra
;;         emacs-modus-themes
;;         emacs-dash
;;         emacs-lispy
;;         emacs-geiser
;;         emacs-geiser-guile
;;         emacs-ac-geiser
;;         emacs-guix
;;         emacs-yasnippet
;;         emacs-yasnippet-snippets

;;         ;; added "nice to have" packages, which should not normally be
;;         ;; installed for root
;;         emacs-cape
;;         emacs-consult
;;         emacs-consult-dir
;;         ;; emacs-consult-flycheck
;;         emacs-corfu
;;         emacs-corfu-terminal
;;         emacs-embark
;;         emacs-marginalia
;;         emacs-orderless
;;         emacs-vertico))

(setopt dc/emacs-modules (expand-file-name "modules" user-emacs-directory))
(add-to-list 'load-path dc/emacs-modules)

(indent-tabs-mode -1)

(use-package a :demand t)
(require 'a)

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

;;;; Consult
(require 'config-ui-consult)

;;;; Misc UI

(use-package ace-window
  :demand t
  :bind (("C-x o" . 'ace-window))
  :custom ((aw-background nil)))

(use-package follow
  :demand t
  :bind ((:map ctl-x-map ("M-f" . 'follow-mode))))

;;; Lisp

(use-package lispy
  :hook '((emacs-lisp-mode scheme-mode geiser-mode)))

(require 'config-guix)

;;; Org
(require 'config-org)

;;; Tools
(require 'config-vcs)
(require 'config-tramp)

;;; Final Setup

;;;; Keys

(defun dc/unbind-keys (key-names &optional keymap)
  (seq-do (lambda (key)
            (if keymap
                (unbind-key key keymap)
              (unbind-key key)))
          key-names))
(dc/unbind-keys
 '("<f2> 2" "<f2> b" "<f2> s" "<f2> <f2>"
   "<f10>" "M-<f10>" "<f11>"
   "<f3>" "<f4>"))

(defun dc/set-bars ()
  (interactive)
  (progn
    (menu-bar-mode -1)
    (scroll-bar-mode -1)
    (tool-bar-mode -1)))

;; (use-package tab-bar) ;; just forget compiled lambdas
(global-set-key (kbd "C-<prior>") 'tab-bar-switch-to-tab)
(global-set-key (kbd "C-<next>") 'tab-bar-switch-to-recent-tab)
(global-set-key (kbd "C-M-S-<return>") 'duplicate-line)
(global-set-key (kbd "C-x M-f") 'find-file-at-point)
