;;; $DOOMDIR/config.el -*- lexical-binding: t; mode: emacs-lisp -*-
;;; Basics

(require 'a)

;;;; System Identification
(defun is-system? (sysid)
  (let ((sysID (concat "ID=" sysid)))
    (and (eq system-type 'gnu/linux)
         (with-temp-buffer
           (insert-file-contents "/etc/os-release")
           (search-forward sysID nil t))
         t)))

(defvar is-guix-system (is-system? "guix"))
(defvar guix-system-path "/run/current-system/profile/bin")
(defvar guix-profile-path "~/.guix-profile/bin")
(defvar guix-guix-path (expand-file-name "~/.config/guix/current/bin"))

(defvar is-nixos (is-system? "nix"))
(defvar nixos-profile-path (concat "/etc/profiles/per-user/" (getenv "USER") "/bin"))
(defvar nixos-system-path "/run/current-system/sw/bin")

(defconst IS-MAC      (eq system-type 'darwin))
(defconst IS-LINUX    (memq system-type '(gnu gnu/linux gnu/kfreebsd berkeley-unix)))
(defconst IS-WINDOWS  (memq system-type '(cygwin windows-nt ms-dos)))
(defconst IS-BSD      (memq system-type '(darwin berkeley-unix gnu/kfreebsd)))
(defconst EMACS28+    (> emacs-major-version 27))
(defconst EMACS29+    (> emacs-major-version 28))
(defconst MODULES     (featurep 'dynamic-modules))
(defconst NATIVECOMP  (featurep 'native-compile))
(defconst DBUS_FOUND (not (null (getenv "DBUS_SESSION"))))

;;; Emacs

;;;; fix hash-table-contains-p missing
(unless (fboundp 'hash-table-contains-p)
  (defsubst hash-table-contains-p (key table)
    "Return non-nil if TABLE has an element with KEY."
    (declare (side-effect-free t)
             (important-return-value t))
    (let ((missing (make-symbol "missing")))
      (not (eq (gethash key table missing) missing)))))

;;;; User

(setq-default user-full-name "David Conner"
              user-mail-address (or (getenv "EMAIL") "noreply@te.xel.io"))

;;;; Paths

;; see doom-{emacs,core,data,docs,env,user,etc,cache,modules,...}-dir
;; (defvar dc/emacs-d (expand-file-name "~/.emacs.g/") "TODO: docs.")
;; (defvar dc/emacs-cache (expand-file-name "~/.cache/emacs/") "TODO: docs.")
;; (defvar dc/emacs-dw (expand-file-name "dw" dc/emacs-d) "TODO: docs.")
;; (defvar dc/emacs-doom-modules (expand-file-name "doom/modules" dc/emacs-d))
;; (defvar dc/emacs-modules (expand-file-name "modules" dc/emacs-d) "TODO: docs.")

;;;;; Project Paths

(defvar dc/ecto-path (or (getenv "_ECTO") (expand-file-name "~/ecto"))
  "Directory where git-repo projects are checked out.")
(defvar dc/repo-path (or (getenv "_REPO") (expand-file-name "~/repo"))
  "Directory containing XML for git-repo projects are checked out.")
(defvar dc/lang-path (or (getenv "_LANG") (expand-file-name "~/lang"))
  "Directory containing quick projects under ./lang. It typically
contains config under ./.lang to encourage native and portable
12factor language configs, when not container.")

;;;;; Guix/Geiser Paths

(defvar dc/guix-checkout-path (getenv "GUIX_SOURCE")
  "Directory containing a guix checkout. The .dir-locals.el in Guix
should be used for setting guix-load-path unless working on
checkouts of channels which depend on modified packages in the
Guix channel.")

(defvar source-directory (getenv "EMACS_SOURCE")
  "Directory containing the ./src directory of an Emacs checkout.")

(unless source-directory (warn "Emacs: source-directory is not set"))
(unless dc/guix-checkout-path (warn "Emacs: source-directory is not set"))

;; don't accidentally edit doom's straight.el files
(def-project-mode! +df-doom-read-only-mode
  :match (rx-to-string doom-emacs-dir)
  :modes '(emacs-lisp-mode)
  :add-hooks (list #'read-only-mode))

;;;;; Load Path

(add-to-list 'load-path (expand-file-name ".dotfiles/.emacs.d/lisp" (getenv "HOME")))
(require 'dw-settings)
(require 'dc-util)

;; doom-specific modules
(add-to-list 'load-path (expand-file-name "modules" doom-user-dir))

;; doom-specific module tests
(defun dc/doom-load-ert ()
  "Load tests for interactive assertions"
  (interactive)
  (use-package ert :demand t)
  (add-to-list 'load-path (expand-file-name "spec")))

;; TODO no-littering?
;;\\(?:;[^#]\\|\\*+\\)

;;;; Use Package

(setopt use-package-enable-imenu-support t)

;;;; Keymaps (Emacs Native)

;;;;; Unbinds

(defun dc/unbind-keys (key-names &optional keymap)
  (seq-do (lambda (key)
            (if keymap
                (unbind-key key keymap)
              (unbind-key key)))
          key-names))


;; Unbinds for +popup
;;
;; - M-` tmm-menubar: helpful, but also accessible via ESC-`. that gets
;;   unbound. so remap to C-h C-<SPC>
;; - M-~ not-modified: (mostly) useless, except as valuable unclaimed key
(dc/unbind-keys '("M-`" "M-~"))

;; Unbind 2C-mode

;; This mode's global bindings are also bound on C-x 6 {2,s,RET}
(dc/unbind-keys '("<f2> 2" "<f2> b" "<f2> s" "<f2> <f2>"))

;; Unbind fullscreen

;; toggle-frame-fullscreen is setting bad state in the parameters to restore
;; and won't toggle back. on f10, KDE just moves focus to emacs' menus
(dc/unbind-keys '("<f10>" "M-<f10>" "<f11>"))

;; Unbind kmacro
;;
;; - `kmacro-start-macro-or-insert-macro' not doubly mapped
;; - `kmacro-end-or-call-macro' via `C-x e'
;;
(dc/unbind-keys '("<f3>" "<f4>"))

;; ;;;;;; Unbind completion-at-point (for cape)

;; (dc/unbind-keys '("C-M-i"))

;;;;; Global

(global-set-key (kbd "C-<prior>") #'tab-bar-switch-to-tab)
(global-set-key (kbd "C-<next>") #'tab-bar-switch-to-recent-tab)
(global-set-key (kbd "C-M-S-<return>") #'duplicate-line)
(global-set-key (kbd "C-x M-f") #'find-file-at-point)

(use-package! xkb-mode :defer t)

(defun dc/underspace ()
  "Insert an underscore with `S-<SPC>'. Configure here rather than in XKB."
  (interactive)
  ;; (insert "_")
  (insert-char "_"))

(global-set-key (kbd "S-<SPC>") #'dc/underspace)
;; (global-set-key (kbd "S-<SPC>") (apply-partially #'insert-char "_"))

;; ... LIT ER ALL Y? damit

(defvar +df-xkb-regexp
  (rx-let ((xdg-config (literal (or (getenv "XDG_CONFIG_HOME")
                                    (expand-file-name "~/.config"))))
           (df-xdg-config (literal (expand-file-name "~/.dotfiles/.config"))))
    (rx bos (and (| xdg-config df-xdg-config)
                 (| "/xkb" "/xkbtest" "/xkbstd")
                 (| "/types" "/compat" "/symbols" "/keycodes")))) ;; "/rules"
  "Regexp to match XKB files for `+df-xkb-mode'.")

(def-project-mode! +df-xkb-mode
  :match +df-xkb-regexp
  :on-enter (xkb-mode))

;;;;; Quick Map <f1> <f2>

;; this is a bit tough to determine how to get it to be a separate keymap. you
;; can't (map! :map help-map "<f2>" 'dc/quick-map)

(general-create-definer quick-def
  :prefix-map 'dc/quick-map
  :prefix-command 'dc/quick-map)

;; NOTE: running this after defining the map means all subsequent references
;; must use (map! :map dc/quick-map)
;;
(general-define-key
 :keymaps 'help-map
 "<f2>" '(:prefix-command dc/quick-map :wk "QUICK"))

(quick-def
  "D" '(:ignore t :which-key "DESKTOP")
  "Ds" #'desktop-save-in-desktop-dir
  "DS" #'desktop-save
  "Dr" #'desktop-read
  "DC" #'dc/desktop-lock-clear

  "h" #'shortdoc

  "<f3>" #'forge-dispatch
  "<SPC>" #'magit-dispatch

  "M-l" '(:ignore t :which-key "LOCALVARS")
  "M-l ad" #'add-dir-local-variable
  "M-l aF" #'add-file-local-variable
  "M-l af" #'add-file-local-variable-prop-line
  "M-l dd" #'delete-dir-local-variable
  "M-l dF" #'delete-file-local-variable
  "M-l df" #'delete-file-local-variable-prop-line
  "M-l k" #'kill-local-variable
  "M-l m" #'make-local-variable
  "M-l M" #'make-variable-buffer-local
  "M-l h" #'apropos-local-variable
  "M-l H" #'array-display-local-variables)

;;;;; Doc Map <f3>
(general-create-definer doc-def
  :prefix-map 'dc/doc-map
  :prefix-command 'dc/doc-map)
(general-define-key
 :keymaps 'global-map
 "<f3>" '(:prefix-command dc/doc-map :wk "DOC"))
(doc-def
  "<f3>" #'lsp-ui-doc-toggle
  "<f4>" #'lsp-lens-show)

;;;;; Cape Map <f5>
(general-create-definer cape-def
  :prefix-map 'dc/cape-map
  :prefix-command 'dc/cape-map)
(general-define-key
 :keymaps 'global-map
 "<f2>" '(:prefix-command dc/cape-map :wk "CAPE")
 "<f11>" '(:prefix-command dc/cape-map :wk "CAPE"))
(cape-def
  "M-a" #'cape-abbrev
  "M-A" #'cape-dabbrev
  "f" #'cape-file
  "l" #'cape-line
  "M-d" #'cape-dict
  "e" #'cape-elisp-symbol
  "E" #'cape-elisp-block
  ":" #'cape-emoji
  "k" #'cape-keyword
  "t" #'complete-tag
  "x" #'cape-sgml
  "<SPC>" #'cape-tex
  "M-<SPC>" #'cape-rfc1345)

;;;;; LSP Map <f8>
(general-create-definer lsp-def
  :prefix-map 'dc/lsp-map
  :prefix-command 'dc/lsp-map)
(general-define-key
 :keymaps 'global-map
 "<f8>" '(:prefix-command dc/lsp-map :wk "LSP"))
(lsp-def
  "<f7>" #'lsp-ui-peek-enable
  "<f8>" #'lsp-ui-lsp-toggle
  "<f9>" #'lsp-lens)

;;; Projects
;;
;;;; Project.el
;;
;;;;; Projectile

(use-package! projectile
  :custom (projectile-project-search-path `((,dc/repo-path . 1)
                                            (,dc/ecto-path . 3))))

;;;;; Activities
(use-package! activities
  :demand t
  :config
  (map! :map dc/quick-map
        "b" #'activities-switch-buffer ;; prefix: select from other activities
        (:prefix ("@" . "ACTIVITIES")
                 "@" #'activities-list
                 "m" #'activities-mode
                 "k" #'activities-kill
                 "M-k" #'activities-discard
                 "n" #'activities-define ;; prefix: redefine default state
                 "M-n" #'activities-new
                 "r" #'activities-resume ;; prefix: restore default state
                 "R" #'activities-rename
                 "M-r" #'activities-revert
                 "$" #'activities-save-all
                 "q" #'activities-suspend
                 "b" #'activities-switch
                 "t" #'activities-tabs-mode))
  :hook (doom-init-ui-hook . (lambda () (activities-tabs-mode +1))))

;; phew, I can jump to a window on KDE without using my eyes
(defun dc/tab-names (&optional tabs)
  (let ((tabs (or tabs (funcall tab-bar-tabs-function))))
    (mapcar (lambda (tab) (alist-get 'name tab)) tabs)))

(after! tab-bar
  (setq frame-title-format
        '("♦ DOOM ♣︎ ╟─» "
          (:eval (string-join (dc/tab-names) " «─┼─» "))
          " «─╢ ♠︎ %b ♥︎")))

;;; Interface

(defun dc/toggle-window-balance ()
  "Toggle between `maximize-window' and `balance-windows' depending on
`window-width' and `frame-width'."
  (interactive)
  (let ((width-percentage
         (* 100.0 (/ (* 1.0 (window-width)) (frame-width))))
        (restore-popups (+popup-windows)))
    (when restore-popups (+popup/toggle))
    (if (> width-percentage 75)
        (balance-windows)
      (maximize-window))
    (when restore-popups (+popup/toggle))))

(defun +popup/toggle ()
  "Toggle any visible popups.
If no popups are available, display the *Messages* buffer in a popup window."
  (interactive)
  (let ((+popup--inhibit-transient t))
    (cond ((+popup-windows) (+popup/close-all t))
          ((ignore-errors (+popup/restore)))
          ((display-buffer (get-buffer "*Messages*"))))))

;;;; Basics
;;
;;;;; Tooltips

(add-hook! 'doom-init-ui-hook
  (progn
    (menu-bar-mode +1)
    (scroll-bar-mode -1)
    (tool-bar-mode -1)))

;;;;; Menus
;;;;; Date & Time
;;;;; Desktop
(after! customize
  (set-popup-rules!
    '(("^\\*Customize"
       :slot 2 :side right :size 0.25 :select t :quit nil))))

;;;;; Desktop

;; phew, it can ask (doesn't really work though)
;;
;; I need to shutdown emacs gracefully from systemd
(use-package! desktop
  :demand t
  :custom
  (desktop-dirname doom-cache-dir)
  (desktop-load-locked-desktop 'ask))

;; desktop-dirname

(defun dc/desktop-lock-p (&optional dirname)
  (let* ((dirname (or dirname desktop-dirname)))
    (file-exists-p (desktop-full-lock-name dirname))))

(defun dc/desktop-in-use-p (&optional dirname)
  (let* ((dirname (or dirname desktop-dirname))
         ;; (desktop-lock (format "%s.lock" desktop-file))
         (desktop-comm (alist-get 'comm (process-attributes (or (desktop-owner dirname) 0)))))
    (when desktop-comm
      (string-match-p (rx "emacs") desktop-comm))))

(defun dc/desktop-lock-clear (&optional dirname arg)
  "Release the PID file at `desktop-full-lock-name' for `dirname' if it
it's associated to an emacs process. It's difficult to /always/ ensure
proper shutdown and release when systemd runs the server. Many failure
modes and testing is tedious."
  (interactive "i\np")
  (let* ((arg-read (memq arg '(4)))
         (dirname
          (or dirname
              (and arg-read
                   (read-directory-name "Directory for desktop file: " nil nil t))
              desktop-dirname))
         (desktop-file (desktop-full-file-name dirname))
         (desktop-lock (desktop-full-lock-name dirname)))
    (message (format "Clearing desktop file in: %s" dirname))
    (when (dc/desktop-lock-p dirname)
      (when (dc/desktop-in-use-p dirname)
        (user-error (format "Desktop in use: %s"
                            (desktop-full-file-name dirname))))
      (desktop-release-lock dirname))))

;;;;; Mouse

;;;; Casual

;; TODO: PKG recent-rgrep https://github.com/kickingvegas/recent-rgrep

;;;;; Calc Mode For The Plebs
(use-package! casual-suite
  :defer t
  :config
  (map! "C-o" #'casual-editkit-main-tmenu
        :map goto-map "M-g" #'casual-avy-tmenu
        :map ibuffer-mode-map
        "C-o" #'casual-ibuffer-tmenu
        "F" #'casual-ibuffer-filter-tmenu
        "s" #'casual-ibuffer-sortby-tmenu
        :map calc-mode-map "C-o" #'casual-calc-tmenu
        :map dired-mode-map "C-o" #'casual-dired-tmenu
        :map isearch-mode-map "C-o" #'casual-isearch-tmenu
        :map Info-mode-map "C-o" #'casual-info-tmenu
        :map reb-mode-map "C-o" #'casual-re-builder-tmenu
        :map reb-lisp-mode-map "C-o" #'casual-re-builder-tmenu
        :map bookmark-bmenu-mode-map "C-o" #'casual-bookmarks-tmenu
        :map org-agenda-mode-map "C-o" #'casual-agenda-tmenu
        :map symbol-overlay-map "C-o" #'casual-symbol-overlay-tmenu))

;;;; Search
;;;;; Xref
;;;;; Grep

;;;;;; Ripgrep
(defvar dc/ripgrep-args "--hidden -g \"!/po\""
  "Args for ripgrep commands. Could be configured in ~/.ripgreprc or
.{git,rg,}ignore files.")

;;;; Buffers
(setopt global-auto-revert-non-file-buffers t
        auto-revert-verbose nil)

;; TODO: decide on global-auto-revert-mode
;; (global-auto-revert-mode 1)

;;;;; Bufler

;; The bufler-workspace-set doesn't conflict with activities.el per se, but I
;; like that window title isn't restricted to the project path (

(use-package bufler
  :demand t
  :bind
  (("C-x M-b M-b" . #'bufler-sidebar)
   ("C-x M-b b" . #'bufler-workspace-set)
   ("C-x M-b <SPC>" . #'bufler-workspace-switch-buffer)
   ("C-x M-b M-<SPC>" . #'bufler-switch-buffer)
   ("C-x M-b g" . #'bufler-list)))

;; Use prefix to control *Bufler* sidebar placement
;;
;; (after! bufler
;;   (set-popup-rules!
;;     '(("^\\*Bufler" :side top :vslot -5 :slot -5 :width 80 :select t :quit t)))
;;   (map! :map doom-leader-toggle-map "b" #'bufler-sidebar))

;;;;;; Bufler defauto-groups
;;;;;; Bufler defgroups
;;;;;; Bufler package

;;;; Editor

;;;;; Regexp
(use-package! re-builder
  :custom (reb-re-syntax 'rx))

;;;;; Highlighting

;; call unhighlight-regexp, it lists the regexps corresponding to the current
;; highlights
;;
;; otherwise, highlight-symbol-at-point runs this to get the regexp
;; corresponding to the parsed current symbol at point
;;
;; (hi-lock-regexp-okay (find-tag-default-as-symbol-regexp))
(use-package! highlight-symbol
  :demand t
  :custom (highlight-symbol-delay 0.5)
  :hook ((scheme-mode emacs-lisp-mode conf-mode syslog-mode) . highlight-symbol-mode)
  :config (map! :map doom-leader-toggle-map "M-h" #'highlight-symbol-mode))

;;;;; Follow

;; very useful when used with highlight-symbol. also native with no deps
(use-package! follow
  :demand t
  :bind ((:map doom-leader-toggle-map ("M-f" . #'follow-mode))))

;;;;; Undo
(use-package! undo-tree
  :defer t
  :custom (undo-tree-auto-save-history nil))

;;;; UI

(setq display-line-numbers-type nil)

;; doom--setq-outline-level-for-emacs-lisp-mode-h
;; doom--setq-outline-regexp-for-emacs-lisp-mode-h
;; doom--setq-tab-width-for-emacs-lisp-mode-h

;;;; Dired

(defun dc/dired-quit-window (&optional dont-kill window)
  (interactive "P")
  (call-interactively #'quit-window (not dont-kill) window))
(use-package! dired
  :defer t
  :config
  (map! :map dired-mode-map [remap quit-window] #'dc/dired-quit-window))
;; ... dired is built to omit .go files

(use-package! dired-x
  :defer t
  :config (setq dired-omit-extensions (delete ".go" dired-omit-extensions)))

(setq dired-omit-files "^.DS_Store\\'\\|^.project\\(?:ile\\)?\\'\\|^.\\(svn\\)\\'\\|^.ccls-cache\\'\\|\\(?:\\.js\\)?\\.meta\\'\\|\\.\\(?:elc\\|o\\|pyo\\|swp\\|class\\)\\'")

;; TODO: get Doom to; not close all the direds(advice-add #'+doom-)
;;   - dirvish is somewhat responsible for this
(dirvish-override-dired-mode -1)

;;;;; Recentf

(after! recentf
  (add-to-list 'recentf-exclude (rx (and line-start "/gnu/store"))))

;;;; Alerts
(require 'notifications)
(use-package! alert
  :demand t
  :custom
  (alert-default-style 'libnotify)
  (alert-log-level 'normal)
  :config
  (map! :map dc/quick-map
        (:prefix ("a" . "ALERT")
                 "o" #'alert--log-open-log
                 "M-c" #'alert--log-clear-log
                 "d" #'alert--log-enable-debugging
                 "D" #'alert--log-disable-debugging
                 "l" #'alert--log-disable-logging
                 "L" #'alert--log-enable-logging
                 "m" #'alert--log-enable-messaging
                 "M" #'alert--log-disable-messaging)))

;;;; Confirmations

(setq dired-deletion-confirmer 'y-or-n-p
      dired-confirm-shell-command 'y-or-n-p

      ;; dired-no-confirm '()

      ;; files/urs
      url-confirmation-func 'y-or-n-p
      url-cookie-confirmation 'y-or-n-p
      log-edit-confirm  'changed
      ;; log-edit-confirm #'yes-or-no-p ; defaults to 'changed

      ;; emacs
      confirm-kill-processes 'y-or-n-p
      confirm-kill-emacs #'doom-quit-p

      ;; email
      message-confirm-send 'y-or-n-p

      ;; org
      org-table-fix-formulas-confirm nil ;; 'y-or-n-p ; no default is no

      ;; eglot
      eglot-confirm-server-initiated-edits 'y-or-n-p
      ;; TODO: lsp confirmations?

      ;; smerge-mode is lazy loaded, default: t
      smerge-change-buffer-confirm t)

;;;; Font

(cond
 (is-guix-system
  (setopt doom-font (font-spec :family "Iosevka Nerd Font Mono" :size 12 :weight 'normal)
          doom-serif-font (font-spec :family "Iosevka Nerd Font Mono" :size 12 :weight 'normal)))
 (is-nixos
  (setopt doom-font (font-spec :family "Fira Code Nerd Font Mono" :size 11 :weight 'normal)
          doom-serif-font (font-spec :family "Fira Code Nerd Font Mono" :size 11 :weight 'normal)
          doom-big-font (font-spec :family "Noto Sans" :size 36 :weight 'normal)
          doom-emoji-font (font-spec :family "Noto Sans" :weight 'normal)
          doom-symbol-font (font-spec :family "Noto Sans" :weight 'normal)
          doom-unicode-font (font-spec :family "Noto Sans" :weight 'normal)
          doom-variable-pitch-font (font-spec :family "Noto Sans" :weight 'normal)))
 (t (setopt
     doom-font (font-spec :family "Noto Sans Mono" :size 12 :weight 'normal)
     doom-serif-font (font-spec :family "Noto Serif" :size 12 :weight 'normal)
     doom-big-font (font-spec :family "Noto Sans" :size 36 :weight 'normal)
     doom-emoji-font (font-spec :family "Noto Sans" :weight 'normal)
     doom-symbol-font (font-spec :family "Noto Sans" :weight 'normal)
     doom-unicode-font (font-spec :family "Noto Sans" :weight 'normal)
     doom-variable-pitch-font (font-spec :family "Noto Sans" :weight 'normal))))

;; (math ⊕ ⊗) (kana あいうえお) (kanji 一分亜音) (emoji 🇦 ♍ 🌐 💫)
;;
;; For japanese characters
;; - Noto Sans => falls through to Noto Serif CJK JP
;; - Fira Code Nerd Font => Source Han Serif K
;;
;; Unicode, Symbol, Emoji: Should not define font-size

;;;; Theme
(use-package ef-themes
  :defer t
  :init (setopt doom-theme nil)
  ;; hmmm v2.0?
  ;; :hook (doom-init-ui-hook . (lambda () (ef-themes-load-random-dark)))
  :hook (doom-init-ui-hook . (lambda () (modus-themes-load-random-dark)))
  :config
  (map! :map dc/quick-map
        (:prefix ("t" . "THEME")
                 "r" #'ef-themes-load-random
                 "s" #'ef-themes-select
                 "t" #'ef-themes-toggle)))

;;;; Windows

;; NOTE these functions are duplicated in ~/.emacs.d/lisp/dc-aceable-window.el
(defun dc/aw-window-list ()
  (mapcar (lambda (wnd) (cons (aw-offset wnd) wnd)) (aw-window-list)))

(defun dc/aw-tree ()
  (avy-tree (dc/aw-window-list) aw-keys))

(defun dc/aw-select-nth (fn n)
  (let ((n (or n 0))
        (atree (dc/aw-tree)))
    (unless (> (- 1 n) (length atree))
      (funcall fn (cdddr (nth n atree))))))

(use-package! ace-window
  :commands ace-window aw-show-dispatch-help
  :config
  ;; this logs to messages a bit too often, to engrain the functionality...
  (global-set-key [remap other-window] #'aw-show-dispatch-help)
  ;; overrides C-x C-o as #'delete-blank-lines (useful, as selecting the
  ;; interpolating "not s-expression" of blank lines is PITA)
  (map! :map ctl-x-map "C-o" #'ace-window))

;; TODO buf-move-up: keybinds are not set until used
(use-package! buffer-move
  :commands buf-move-up buf-move-down buf-move-left buf-move-right
  :bind (("C-x M-b h" . #'buf-move-left)
         ("C-x M-b j" . #'buf-move-down)
         ("C-x M-b k" . #'buf-move-up)
         ("C-x M-b l" . #'buf-move-right)))

;; :config (map! "<C-S-up>" #'buf-move-up
;;       "<C-S-down>" #'buf-move-down
;;       "<C-S-left>" #'buf-move-left
;;       "<C-S-right>" #'buf-move-right)

;; from tecosaur: ask for buffer after basic window splits
;;
;; TODO: need to distinguish between interactive calls to split-window-*
;;
;; (defadvice! prompt-for-buffer (&rest _)
;;   :after '(split-window-below split-window-right)
;;   (consult-buffer))

;;;; Tabs Windows

;;;; Completion
;;;;; Vertico

;; Doom defaults

;; (add-to-list 'vertico-multiform-categories
;;              '(file (+vertico-transform-functions . +vertico-highlight-directory)))
;; (add-to-list 'vertico-multiform-commands
;;              '(execute-extended-command (+vertico-transform-functions . +vertico-highlight-enabled-mode)))

;; NOTE: vertico--remote-p checks if path is remote (tramp)

;;;;;; vertico-multiform-{categories,commands}
(after! vertico-multiform
  (cl-dolist
      (vrt-cat
       '((bookmark reverse grid)
         (buffer reverse grid)          ; works for ido
         (command reverse)
         (consult-compile-error buffer)
         (consult-compile-multi grid (vertico-grid-annotate . 20))
         ;; (consult-flymake-error)
         (consult-grep buffer)
         (consult-git-log-grep-result buffer)
         (consult-info reverse)
         ;; (consult-kmacro)
         (consult-location buffer)
         ;; (consult-imenu buffer)
         (consult-man reverse grid (vertigo-cycle . t))
         (consult-xref buffer)
         (environment-variable reverse grid)
         (expression reverse)           ; for repeat-complex-command

         (file reverse grid (vertico-grid-annotate . 20))
         ;; (file reverse grid)
         (imenu buffer)
         (info-menu reverse grid)
         (kill-ring reverse grid)
         (minor-mode reverse)
         (consult-org-heading reverse grid)
         ;; (symbol)
         ;; not sure what symbol-help category refers to
         (symbol-help reverse grid (vertico-grid-annotate . 20))
         (theme reverse grid)
         (unicode-name grid reverse)
         (yasnippet grid reverse (vertico-cycle . t))
         (t)))
    (add-to-list 'vertico-multiform-categories vrt-cat))

  (cl-dolist (vrt-cmd '(("flyspell-correct-*" grid reverse (vertico-grid-annotate . 20))
                        (org-refile grid reverse indexed)
                        (consult-yank-pop indexed)
                        ;; (consult-lsp-diagnostics)
                        (consult-flycheck reverse)
                        (consult-flymake reverse)))
    (add-to-list 'vertico-multiform-commands vrt-cmd)))

;;;;; Corfu
(use-package! corfu
  :defer t
  :bind ((:map corfu-map
               ("C-f" . #'corfu-insert-separator)
               ("'" . #'corfu-quick-complete)))
  :config
  (setq corfu-auto-delay 0.5
        corfu-auto-prefix 3

        ;; the doom defaults
        global-corfu-modes '((not erc-mode circe-mode help-mode gud-mode vterm-mode) t)
        corfu-popupinfo-min-height 5
        corfu-popupinfo-max-height 15
        corfu-popupinfo-direction 'right ; default list: '(right left down)
        corfu-popupinfo-delay '(1.0 0.5)

        corfu-preview-current nil))

;;;;; Orderless

;; TODO: CONF: look into `orderless-style-dispatchers'

;; TODO: CONF: get orderless to group M-g o `consult-outline' candidates by depth

;; Each override has the shape (CATEGORY . ALIST) where ALIST is
;; an association list that can specify properties such as:
;; - `styles': the list of `completion-styles' to use for that category.
;; - `cycle': the `completion-cycle-threshold' to use for that category.
;; - `cycle-sort-function': function to sort entries when cycling.
;; - `display-sort-function': nil means to use either the sorting
;; function from metadata, or if that is nil, fall back to `completions-sort';
;; `identity' disables sorting and keeps the original order; and other
;; possible values are the same as in `completions-sort'.
;; - `group-function': function for grouping the completion candidates.
;; - `annotation-function': function to add annotations in *Completions*.
;; - `affixation-function': function to prepend/append a prefix/suffix.
;; See more description of metadata in `completion-metadata'.

(defun dc/match-orderless-literal ()
  "Components match literally for the rest of the session."
  (interactive)
  (setq-local orderless-matching-styles '(orderless-literal)
              orderless-style-dispatchers nil))

(use-package! orderless
  :custom
  (read-buffer-completion-ignore-case t)
  :bind ((:map minibuffer-local-map
               ;; orderless
               ("C-l" . #'dc/match-components-literally))))

(use-package! consult
  :custom (consult-man-args (or (and is-nixos "/run/current-system/sw/bin/man -k") "man -k"))
  :bind ((:map global-map
               ("C-x M-:" . #'consult-complex-command))))

;; `completion-category-defaults' is set to nil in doom's vertico/corfu modules

;;;;; Consult

(defun dc/consult-ripgrep (&optional dir initial)
  "By default `consult-ripgrep' should not exclude compressed files, but in
large search domains, it's almost always a failure."
  (interactive "P")
  (let ((consult-ripgrep-args (concat consult-ripgrep-args " --no-search-zip")))
    (consult-ripgrep dir initial)))

(use-package! consult-org-roam
  :after (org-roam consult)
  :config
  (unless (string-match dc/ripgrep-args consult-ripgrep-args)
    (setq consult-ripgrep-args
          (string-join (list consult-ripgrep-args dc/ripgrep-args) " ")))
  :custom
  (consult-org-roam-grep-func #'consult-ripgrep)
  (consult-org-roam-buffer-narrow-key consult-narrow-key)
  (consult-org-roam-buffer-after-buffers t))

;; TODO: CONF: consult-org-roam: (consult-customize ...) & (consult-org-roam-mode)

;;;;; Marginalia
;; regex to prevent things from popping on screen
;; (setq marginalia-censor-variables nil)
;; TODO: CONF: extend `marginalia-annotator-registry'

;;;;; Cape

(after! capf
  (map! :map dc/quick-map "SPC" #'cape-prefix-map))

;;;;; Embark
(after! embark
  (set-popup-rules!
    '(("^\\*Embark Collect:" :side bottom :vslot -5 :slot -5 :width 80 :select t :quit t))))

;;; Markdown

(remove-hook 'text-mode-hook #'visual-line-mode)
(add-hook 'text-mode-hook #'auto-fill-mode)

(use-package! markdown-mode
  :hook (markdown-mode-hook . visual-line-mode)
  :config (add-to-list 'auto-mode-alist '("\\.mdx\\'" . markdown-mode)))



;;; Org

;; + Structure via https://tecosaur.github.io/emacs-config
;; + my old config was mostly porting in Doom's org behavior, so there was a
;;   lot of duplication there

;;;; Packages


(setopt org-directory (or (getenv "ORG_DIRECTORY") "~/org"))

;;;;; Org, itself


(after! org-mode
  (remove-hook 'org-mode-hook #'visual-line-mode)
  (add-hook 'org-mode-hook #'auto-fill-mode))

;;;;; Visuals

;;;;;; org-modern
;; TODO: PKG: configure org-modern? (later)
(use-package! org-modern
  :hook (org-mode . org-modern-mode)
  :config
  (setq org-modern-star '("◉" "○" "✸" "✿" "✤" "✜" "◆" "▶")
        org-modern-table-vertical 1
        org-modern-table-horizontal 0.2
        org-modern-list '((43 . "➤")
                          (45 . "–")
                          (42 . "•"))
        org-modern-todo-faces
        '(("TODO" :inverse-video t :inherit org-todo)
          ("PROJ" :inverse-video t :inherit +org-todo-project)
          ("STRT" :inverse-video t :inherit +org-todo-active)
          ("[-]"  :inverse-video t :inherit +org-todo-active)
          ("HOLD" :inverse-video t :inherit +org-todo-onhold)
          ("WAIT" :inverse-video t :inherit +org-todo-onhold)
          ("[?]"  :inverse-video t :inherit +org-todo-onhold)
          ("KILL" :inverse-video t :inherit +org-todo-cancel)
          ("NO"   :inverse-video t :inherit +org-todo-cancel))))

;;;;;; org-appear
;; TODO: PKG: configure org-appear tweaks? (later)
;; https://tecosaur.github.io/emacs-config/config.html#emphasis-markers

;;;;; Extras
;; TODO: PKG: consider org-glossary

;;;;;; Importing with pandoc
;; TODO: PKG: configure org-pandoc-import

;;;;;; Recipes
;; TODO: PKG: configure org-chef (+ capture)

;;;; Behavior

;; from tecosaur
(setq org-use-property-inheritance t
      org-log-done t
      org-list-allow-alphabetical t
      org-catch-invisible-edits 'smart
      org-export-with-sub-superscripts '{}
      ;; org-export-allow-bind-keywords t
      org-image-actual-width '(0.9)

      ;; from .emacs.guix
      org-capture-bookmark t ;; nil
      org-cycle-separator-lines 2
      org-eldoc-breadcrumb-separator " → "
      org-enforce-todo-dependencies t
      ;; startup
      org-startup-folded 'content
      org-startup-indented nil)

;; this adds (:comments . "link"), which is useful for detangling
(setq org-babel-default-header-args
      '((:session . "none")
        (:results . "replace")
        (:exports . "code")
        (:cache . "no")
        (:noweb . "no")
        (:hlines . "no")
        (:tangle . "no")
        (:comments . "link")))

;; (setq org-list-demote-modify-bullet
;;       '(("+" . "-") ("-" . "+") ("*" . "+") ("1." . "a.")))



;;;;; Citation

;; Doom's biblio uses citar/oc in lieu of most of bibtex. we still want
;; the `bibtex-autokey' properties to set consistently though (i think)

;; ---------------------------------------------
;;;;;; org-ref and DOI Utils

;; - org-ref-pdf: extracts bibtex from a PDF file. requires pdftotext
;; - org-ref-url-utils: drag-n-drop webpage from browser onto bibtex buffer to add bibtex entry
;;   may not be reliable

(use-package! bibtex-completion
  :defer t
  :after citar)

(use-package! org-ref
  :defer t
  :after citar)

(after! '(org-ref bibtex-completion)
  (require 'org-ref-core)
  (require 'org-ref-utils)
  (require 'org-ref-url-utils)
  (require 'doi-utils)
  (require 'org-ref-arxiv)
  (require 'org-ref-pubmed)
  (require 'org-ref-scifinder)
  (require 'org-ref-isbn))

;;;;;; Biblio.el

;; (use-package! biblio) ;; doesn't really autokey

;;;;;; Citar

(use-package! citar
  :defer t
  :config
  (let* ((roam-notes (expand-file-name "roam/noter" org-directory))
         (xdg-docs (xdg-user-dir "DOCUMENTS"))
         ;; these need to end in a slash (doi-utils will rename the file incorrectly)
         (lib-paths '("articles/" "books/" "collections/" "doi/" "texts/"))
         (lib-paths (mapcar (lambda (f) (expand-file-name f xdg-docs))
                            lib-paths))
         (bib-files '("articles.bib" "books.bib" "collections.bib" "doi.bib" "texts.bib"))
         (bib-files (mapcar (lambda (f) (expand-file-name f roam-notes))
                            bib-files)))

    ;; oc needs `citar-bibliography' to set `org-cite-global-bibliography'
    ;; doi-utils needs `bibtex-completion-*'
    (setq bibtex-completion-bibliography bib-files
          bibtex-completion-library-path lib-paths
          bibtex-completion-notes-path (list roam-notes)
          citar-bibliography bib-files
          citar-library-paths lib-paths
          citar-notes-paths (list roam-notes)
          citar-org-roam-subdir (file-name-base roam-notes)))

  :custom
  (bibtex-autokey-year-length 4)
  (bibtex-autokey-names 2)
  (bibtex-autokey-names-stretch 1)
  (bibtex-autokey-name-year-separator "-")
  (bibtex-autokey-year-title-separator "-")
  (bibtex-autokey-titleword-separator "-")
  (bibtex-autokey-titlewords 3)
  ;; (remove colon from default: [.!?:;]\|--)
  (bibtex-autokey-title-terminators "[.!?;]\\|--")
  (bibtex-autokey-titlewords-stretch 1)
  (bibtex-autokey-titleword-length 5)
  :hook
  (latex-mode-hook . citar-capf-setup)
  (org-mode-hook . citar-capf-setup))

(defun dc/citar-get-citekey-files (citekey)
  (gethash citekey
           (citar--get-resources citekey
                                 (mapcar (lambda (source)
                                           (plist-get source :items))
                                         citar-file-sources))))

(use-package! citar-org-roam
  :defer t
  :after '(citar org-roam)
  :custom
  (citar-org-roam-note-title-template "${title} - ${author}")
  (citar-org-roam-capture-template-key "nc")

  :config
  (add-to-list
   'org-roam-capture-templates
   `("nc" "Note: Citar" plain "%?" :unnarrowed t
     :target
     (file+head
      "noter/${citar-citekey}.org"
      ;; NOTE: The NOTER_DOCUMENT must be under a heading
      "#+title: (${citar-date}) ${note-title}.
#+created: %U
#+last_modified: %U

+ file :: ${citar-citekey}.pdf

* Notes
:PROPERTIES:
# :NOTER_DOCUMENT: %(dc/citar-get-citekey-files \"%{citar-citekey}\")
:END:
"
      ;; (citar-org-roam-mode)
      ))))

;;;;;; Zotero
;; TODO: PKG: oc-csl, renders/exports citations with Zotoro CSL styles (req. download)

;; - doom imports citation

;; (after! oc (setq org-cite-export-processors '((t csl))))
;; TODO: CONF: org-ref => org-cite https://tecosaur.github.io/emacs-config/config.html#citation,code--7

;;;;; cdlatex

;;;;; Org Agenda
;; start with empty org-agenda-files
(setq org-agenda-files (list (expand-file-name ".dotfiles/todo.org" (getenv "HOME")))

      ;; org-habit
      org-habit-show-habits t           ; default
      org-habit-show-habits-only-for-today nil

      ;; org-clock
      org-clock-auto-clockout-timer 300
      org-clock-history-length 25
      org-clock-in-switch-to-state "STRT"
      org-clock-out-switch-to-state "HOLD"
      org-clock-out-remove-zero-time-clocks t

      ;; Don't monopolize the whole frame just for the agenda
      org-agenda-window-setup 'current-window
      org-agenda-skip-unavailable-files t

      ;; org-clock-persist
      org-clock-persist t
      org-clock-persist-query-save t
      org-clock-persist-query-resume nil ; default

      ;; org-log-into-drawer t ;; use #+STARTUP: logdrawer
      org-log-done 'time

      ;; org-columns-default-format-for-agenda
      org-columns-default-format (string-join '("%20CATEGORY(Category)"
                                                "%65ITEM(Task)"
                                                "%TODO"
                                                "%6Effort(Estim){:}"
                                                "%6CLOCKSUM(Clock)"
                                                "%TAGS") " ")

      ;; from tecosaur
      org-agenda-skip-scheduled-if-done t
      org-agenda-skip-deadline-if-done t
      org-agenda-include-deadlines t
      ;; org-agenda-block-separator nil
      ;; org-agenda-tags-column 100 ;; from testing this seems to be a good value
      org-agenda-compact-blocks t)

;; TODO: PKG: org-expiry to add :CREATED: props with auto-expiry
;;
;; - org-expiry uses the older (<24.4) advice-add, advice-remove, so needs
;;   some work

(setq-default org-agenda-span 10
              org-agenda-start-on-weekday nil
              org-agenda-start-day "-3d"
              org-agenda-inhibit-startup t
              org-agenda-deadline-faces '((1.001 . error)
                                          (1.0 . org-warning)
                                          (0.5 . org-upcoming-deadline)
                                          (0.0 . org-upcoming-distant-deadline)))

;;;;; Org Tags

;; Deadline/Schedule: Use prop tags instead
;; (:startgroup . nil) ("DL" . ?d) ("SCH" . ?s) (:newline) (:endgroup . nil)

(setq-default
 org-tag-persistent-alist
 '((:startgroup . "WHY")
   ;; WHY =======================
   ("VIS" . ?v) ;; Visibility
   ("ISH" . ?!) ;; Issues
   ("GO" . ?G)  ;; Goals
   ("FIN" . ?$) ;; Finance
   (:newline . nil) (:endgroup . nil)
   (:startgrouptag) ("HOW")
   (:grouptags)
   ("AUTO" . ?a) ;; Automation
   ("NET" . ?n)  ;; Network
   ("FS" . ?f)   ;; Files
   ("DO" . ?d)   ;; Devops
   ("AU" . ?@)   ;; Auth
   ("ID" . ?#)   ;; Identity
   ("DF" . ?.)   ;; Dotfiles
   (:newline . nil) (:endgrouptag)
   (:startgrouptag) ("WHAT")
   (:grouptags)
   ("CODE" . ?%)  ;; Code
   ("READ" . ?&)  ;; Read
   ("3D" . ?3)    ;; 3D
   ("CAD" . ?C)   ;; Design
   ("WS" . ?w)    ;; Workshop
   ("ART" . ?A)   ;; Art
   ("MUS" . ?#)   ;; Music
   ("LEARN" . ?L) ;; Learn
   ("EDU" . ?E)   ;; Edu
   ("HOME" . ?H)  ;; Home
   ("FAB" . ?F)   ;; Fablab
   (:newline . nil) (:endgrouptag)
   (:startgroup . "WHO")
   ;; WHO  =======================
   ("MEET" . ?M) ;; Meetups
   ("MSG" . ?m)  ;; Msg
   ("EV" . ?V)   ;; Events
   ("CON" . ?c)  ;; Contacts
   (:newline . nil)
   (:endgroup . nil)))

;;;;; Org Super Agenda

(use-package! org-super-agenda
  :commands org-super-agenda-mode
  :init (setq org-super-agenda-header-separator ""
              org-super-agenda-groups
              '((:name "Today" :time-grid t :todo "TODO")
                ;; (:habit t)
                (:name "Due today" :deadline today)
                (:name "Overdue" :deadline past)
                (:name "Due soon" :deadline future)
                (:name "Urgent" :priority "A")
                (:name "Crit" :priority "B")
                (:name "No Estimate" :scheduled t)
                (:name "No Deadline" :scheduled t)
                (:priority<= "C" :order 1))))

(after! org-agenda
  (let ((inhibit-message t))
    (org-super-agenda-mode)
    (org-clock-auto-clockout-insinuate)))


;;;;;; org-clock sounds
;; (and (file-exists-p dc/emacs-sound-theme-path)
;;      (setq-default org-clock-sound (expand-file-name "complete.oga"
;;                                                      dc/emacs-sound-theme-path)))


;;;;; Capture

;;;;; Roam

;;;;; Snippets

;;;; Visuals
;;;;; Font Display

(add-hook 'org-mode-hook #'+org-pretty-mode)

(custom-set-faces!
  '(outline-1 :weight extra-bold :height 1.25)
  '(outline-2 :weight bold :height 1.15)
  '(outline-3 :weight bold :height 1.12)
  '(outline-4 :weight semi-bold :height 1.09)
  '(outline-5 :weight semi-bold :height 1.06)
  '(outline-6 :weight semi-bold :height 1.03)
  '(outline-8 :weight semi-bold)
  '(outline-9 :weight semi-bold))

(custom-set-faces!
  '(org-document-title :height 1.2))

(setq org-fontify-quote-and-verse-blocks t
      doom-themes-org-fontify-special-tags nil)

;;;;; Reduced Text Indent
;; TODO: CONF: ORG: reduced text indent (maybe configure this later)

;;;;; Symbols

(setq org-ellipsis " ▾"
      org-priority-default ?A
      org-priority-highest ?A
      org-priority-lowest ?E
      org-priority-faces '((?A . nerd-icons-red)
                           (?B . nerd-icons-orange)
                           (?C . nerd-icons-yellow)
                           (?D . nerd-icons-green)
                           (?E . nerd-icons-blue)))

;; TODO: consider +ligatures-extra-symbols (req config. org-modern)

;;;;; Latex Fragments

(after! org
  (setq org-highlight-latex-and-related '(native script-entities))
  ;; doom already does this
  ;; (plist-put org-format-latex-options :scale 1.5)
  (require 'org-src)
  (add-to-list 'org-src-block-faces '("latex" (:inherit default :extend t))))



;;;;;; Latex Fragments (extras)

;; TODO: CONF: org/latex: auto-preview
;; (add-hook 'org-mode-hook #'org-latex-preview-auto-mode)

;; also: https://tecosaur.github.io/emacs-config/config.html#prettier-rendering

;;;;; Org Plot

;; TODO: CONF: org-plot preamble: https://tecosaur.github.io/emacs-config/config.html#org-plot

;;;; Roam

;; TODO: CONF: org-roam-dailies-capture-templates (req. dc/read-template ... & paths)
;;  - need to fix paths in ~/.emacs.g
;; TODO: CONF: rename org-roam-dailies-directory back to default...?

(defun dc/org-roam-insert-slug ()
  (interactive)
  (insert (org-roam-node-slug (org-roam-node-at-point))))

(defun dc/org-roam-get-slug ()
  (org-roam-node-slug (org-roam-node-at-point)))

;; doom loads roam via :hook (org-load . +org-init-roam-h)
(setq org-roam-directory (expand-file-name "roam" org-directory)
      org-roam-dailies-directory "dailies/"
      ;; TODO: CONF: org-roam-extract-new-file-path "${slug}-%<%Y%m%d%H%M%S>-.org"
      org-roam-list-files-commands '(fd fdfind rg find)
      org-roam-db-gc-threshold most-positive-fixnum
      org-roam-mode-section-functions #'(org-roam-backlinks-section
                                         org-roam-reflinks-section)
      org-roam-completion-everywhere nil ;; this is getting turned on anyways
      org-roam-capture-templates
      (append
       '(("n" "Note")
         ("d" "Default"
          plain "%?" :unnarrowed t
          :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n"))
         ("p" "Project"
          plain "%?" :unnarrowed t
          :target (file+head
                   "projects/${slug}.org"
                   "#+TITLE: ${title}\n#+DESCRIPTION: ${description}\n"))
         ("t" "Topic"
          plain "%?" :unnarrowed t
          :target (file+head+olp
                   "topics/${slug}.org"
                   "#+TITLE: ${title}\n#+DESCRIPTION: ${description}\n#+TAGS:\n\n"
                   ("Roam" "Docs" "Resources" "Topics" "Issues")))
         ("c" "Code"
          plain "%?" :unnarrowed t
          :target (file+head
                   "code/${slug}.org"
                   "#+TITLE: ${title}\n#+DESCRIPTION: ${description}\n#+TAGS:\n\n")))
       `(("z" "Zettel"
          plain "%?" :unnarrowed t
          :target (file+head+olp
                   "slips/%<%Y%m%d%H%M%S>-${slug}.org"
                   ,(string-join '("#+TITLE: ${title}"
                                   "#+CATEGORY: slips"
                                   "#+TAGS: ") "\n")
                   ("Roam" "Docs" "Resources" "Issues" "Projects"))))))

;;;;; Roam UI

;;;;; Noter
;; TODO: PKG: org-noter (req. determining cd/aca-notes-path)
;; (use-package! pdf-tools
;;   :defer t
;;   automatically check/ask either on first frame or on first PDF
;;   :config ())

;;;; Exports

;;;;; General

;; to match the LaTeX article's five levels
(setq org-export-headline-levels 5)

(after! org
  (require 'ox-extra)
  (ox-extras-activate '(ignore-headlines)))

;;;;;; Acronym Formatting

;;;;; HTML

;;;;; LaTeX

;;;;; Beamer

;;;;; Reveal

;;;; Babel

;;;;; Google Translate

(defun google-translate--search-tkk ()
  (list 430675 2721866130))
(defun my-google-translate-at-point ()
  "reverse translate if prefix"
  (interactive)
  (if current-prefix-arg
      (google-translate-at-point)
    (google-translate-at-point-reverse)))

(use-package! google-translate
  ;; (:bind "C-T" #'my-google-translate-at-point)
  :custom (google-translate-backend-method 'curl))


(after! ob
  (use-package! ob-translate :defer t)
  ;; TODO :custom ob-mermaid-cli-path "~/.nix-profile/bin/mmdc"
  (use-package! ob-mermaid :defer t :custom (ob-mermaid-cli-path "mmdc")))

;;;;;; Wiktionary

;; TODO: https://github.com/umanwizard/emacs-wiktionary


;;; Programming
;;
;;;; Tree Sitter

;;(setq treesit-extra-load-path
;;      (list (expand-file-name ".local/lib/tree-sitter" (getenv "HOME")))
;;      treesit-language-source-alist
;;      '(;; (astro . ("https://github.com/virchau13/tree-sitter-astro"))
;;        (yaml . ("https://github.com/ikatyang/tree-sitter-yaml"))))
;;
;;(after! treesit
;;  (cl-loop for lang-key
;;           in (a-keys treesit-language-source-alist)
;;           unless (treesit-language-available-p lang-key)
;;           do (treesit-install-language-grammar lang-key)))

;;;; LSP

;; (dc/lsp-json-pp lsp--client-capabilities)
;; (dc/lsp-json-pp lsp--server-capabilities)

(defun dc/lsp-json-serialize (params)
  ;; (let ((json-encoding-pretty-print t))) ; can't extend inside macro scope?
  (lsp--json-serialize params))

(defun dc/lsp-json-pp (params)
  (with-temp-buffer
    (insert (dc/lsp-json-serialize params))
    (json-pretty-print (point-min) (point-max))
    (buffer-string)))

;; M-x lsp-describe-sessions
;; M-x lsp-workspace-show-log

;; TODO: how to manage eglot/lsp
;; - i would actually prefer eglot more often than not.

;; TODO: determine how to manage functionality per guix-system & foriegn-dist
;; - i.e. guix-system no likey npm install bigbloobfromsky (neither do i)
;; - tools shud just be written in rust for umm... a great memory safety good

;; TODO: ponder deeply the deepest unasked questions of the cosmos
;; TODO: CONF: per-project `lsp-diagnostics-provider'? fly{check,make}

;;;;; LSP Default Functionality

;; See: https://emacs-lsp.github.io/lsp-mode/tutorials/how-to-turn-of
;;
;; `lsp--auto-configure' manages `lsp-mode' functionality (and then, doom)
;;
;; + activates `lsp-ui-mode' if fn defined
;; + ensures: `yas-inhibit-overlay-modification-protection' t
;; + toggles functions on `lsp-configure-hook' for:
;;   - `lsp-headerline-breadcrumb-enable' => `lsp-headerline-breadcrumb-mode'
;;   - `lsp-modeline-code-actions-enable' => `lsp-modeline-code-actions-mode'
;;   - `lsp-modeline-diagnostics-enable' => `lsp-modeline-diagnostics-mode'
;;   - `lsp-modeline-workspace-status-enable' => `lsp-modeline-workspace-status-mode'
;;   - `lsp-lens-enable' => `lsp-lens--enable'
;;   - `lsp-semantic-tokens-enable' => `lsp-semantic-tokens--enable'

;; TODO: set lsp-disabled-servers or lsp-enabled-servers
;;
;;   These are checked in `lsp--supports-buffer?', setting anything in
;;   `lsp-enabled-servers' will stop checking before `lsp-disabled-servers' is
;;   considered. `lsp-enabled-servers' is also effectively checked elsewhere,
;;   but these act like a typical whitelist/blacklist
;;
;; TODO: decide on `lsp-auto-guess-root' (to avoid popup)

(setq-default
 lsp-enable-suggest-server-download nil
 lsp-keep-workspace-alive nil           ; custom
 lsp-enable-symbol-highlighting t       ; doom default
 lsp-lens-enable t              ; TODO: enable lsp-lens, but hook per-language
 lsp-headerline-breadcrumb-enable t     ; custom

 ;; lsp-ui-doc
 lsp-ui-doc-enable t
 lsp-ui-doc-show-with-cursor nil
 lsp-ui-doc-show-with-mouse t

 ;; lsp-eldoc
 lsp-eldoc-enable-hover t               ; doom default

 ;; lsp-ui-sideline
 lsp-ui-sideline-enable t               ; doom default
 lsp-ui-sideline-show-code-actions nil  ; doom default, enable per-project?

 ;; lsp-diagnostics
 lsp-diagnostics-provider :auto         ; doom default

 ;; lsp-modeline
 lsp-modeline-code-actions-enable t        ; doom default
 lsp-modeline-diagnostics-enable t         ; doom default
 lsp-modeline-diagnostics-scope :workspace ; doom default

 ;; lsp-signature
 lsp-signature-auto-activate '(:on-trigger-char :on-server-request)
 lsp-signature-render-documentation t   ; doom default

 ;; lsp-completion
 lsp-completion-provider :none          ; doom default, maybe :capf
 lsp-completion-show-detail t           ; doom default
 lsp-completion-show-kind t             ; doom default
 )

(after! lsp-mode
  (set-popup-rules!
    '(("^\\*LSP Error List*" :side left :vslot -5 :slot 0 :width 40 :select t :quit nil)))
  (advice-add 'lsp! :override #'ignore))

;; i'm starting way too much of this for random config files
;;
;; TODO: re-enable lsp-mode for specific modes
;;
;; (def-project-mode! doom-ecto-projects-mode
;;   :match (rx-to-string (string-join (list dc/ecto-path) "/"))
;;   :on-enter (remove-hook! ... #'lsp!))


;;;; Sesman
(use-package! sesman
  :defer t
  :config
  (set-popup-rules!
    '(("^\\*sesman" :side bottom :vslot -5 :slot -5 :width 80 :select t :quit nil))))

;;;; Lisp

;; fixes issues navigating by lispy outline, but with advice below,
;; +el-outline-regexp no longer overrides lispy-outline-level
;;
;; protesilaos: "organize your init file with outline-minor-mode"
;;
;; https://www.youtube.com/watch?v=Dkoy3NrLN9g ... dammit
;;
;; (setq-hook! 'emacs-lisp-mode-hook
;;   +emacs-lisp-outline-regexp ";;\\(?:;[^#]\\|\\*+\\)"
;;   outline-regexp +emacs-lisp-outline-regexp)
;;
;; although it doesn't seem to highligh faces of comments.

(use-package! prism
  :commands prism-mode prism-whitespace-mode
  :config
  (map! :map doom-leader-toggle-map "M-p" #'prism-mode))

;;;;; Emacs Lisp
(after! flycheck
  (unless (memq 'emacs-lisp-checkdoc flycheck-disabled-checkers)
    (setq flycheck-disabled-checkers
          (cons 'emacs-lisp-checkdoc flycheck-disabled-checkers))))

(defvar +df-emacs-config-regexp
  (rx-let ((dir-df (literal (expand-file-name "~/.dotfiles")))
           (dir-home (literal (expand-file-name "~"))))
    (rx bol
        (| (and dir-home (| "/.emacs.d" "/.emacs.g"))
           (and dir-df (| "/.doom.d" "/.emacs.hop" "/.emacs.console" "/.emacs.d")))
        (* anything) ".el" eol))
  "Regexp to match emacs-lisp files for `+df-emacs-config-mode'.")

;; disable emacs-checkdoc flycheck in the above directories
(def-project-mode! +df-emacs-config-mode
  :match +df-emacs-config-regexp
  :modes '(emacs-lisp-mode)
  :on-enter
  (unless (memq 'emacs-lisp-checkdoc flycheck-disabled-checkers)
    (setq-local flycheck-disabled-checkers
                (cons 'emacs-lisp-checkdoc flycheck-disabled-checkers))))


;;;;; Yuck
(use-package! yuck-mode)

;;;;; Common Lisp
(use-package sly :defer t
  :custom (inferior-lisp-program '("guix" "shell" "sbcl" "--" "sbcl")))

;;;;; Scheme

;;;;;; Geiser

(use-package! geiser
  :defer t
  :config
  (add-to-list 'geiser-implementations-alist '(((regexp "\\.scm$") guile)))
  :custom
  ;; TODO: PKG: project.el -- maybe update to geiser-repl-project-root
  (geiser-repl-current-project-function #'projectile-project-root)
  (geiser-repl-add-project-paths
   nil
   "`guix-load-path' seems to append using `add-to-list', so whether the
.dotfiles channel is added via that or `geiser-repl-add-project-paths',
the result is the same, unless the project's guile modules are not at
the root")
  (geiser-debug-treat-ansi-colors 'colors "Requires guile-colorized (ice-9 colorized)")
  (geiser-default-implementation 'guile)
  (geiser-repl-highlight-output-p t))

;; req. for lispy? even with master?
(use-package! geiser-racket :defer t :after geiser)

(use-package! geiser-guile
  :defer t
  :after geiser
  :config
  (add-to-list 'geiser-guile-manual-lookup-nodes "Geiser")
  (add-to-list 'geiser-guile-manual-lookup-nodes "Guile Reference")
  (add-to-list 'geiser-guile-manual-lookup-nodes "Guile Library")
  (add-to-list 'geiser-guile-manual-lookup-nodes "Guix"))

;; don't change these (or move the files). geiser will not indicate whatsover
;; (not even in logs) that something is wrong. it won't compile a module you
;; switch to (a clue).  it also won't recognize %load-path.
;;
;; (geiser-guile-init-file nil)
;;
;; (geiser-guile-load-init-file nil)

;; TODO: flycheck-guile: fix load path (probably just use .envrc)
;;
;; if `geiser-repl-add-project-paths', it prepends only "." onto
;; `geiser-guile-load-path' and doesn't permit injecting anything else. this
;; results in zero information being displayed if modules aren't loaded

(use-package! flycheck-guile
  :defer t
  :after geiser-guile
  :config (add-to-list 'flycheck-disabled-checkers 'guile))

;;;;;; Arei

;; TODO: manage GUILE_LOAD_PATH for arei? it's incorrect on arch profile for
;; now (and being loaded from .envrc)

;; the arei package hooks itself already. .dir-locals.el may need an update.
;;
;; run guile-ares-rs server externally, then connect using sesman-start
(use-package! arei
  :defer t
  :config
  (set-popup-rules!
    '(("^\\*arei:" :side bottom :vslot -5 :slot -5 :width 80 :select t :quit t))))

;; ---------------------------------------------
;;
;; I'm using `guix-scheme-mode' for now, which relies on an absolute regexp
;;
;; NOTE: guix.el defines a separate `guix-scheme-mode', but if the geiser
;; packages are activated after guix.el, it may override that entry in
;; `auto-mode-alist'. `guix-scheme-mode' runs `guix-pretty-print-buffer', but
;; this doesn't directly depend on geiser. most guix.el functionality relies
;; on a separate repl AFAIK (the global functionality like popup does, can't
;; remember about project-specific repls). `geiser-mode' will not activate: it
;; activation function tests `(eq major-mode 'scheme-mode)'
;;
;; so, there may be some issues with guix.el where it pulls Geiser
;; functionality back into scheme-mode (i think maybe it's okay). i would try,
;; but i'd rather just have something consistent.
;;
;; file-local variables could help force guix-scheme-mode ... but only in your
;; own projects.
;;
;; otherwise, one could just set up set-{repl,eval,lookup}-handlers! for
;; `guix-scheme-mode' and point these at arel.el functionality.
;;
;; https://github.com/doomemacs/doomemacs/blob/master/modules/lang/scheme/config.el#L18
;; ---------------------------------------------

;;;;; Lispy
;; lispy-outline: "^[ 	]*;;;\\(;*\\**\\) [^ 	\n]"
;;   doom: "^;;\\(?:;[^#]\\|\\*+\\)"
(use-package! lispy
  :defer t
  :hook 'lisp-data-mode-hook
  :custom ((lispy-x-default-verbosity 1 "Show more info for `lispy-x'"))
  :config
  (setq lispy-outline "^;;\\(?:;[^#]\\|\\*+\\)")
  ;; removing advice fixes consult-outline
  (advice-remove #'lispy-outline-level #'+emacs-lisp-outline-level))

;; TODO: to implement for arel:
;;
;; + patch lispy-describe-inline to avoid the direct geiser reference
;; + add le-arel (or casify the logic in le-scheme).
;;   + implement behavior in lispy--eval-scheme-mode
;;   + casify lispy-goto-symbol-scheme
;; + customize lispy-goto-symbol-alist: add arel's repl mode & decouple scheme mode
;;
;; this should get you between 80% to -20% there.


;;;; Web

;;;;; HTML

;;TODO Web: ensure emmet-mode is configured

;; html-ts-mode requires mhtml-ts-mode, but needs html-ts-mode-indent-offset set
;; (setq-default html-ts-mode-indent-offset 2) ;; NOTE remove if unnecessary
(use-package mhtml-ts-mode :defer t)

(use-package! apheleia
  :config
  (dolist (webml '(html-mode html-ts-mode mhtml-mode web-mode))
    ;; aphelia: prettier instead of prettier-html, so it relies on .prettierrc
    (add-to-list 'apheleia-mode-alist `(,webml . prettier)))
  (setq apheleia-mode-alist             ; '(nix-mode treefmt nix-ts-mode treefmt)
        (apply #'a-assoc apheleia-mode-alist '(nix-mode nixfmt nix-ts-mode nixfmt))))

;;;;; Tailwind

;; see https://emacs-lsp.github.io/lsp-mode/page/faq/#i-have-multiple-language-servers-registered-for-language-foo-which-one-will-be-used-when-opening-a-project

(use-package lsp-tailwindcss
  :init (setq lsp-tailwindcss-add-on-mode t
              ;; set in .dir-locals.el, along with lsp-disabled-servers
              ;; ... or just use it for rustywind (i can 't get it to work)
              lsp-tailwindcss-major-modes nil))


;;;;; Astro

(use-package astro-ts-mode
  :mode "\\.astro\\'")

;;;;; Javascript
(use-package! websocket :defer t)

;; (defvaralias 'js2-basic-offset 'js-indent-level)
(use-package! js-mode :defer t :custom (js-indent-level 2))

;;;; Scripting

;;;;; Shell

(use-package! flymake-shellcheck
  :commands flymake-shellcheck-load
  :config (add-hook 'sh-mode-hook 'flymake-shellcheck-load))

(use-package! vterm
  :defer t
  :custom
  (vterm-max-scrollback 1000)
  (vterm-set-bold-hightbright t)
  :config
  (map! :map dc/quick-map "M-v" #'vterm))

;; TODO: per-project vterm
;; https://github.com/doomemacs/doomemacs/blob/master/modules/term/vterm/autoload.el

;;;; Compiled Langs

(use-package! zig-mode
  ;; :custom (lsp-zig-zls-executable . "~/.fdsa")
  :defer t)

(use-package! java-mode
  ;; but now my smartparens would be dumb... lispy >> smartparens
  ;; :config (add-to-list 'sp-ignore-modes-list 'java-mode)
  :defer t)

;;;; Functional Langs

;;;; Other Langs

;;;; Compile


;;;;; Compile Multi
;;
;; TODO: CONF: set a default compile-multi-config
;; - use a function to parse out targets from makefile
;; - https://github.com/mohkale/compile-multi?tab=readme-ov-file#actions
;;
;; the extensions here will load when the `compile-multi' command is run. no
;; :defer is needed. doomemacs defers loading of consult.
(use-package! compile-multi :after consult)
(use-package! consult-compile-multi
  :after (consult compile-multi)
  :config (consult-compile-multi-mode))
(use-package! compile-multi-nerd-icons
  :after (compile-multi nerd-icons-completion))
(use-package! compile-multi-embark
  :after (compile-multi embark)
  :config (compile-multi-embark-mode +1))

;;; VCS
;;;; Git
(use-package! git-link
  :commands git-link git-link-commit git-link-homepage
  :config (map! :leader
                "v M-g l" #'git-link
                "v M-g c" #'git-link-commit
                "v M-g h" #'git-link-homepage))

(use-package! git-timemachine
  :defer t
  :bind ((:map doom-leader-toggle-map ("G" . #'git-timemachine-toggle))))

;;;; Diff
;;;;; Patches
;;;;; Smerge
(after! hydra
  (require 'smerge-mode)
  (defhydra dw/smerge-panel ()
    "smerge"
    ("k" (smerge-prev) "prev change" )
    ("j" (smerge-next) "next change")
    ("u" (smerge-keep-upper) "keep upper")
    ("l" (smerge-keep-lower) "keep lower")
    ("q" nil "quit" :exit t)))

;;;;; Emerge
;;
;;;; Magit

(use-package! magit
  :custom (magit-delete-by-moving-to-trash nil))

;; :config (setq magit-display-buffer-function
;;               #'magit-display-buffer-same-window-except-diff-v1)

(after! magit
  (use-package! magit-todos
    :config (magit-todos-mode 1))

  ;; magit-tbdiff commands interfaced via a transient
  (use-package! magit-tbdiff))

;;;; Ghub

;;;; Forge
(use-package! forge
  :defer t
  :after (:all magit graphql ghub)
  :config
  (setq forge-bug-reference-remote-files nil)
  (add-to-list 'forge-owned-accounts '("dcunited001" "aionfork"))
  (add-to-list 'forge-alist
               '("invent.kde.org"
                 "invent.kde.org/api/v4"
                 "invent.kde.org"
                 forge-gitlab-repository))
  (add-to-list 'forge-alist
               '("gitlab.freedestkop.org"
                 "gitlab.freedesktop.org/api/v4"
                 "gitlab.freedesktop.org"
                 forge-gitlab-repository)))


;; (set-popup-rule! "^\\*?[0-9]+:\\(?:new-\\|[0-9]+$\\)" :size 0.45 :modeline t :ttl 0 :quit nil)
;; (set-popup-rule! "^\\*\\(?:[^/]+/[^ ]+ #[0-9]+\\*$\\|Issues\\|Pull-Requests\\|forge\\)" :ignore t)
;; => ("^\\*\\(?:[^/]+/[^ ]+ #[0-9]+\\*$\\|Issues\\|Pull-Requests\\|forge\\)" nil)
(after! forge
  (map! :map forge-topic-mode-map
        "c" #'forge-create-post
        (:prefix ("e" . "FORGE-EDIT")
                 "a" #'forge-edit-topic-assignees
                 "d" #'forge-edit-topic-draft
                 "k" #'forge-delete-comment
                 "l" #'forge-edit-topic-labels
                 "m" #'forge-edit-topic-marks
                 ;; "M" #'forge-merge
                 "n" #'forge-edit-topic-note
                 "p" #'forge-edit-post
                 "r" #'forge-edit-topic-review-requests
                 "s" #'forge-edit-topic-state
                 "t" #'forge-edit-topic-title)))

;; to recreate database, M-x forge-reset-database, then list repositories to
;; set up schema

;;;;; Forge Functionality

(defun dc/forge-repository-toggle-selective-p (repository)
  "Update `repository' by toggling `selective-p'."
  (interactive (list (forge-read-repository "Toggle repository :selective")))
  (let ((repo-was-selective-p (oref repository selective-p))
        (repo-id (oref repository id)))
    ;; (pp repo-was-selective-p)
    ;; (pp repo-id)
    (forge-sql [:update repository
                :set (= [selective_p] $s1)
                :where (= id $s2)]
               (not repo-was-selective-p) repo-id)))
;;;;; Forge Setup

(defun dc/forge-all-marks ()
  "Gets generic marks from the `forge-database'."
  (let ((arg nil))
    ;; `forge-sql-cdr' will drop the first column from :select (?)
    ;; so (->> (mapcar #'car) marks) returns a list of id's there
    (forge-sql-cdr
     `[:select * :from mark
       ,@(and arg '(:where (in name $v1)))
       :order-by [(asc name)]]
     (vconcat arg))))

;; (forge-create-mark "test2" 'modus-themes-mark-alt "modus-themes-mark-alt")

;; `forge-edit-mark' (interactive ...) lists the the marks, displays the name
;; via (->> (mapcar #'car) marks) ... which doesn't drop id there.
;;
;; after selection, (interactive ...) result acts like a (let ...) environment
;; binding and morphs the function args into what gets passed to the body
;;
;; then forge sql updates one mark by keying on the UUID in `id'

;; select * from mark; delete from mark;
;;
;; |"uuid1"|"testmark"|modus-themes-nuanced-cyan|"modus-themes-nuanced-cyan"
;; |"uuid2"|"test2"|modus-themes-mark-alt|"modus-themes-mark-alt"

(defvar dc/forge-nuanced-marks
  ;; nothing to key on unless the UUIDs are external
  '(("f8524701-93d9-4aad-a894-44de11fbf20c" "WATCH" modus-themes-nuanced-blue "modus-themes-nuanced-blue")
    ("7c1c2f6a-b5aa-4ba1-84d5-026510f849f5" "LINK" modus-themes-nuanced-cyan "modus-themes-nuanced-cyan")
    ("cbac5d21-f162-430c-8b55-197482a54d9f" "LEARN" modus-themes-nuanced-green "modus-themes-nuanced-green")
    ("99cd5d9a-e135-4eac-8e95-cadbbe73a301" "CHECK" modus-themes-nuanced-magenta "modus-themes-nuanced-magenta")
    ("b75c358b-56fa-4f4d-9a1e-a925d3000716" "TODO" modus-themes-nuanced-red "modus-themes-nuanced-red")
    ("d5b688a6-4d35-4535-b426-24828cc54db6" "WAIT" modus-themes-nuanced-yellow "modus-themes-nuanced-yellow")))

;; can't re-init with UUIDs in tact
(defun dc/forge-create-nuanced-marks ()
  ;; (all-marks (dc/forge-all-marks)) ; no need 2 accumulate non-extant marks
  (mapc (lambda (m) (apply #'forge-create-mark m)) dc/forge-nuanced-marks))

;; (dc/forge-create-nuanced-marks)

(defun dc/forge-edit-nuanced-marks (&optional marks)
  (let ((new-marks (or marks dc/forge-nuanced-marks)))
    (mapc (lambda (m) (apply #'forge-edit-mark m)) new-marks)))

;; (dc/forge-edit-nuanced-marks dc/forge-nuanced-marks)

;;;;; Sourcehut
;;;;; Repo

;;; Tools

;;;; Direnv
(use-package! envrc
  :config
  (map! :map dc/quick-map
        (:prefix ("E" . "ENVRC")
                 "r" #'envrc-reload-all
                 "g" #'envrc-global-mode)))

;;;; Mise
(use-package! mise
  :hook (after-init-hook . global-mise-mode))

;;;; Auth
;;
;;;;; epa
(require 'epg)

;;;;; pinentry

(setopt epg-user-id user-mail-address)
;; (setopt epg-pinentry-mode nil) ; cancel/ask/loopback
;; (setq epg-debug t)

;;;;; auth-source-pass
;; via doom

;;;; Docs
;; (external docs: dash/tldr)

;;;;; Dash
;;
;; Dash autodownloads, but built docsets should be in both: (todo pkg/make)
;;
;; ~/.var/app/org.zealdocs.Zeal/data/Zeal/Zeal/docsets/*
;; ~/.emacs.doom/.local/etc/docsets

(use-package! dash-docs
  :config
  (setq dash-docs-browser-func #'browse-url)
  (set-docsets! '(nix-mode) "nix" "nixos")
  (set-docsets! '(sh-mode sh-base-mode org-mode) "jq" "Bash")
  ;; hard to lookup for make
  (set-docsets! '(makefile-mode) "GNU_Make" "GNU_Coding_Standards"))

;; can set context with [DOCSET FORM]
;;
;; (set-docsets! '(js2-mode rjsx-mode) "JavaScript"
;;   ["React" (eq major-mode 'rjsx-mode)]
;;   ["TypeScript" (bound-and-true-p tide-mode)])

;;;;; Man

(use-package! man
  :custom (manual-program (if is-nixos "/run/current-system/sw/bin/man" "man")))

;; M-x async-shell-command manpath
;; /gnu/store/0pn3fjlfmvyjc9g29hzlgprvfchkv6ld-profile/share/man
;; /run/current-system/sw/share/man
;; /home/dc/.guix-profile/share/man
;; /etc/profiles/per-user/dc/share/man
;; /nix/store/bflsjj2cndl8fz690nx8aigf2x3q16d4-newt-0.52.24/share/man
;; /nix/store/avhdfiwxm991wgmcgvmhmvgvwn9gavq6-python3-3.12.11-env/share/man
;; /nix/store/gwk546kxw024v371l34sw11zvzqrxhdv-dmenu-5.3/share/man
;; /nix/store/7gspl5402q53m36mavbq3rxxlh70kqfv-pciutils-3.13.0/share/man
;; /gnu/store/m3y5v726hbdkpnh8zhad47ni82bna6ki-gzip-1.14/share/man           #  missing
;; /gnu/store/lzf20vxz8rq5d1akv907c1g3a0mq2z01-coreutils-9.1/share/man       #  missing

;; nixos
;; /home/dc/.local/share/man              # missing
;; /run/current-system/sw/share/man
;; /home/dc/.guix-profile/share/man
;; /etc/profiles/per-user/dc/share/man
;; /nix/store/bflsjj2cndl8fz690nx8aigf2x3q16d4-newt-0.52.24/share/man
;; /nix/store/avhdfiwxm991wgmcgvmhmvgvwn9gavq6-python3-3.12.11-env/share/man
;; /nix/store/gwk546kxw024v371l34sw11zvzqrxhdv-dmenu-5.3/share/man
;; /nix/store/7gspl5402q53m36mavbq3rxxlh70kqfv-pciutils-3.13.0/share/man

;;;;; Info
(use-package! info
  :defer t)
(use-package! info+
  :after info
  :custom
  (Info-breadcrumbs-depth 4)
  (Info-breadcrumbs-depth-internal 6)
  (Info-breadcrumbs-in-header-flag t)
  ;; (Info-saved-history-file (expand-file-name
  ;;                           "info-history"
  ;;                           no-littering-var-directory))
  ;; TODO: (Info-apropos-manuals (dc/Info-manuals))

  :config
  ;; these would never run as emacs-startup-hook
  (Info-breadcrumbs-in-mode-line-mode)
  (Info-persist-history-mode +1))

;;;; Systems

;;;;; Emacs
;; via tecosaur (open elp/etrace output in flamegraphs)
;;
;; open in: speedscope https://www.speedscope.app/ or chrome://tracing
(use-package! etrace :after elp)

;;;;; Arch
(use-package! aurel :defer t)

;;;;; Guix
;;
;; guix.el gets built into the guix profile for doomemacs, so it doesn't need
;; use-package.
;;

(defun dc/guix-scheme-mode-regexp (path)
  (rx (and (literal path) "/" (+ any) ".scm")))

(setopt guix-load-path (list (expand-file-name ".dotfiles/ellipsis" (getenv "HOME"))
                             (expand-file-name ".dotfiles/dc" (getenv "HOME"))))

;; TODO: GUIX: maybe set guix-load-compiled-path
;; see [[file:~/.emacs.doom/.local/straight/repos/emacs-guix/elisp/guix-repl.el::defun guix-repl-guile-args]]

;; TODO: defer guix?
(use-package! guix
  :demand t
  :init (require 'ffap)
  :bind ((:map dc/quick-map
               ("g <SPC>" . #'guix)
               ("gX" . #'guix-extended-command)
               ("gh" . #'guix-hash)
               ("gsb" . #'guix-switch-to-buffer)
               ("gsr" . #'guix-switch-to-repl)))
  :config
  (require 'guix-ui)
  ;; demand for now, for `guix-pulled-profile' and `guix-scheme-mode'. this
  ;; will also load geiser/guile at start. (require 'guix-ui) is necessary,
  ;; otherwise guix-pulled-profile doesn't exist
  (add-to-list 'auto-mode-alist (cons (dc/guix-scheme-mode-regexp (expand-file-name ".dotfiles" (getenv "HOME"))) 'guix-scheme-mode))
  (add-hook 'guix-scheme-mode-hook 'prism-mode)
  (setopt guix-devel-ffap-patch-directories (flatten-list (list guix-pulled-profile "patches"))))

;; guix-pulled-profile won't be bound until after guix loads
;; :config ...

;; NOTE: guix-load-path needs to be done outside of use-package! or hook for
;; some reasons -- e.g. guix-repl.el isn't loaded by use-package, where
;; guix-load-path is defined. it looks like it's intended to be managed by
;; GUILE_LOAD_PATH, (setq guix-load-path ...) or in .dir-locals.el

;;;;;; recutils: manipulate guix cli output
(use-package! rec-mode
  :defer t
  :config
  (require 'ob-rec))

;;;;; Nix

;; use these servers on-demand
(after! lsp-mode
  (setq lsp-disabled-clients (append lsp-disabled-clients '(nix-nil rnix-lsp))))

(def-project-mode! +df-nixos-mode
  :match "\\(?:/home/dc/\\.dotfiles/nixos/.*\\.nix\\)"
  :add-hooks (list #'lsp)
  :modes '(nix-mode nix-ts-mode))

(defconst dc/nixos-dotfiles-flake
  (expand-file-name ".dotfiles/nixos/flake.nix" (getenv "HOME")))
(use-package! nix-mode
  :defer t
  :config
  (setq lsp-nix-nixd-server-path "nixd"
        lsp-nix-nixd-formatting-command [ "nixfmt" ]
        lsp-nix-nixd-nixpkgs-expr "import <nixpkgs> { }"
        lsp-nix-nixd-home-manager-options-expr
        "(builtins.getFlake \"/home/nb/nixos\").homeConfigurations.\"nb@mnd\".options"
        lsp-nix-nixd-nixos-options-expr
        (format
         "(builtins.getFlake \"%s\").nixosConfigurations.mnd.options"
         dc/nixos-dotfiles-flake)))


;;;;; Unix

(use-package! elf-mode
  :commands elf-mode
  :magic ("ELF" . 'elf-mode))

(use-package! crontab-mode)

;; this library should fontify when you run `syslog-show-notes'
;;
;; for some reason, it doesn't. I've changed the `syslog-notes-files' location because it
;; doesn't account for the file having been loaded from a .eln build
;;
;; my one-line patch to disable the older read-only mode is still necessary, though
;; maybe that's interfering with the fontification.
;;
;; it pulls notes from a lot of manpages, but does some impressive work to
;; look that up by searching relative to the paths listed in the strace. i
;; think this tool was intended to be used for deeper kernel development,
;; judging by that and by the need for a `strace_notes.el' file ... does that
;; really need to be changed as a variable? apparently so.
(use-package! syslog-mode
  :custom
  (syslog-setup-on-load t)
  (syslog-notes-files
   (let ((dir (file-name-directory
               (expand-file-name "straight/repos/syslog-mode/strace_notes.el" doom-local-dir))))
     (list (cons ".*\\.strace" (concat dir "strace_notes.el"))
           (cons "^strace:" (concat dir "strace_notes.el"))
           (cons "syslog.*" (concat dir "syslog_notes.el")))))
  :mode "\\(messages\\(\\.[0-9]\\)?\\|SYSLOG\\|\\.s?trace\\)\\'")

;; ("\\(messages\\(\\.[0-9]\\)?\\|SYSLOG\\|\\.s?trace\\)\\'" . syslog-mode)

;; TODO: PROCED: setup keybindings
(use-package! proced
  :commands proced
  :init (map! :map dc/quick-map "p" #'proced)
  :config (map! :map proced-mode-map "sh" #'proced-sort-header))

;;;;; Linux
(use-package! repology
  :commands repology
  :init (map! :map dc/quick-map "r" #'repology))
(use-package! dts-mode :defer t)
(use-package! archive-rpm :defer t)

;;;; Services

;; TODO: DAEMONS.EL: setup keybindings (daemons-systemd-toggle-user)
(use-package! daemons
  :commands daemons
  :custom ((daemons-systemd-is-user t)))

(use-package! systemd
  :defer t
  ;; NOTE ensure this gets built (running doom rebuild or whatever should take care of it)
  :init (require 'systemd (expand-file-name ".local/straight/repos/systemd-mode/systemd.elc" doom-emacs-dir))
  :custom
  (systemd-mode-hook '(flycheck-mode)))

(use-package! journalctl-mode
  :commands journalctl
  :config (map! :leader "oj" #'journalctl))


;;;;; Network
(use-package! pcap-mode :defer t)                ; req tshark from wireshark cli
(use-package! nftables-mode :defer t)
(use-package! yang-mode :defer t)

;; password-store.el? pass.el?
(use-package! terraform-mode
  :defer t
  :custom (terraform-format-on-save t))

;;;;;; ContainerD

;; to configure for either docker/podman, customize:
;;
;; - docker-{command,compose-command,container-tramp-method}
;; - dockerfile-mode-command
;;
;; https://www.rahuljuliato.com/posts/emacs-docker-podman
;;

(use-package! docker
  :commands docker
  :config (map! :map dc/quick-map "d" #'docker))

;;;;;; Terraform/HCL

;;;;;; K8S

;;;;;; SSH

(use-package! ssh-config-mode)

(use-package! x509-mode
  ;; TODO x509-mode-hook: jumping straight to x509 doesn't work well
  ;; :hook (x509-mode-hook . (lambda () (call-interactively 'x509-dwim)))
  :mode (rx "." (| "pem" "cer" "der" "crt" "crl") eos))

;;;;;;; Tramp
;; TODO: TRAMP: check that guix emacs build prepends to tramp-remote-path
(use-package tramp :demand t
  :config
  (require 'tramp-container)
  (cl-dolist (p '("~/.guix-profile/bin"
                  "~/.guix-profile"
                  "/run/current-system/profile/bin"
                  "/run/current-system/profile/sbin"))
    (add-to-list 'tramp-remote-path p))
  (map! :map dc/quick-map
        (:prefix ("T" . "TRAMP")
                 "b" #'tramp-cleanup-all-buffers
                 "c" #'tramp-cleanup-connection
                 "C" #'tramp-cleanup-this-connection
                 "M-c" #'tramp-cleanup-all-connections
                 "M" #'tramp-compat-set-file-modes
                 "d" #'tramp-setup-debug-buffer
                 ;; TODO check tramp-completion-use-auth-sources
                 "g" #'tramp-crypt-add-directory))
  :custom (tramp-default-method "ssh"))

;;;; Data
(use-package! jinja2-mode :defer t)

;;;;; Structure

;;;;;; XML
(require 'dom)
(use-package! esxml :demand t)

;;;;;; Protobuf
(use-package! protobuf :defer t)
;;;;; Database

;;;;; API
(use-package! graphql :after ghub)

;;;;; Visualization

;; gnu plot, graphviz/plot, d2, mermaid, plantuml

(use-package! graphviz-dot-mode
  :defer t
  :init ;; NOTE 2025/9/21 see ppareit/graphviz-dot-mode#87, there is compilation issue
  (setq graphviz-dot (or (and is-nixos (expand-file-name "dot" nixos-profile-path))
                         (and is-guix-system (expand-file-name "dot" guix-profile-path))
                         "/usr/bin/dot")))

(use-package! ob-dot
  :defer t
  :after (graphviz-dot-mode))

(use-package! d2-mode
  :defer t
  ;; must be in path
  :custom (d2-location "d2"))

(use-package! plantuml-mode
  :defer t
  :mode "\\.puml\\'"
  :custom (plantuml-indent-level 2))

(use-package! mermaid-mode
  :defer t
  :custom (mermaid-mmdc-location "~/.npm-global/bin/mmdc"))

;;;; Misc

;;;; Desktop

;;;;; Hyprland

;; NOTE: need to :demand t because auto-mode-alist is path-based...
;;
;; - but it's tricky because *somehow* conf-mode gets automagically generated
;;   and shimmed onto auto-mode-alist on top (ughhh...)
;;
;; - .dir-locals.el is messy and file-local-variables will simply not load
;;   mode: hyprlang unless it's loaded.
;;
;; - so it looks like file-local-variables & :demand t are needed redundantly
;;
;; - mostly :defer t bc it's a custom package and who knows whether it'll
;;   break ... maybe i can push the tree-sitter-hyprlang upstream, but the
;;   tests fail
;;
;; also note: major-mode-remap-alist (not configured by libs AFAIK) and
;; org-src-lang-modes

;; set up in .dir-locals.el unless using doom
;;
;; (hyprlang-ts-mode
;;  (eval . (add-hook 'hyprlang-ts-mode-hook #'lsp)))

;; (string-match-p df-hypr-rx "/home/dc/.dotfiles/.config/hypr/rules.conf")
(after! hyprlang-ts-mode
  (let* ((df-hypr-path-rx
          (rx-to-string
           (string-join (list (getenv "HOME") ".dotfiles" ".config" "hypr") "/")))
         (df-hypr-rx (concat df-hypr-path-rx "/.*\\.conf")))
    (def-project-mode! +df-hypr-mode
      :match df-hypr-rx
      :modes '(hyprlang-ts-mode)
      :add-hooks '(lsp))))

;; :on-enter (add-hook 'hyprlang-ts-mode-hook #'lsp 'append)

;; i think that (lsp-register-client ...) does this
;; (add-to-list 'lsp-language-id-configuration
;;  '(hyprlang-ts-mode . "hyprlang"))

;; TODO: look into .scm queries used by ts-hyprlang & hyprlang-ts-mode


(defun dc/hyprlang-setup-outline-mode ()
  "It will insert headings but it will not act on them (or highlight them)"
  ;; it otherwise sets #'treesit-outline-search or something (doesn't work here)
  (setq-local outline-level 'outline-regexp
              ;; idk if -highlight works as a local var
              outline-minor-mode-highlight t
              outline-heading-alist '(("### *" . 1)
                                      ("### **" . 2)
                                      ("### ***" . 3))
              outline-regexp (rx bol "### " (+ "*")))
  (outline-minor-mode +1))

(use-package! hyprlang-ts-mode
  :demand t
  :init
  (after! org-src
    (add-to-list 'org-src-lang-modes '("hyprlang" . hyprlang-ts)))
  (after! lsp-mode
    (add-to-list 'lsp-language-id-configuration '(hyprlang-ts-mode . "hyprlang"))
    (lsp-register-client
     (make-lsp-client
      :new-connection (lsp-stdio-connection "hyprls")
      :major-modes '(hyprlang-ts-mode)
      :language-id "hyprlang"
      :priority -1
      :server-id 'hyprls)))
  (after! apheleia-mode
    (add-to-list 'apheleia-formatters '(whitespace . whitespace-cleanup))
    (add-to-list 'apheleia-mode-alist '(hyprlang-ts . whitespace)))
  ;; :hook (hyprlang-ts-mode . dc/hyprlang-setup-outline-mode)
  :hook (hyprlang-ts-mode . (lambda () (setq-local rainbow-html-colors t)))
  :config
  (add-to-list 'major-mode-remap-alist '(hyprlang-mode . hyprlang-ts-mode)))

(use-package! rainbow-mode
  :demand t
  :hook (hyprlang-ts-mode . rainbow-mode))

;;; Applications

;;; Social

;;;; Pastebin

(use-package! 0x0
  :defer t
  :init
  (require 'dc-0x0)
  :config
  (map! :map dc/quick-map
        (:prefix ("0" . "0x0")
                 "-" #'0x0-dwim
                 "t" #'0x0-upload-text
                 "f" #'0x0-upload-file
                 "k" #'0x0-upload-kill-ring
                 "p" #'0x0-popup
                 "u" #'0x0-shorten-uri)))
(after! 0x0
  (setopt 0x0-servers (dc/0x0-retention-policy)))

;;;; Open Source
;;
;;;;; Debbugs
(use-package! debbugs
  :commands debbugs-gnu-bugs debbugs-gnu-guix-search debbugs-gnu-search debbugs-gnu-package
  :custom (debbugs-gnu-default-packages '("guix-patches" "guix"))
  :hook  ((bug-reference-mode-hook bug-reference-prog-mode-hook) . #'debbugs-browse-mode)
  :config (map! :map ctl-x-map
                (:prefix ("G" . "DEBBUGS")
                         "Gb" #'debbugs-gnu-bugs
                         "Gg" #'debbugs-gnu-guix-search
                         "Gs" #'debbugs-gnu-search
                         "Gp" #'debbugs-gnu-package)))

;;; Keymaps

;; TODO: move setup for <f1> <f2> map to after use-package (and after! which-key?)

;;;; Global Remaps

;; "<f1> C-`" #'+popup/diagnose ;; in help-map
(map! :map 'global-map
      "S-<f11>" #'dc/toggle-window-balance

      ;; "C-`" #'+popup/toggle          ; pop the popup
      "C-~" #'+popup/other              ; look around      (+ shift)

      "M-`" #'+popup/buffer             ; window -> popup
      "C-M-`" #'+popup/raise            ; popup -> window  (+ ctrl)
                                        ;   shift is hard to hit

      "C-M-~" #'+popup/restore)         ; where'd it go?   (req. most keys)

;; doesn't easily do the opposite
;;
;; - maybe use cmd!, but there's no push-global-mark-command
;;
;; (map! :map 'ctl-x-map
;;       ;; "C-<SPC>" #'pop-global-mark
;;       "C-M-<SPC>" #'push-mark-command)

;; "<f11>" #'maximize-window
;; "S-<f11>" #'balance-windows

;;;; Help Map

;;;;; Help Map

(map! :map 'help-map
      "C-<SPC>" #'tmm-menubar         ; "^" previous menu ; "PgUp" help buffer
      "C-`" #'+popup/diagnose


      "B" #'embark-bindings
      "M-b" #'embark-bindings-in-keymap

      "M-f" #'list-faces-display

      ;; not interactive, also not sure whether it works (it doesn't...)
      ;; "C-k" (apply-partially #'embark-bindings-at-point)

      "M-k" #'describe-keymap

      "M-m" #'consult-minor-mode-menu

      ;; can insert values with embark
      "M-v" #'getenv)

;; "<" #'help-go-back
;; ">" #'help-go-forward
;; "<XF86Back>" #'help-go-back
;; "<XF86Forward>" #'help-go-forward
;; "<backtab>" #'backward-button
;; "?" #'describe-mode
;; "C-c C-b" #'help-go-back
;; "C-c C-c" #'help-follow-symbol
;; "C-c C-f" #'help-go-forward
;; "DEL" #'scroll-down-command
;; "I" #'help-goto-lispref-info
;; "S-SPC" #'scroll-down-command
;; "c" #'help-customize
;; "g" #'revert-buffer
;; "h" #'describe-mode
;; "i" #'help-goto-info
;; "l" #'help-go-back
;; "n" #'forward-button
;; "o" #'link-hint-open-link
;; "p" #'backward-button
;; "q" #'quit-window
;; "r" #'help-go-forward
;; "s" #'help-view-source

;;;; Search Map

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
      "M-r" #'dc/consult-ripgrep
      "s" #'consult-line-multi          ; "L"
      "S" #'swiper
      ;; "M-s" #'consult-yasnippet
      ;; "M-S" #'consult-yasnippet-visit-snippet-file
      "u" #'consult-focus-lines

      ;; Isearch integration
      "e" #'consult-isearch-history)

;;;; Goto Map
(map! :map 'goto-map
      "a" #'consult-org-agenda
      "e" #'consult-compile-error
      (:prefix ("f" . "FLY")
               "c" #'consult-flycheck
               "m" #'consult-flymake)
      "i" #'consult-imenu-multi
      "I" #'consult-imenu-multi ;; duplicate
      "k" #'consult-global-mark
      "m" #'consult-mark
      "o" #'consult-outline
      (:prefix ("r" . "ROAM")
               "r" #'consult-org-roam-search
               "b" #'consult-org-roam-backlinks
               "f" #'consult-org-roam-file-find
               "l" #'consult-org-roam-forward-links))

;;;; Keypad

;; (after! tmr) ;; (after! forge) ;; these should autoload
(global-set-key (kbd "<kp-insert>") #'tmr-tabulated-view)
(global-set-key (kbd "<C-kp-insert>") #'tmr)
(global-set-key (kbd "<S-kp-insert>") #'ffap-menu)
(global-set-key (kbd "<kp-delete>") #'forge-dispatch)
(global-set-key (kbd "C-<kp-delete>") #'forge-list-notifications)

;; This gets hooked in ace-window :config
(defun dc/map-kp-aceable-window ()
  (mapc (lambda (k)
          (global-set-key
           (kbd (car k))
           (cmd! (dc/aw-select-nth #'aw-switch-to-window (cdr k)))))
        ;; '"<kp-divide>" "<kp-multiply>" "<kp-subtract>"
        '(("<kp-home>" . 0) ("<kp-up>". 1) ("<kp-prior>" . 2)
          ("<kp-left>". 3) ("<kp-begin>". 4) ("<kp-right>". 5)
          ("<kp-end>". 6) ("<kp-down>". 7) ("<kp-next>". 8))) ;; "<kp-enter">
  ;; C-u #'ace-window => #'aw-swap-window
  ;; #'aw-flip-window
  ;; "<kp-add>"
  ;; (global-set-key (kbd "<kp-insert>") #'aw-flip-window)

  (global-set-key (kbd "C-<kp-add>") (cmd!! #'ace-window 4))) ;; "<kp-delete>"
(dc/map-kp-aceable-window)


;; FIXME: the hook gets set. the code works, but it doesn't run
(add-hook! 'server-mode-hook
           ;; (when server-mode) ;; still doesn't run
           (alert (format "Loaded Emacs Server\n\nConfig: %s\nSocket: %s"
                          doom-user-dir
                          (expand-file-name server-name server-socket-dir))
                  :title "Doom:"))

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
