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

;;* Emacs

;;** User

(setq-default user-full-name "David Conner"
              user-mail-address (or (getenv "EMAIL") "noreply@te.xel.io"))

;;** Paths

;; see doom-{emacs,core,data,docs,env,user,etc,cache,modules,...}-dir
;; (defvar dc/emacs-d (expand-file-name "~/.emacs.g/") "TODO: docs.")
;; (defvar dc/emacs-cache (expand-file-name "~/.cache/emacs/") "TODO: docs.")
;; (defvar dc/emacs-dw (expand-file-name "dw" dc/emacs-d) "TODO: docs.")
;; (defvar dc/emacs-doom-modules (expand-file-name "doom/modules" dc/emacs-d))
;; (defvar dc/emacs-modules (expand-file-name "modules" dc/emacs-d) "TODO: docs.")

;;*** Project Paths

(defvar dc/ecto-path (or (getenv "_ECTO") (expand-file-name "~/ecto"))
  "Directory where git-repo projects are checked out.")
(defvar dc/repo-path (or (getenv "_REPO") (expand-file-name "~/repo"))
  "Directory containing XML for git-repo projects are checked out.")
(defvar dc/lang-path (or (getenv "_LANG") (expand-file-name "~/lang"))
  "Directory containing quick projects under ./lang. It typically
contains config under ./.lang to encourage native and portable
12factor language configs, when not container.")

;;*** Guix/Geiser Paths

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
(def-project-mode! doom-straight-read-only-mode
  :match (rx-to-string (string-join (list "" (f-base doom-emacs-dir) ".local" "straight" "") "/"))
  :modes '(emacs-lisp-mode)
  :on-enter (setq-local buffer-read-only t))

;;*** Load Path

(add-to-list 'load-path (expand-file-name ".dotfiles/.emacs.d/lisp" (getenv "HOME")))
(require 'dw-settings)
(require 'dc-util)

;; TODO no-littering?
;;\\(?:;[^#]\\|\\*+\\)

;;** Use Package

(setopt use-package-enable-imenu-support t)

;;** Keymaps (Emacs Native)

;;*** Unbind Globally

(defun dc/unbind-keys (key-names &optional keymap)
  (seq-do (lambda (key)
            (if keymap
                (unbind-key key keymap)
              (unbind-key key)))
          key-names))

;;**** Unbind 2C-mode

;; This mode's global bindings are also bound on C-x 6 {2,s,RET}
(dc/unbind-keys '("<f2> 2" "<f2> b" "<f2> s" "<f2> <f2>"))

;;**** Unbind fullscreen

;; toggle-frame-fullscreen is setting bad state in the parameters to restore
;; and won't toggle back. on f10, KDE just moves focus to emacs' menus
(dc/unbind-keys '("<f10>" "M-<f10>" "<f11>"))

;;**** Unbind kmacro
;;
;; - `kmacro-start-macro-or-insert-macro' not doubly mapped
;; - `kmacro-end-or-call-macro' via `C-x e'
;;
(dc/unbind-keys '("<f3>" "<f4>"))

;; ;;**** Unbind completion-at-point (for cape)

;; (dc/unbind-keys '("C-M-i"))

;;*** Global

(global-set-key (kbd "C-<prior>") #'tab-bar-switch-to-tab)
(global-set-key (kbd "C-<next>") #'tab-bar-switch-to-recent-tab)

;;*** Help Map (native)

(map! :map 'help-map
      "M-k" #'describe-keymap
      "M-f" #'list-faces-display)

;; TODO: these don't work because doom uses [remap]
;; :prefix ("<f1>" . "NATIVEHELP")
;; "f" #'describe-function

;;*** Quick Map (native) <f1> <f2>

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

  "h" #'shortdoc

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

;;* Projects
;;
;;** Project.el
;;
;;*** Projectile

(use-package! projectile
  :custom (projectile-project-search-path `((,dc/repo-path . 1)
                                            (,dc/ecto-path . 3))))

;;*** Activities
(use-package! activities
  :demand t
  :config
  (map! :map dc/quick-map
        (:prefix ("@" . "ACTIVITIES")
                 "@" #'activities-list
                 "m" #'activities-mode
                 "k" #'activities-kill
                 "M-k" #'activities-discard
                 "n" #'activities-define
                 "M-n" #'activities-new
                 "r" #'activities-resume
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

;;* Interface
;;
;;** Basics
;;
;;*** Tooltips

(add-hook! 'doom-init-ui-hook
  (progn
    (menu-bar-mode +1)
    (scroll-bar-mode -1)
    (tool-bar-mode -1)))

;;*** Menus
;;*** Date & Time
;;*** Mouse

;;** Casual

;; TODO: PKG recent-rgrep https://github.com/kickingvegas/recent-rgrep

;;*** Calc Mode For The Plebs
(use-package! casual-suite
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

;;** Search
;;*** Xref
;;*** Grep

;;** Buffers
(setopt global-auto-revert-non-file-buffers t
        auto-revert-verbose nil)

;; TODO: decide on global-auto-revert-mode
;; (global-auto-revert-mode 1)

;;*** Bufler
;;**** Bufler defauto-groups
;;**** Bufler defgroups
;;**** Bufler package

;;** Editor

;;*** Highlighting

;; call unhighlight-regexp, it lists the regexps corresponding to the current
;; highlights
;;
;; otherwise, highlight-symbol-at-point runs this to get the regexp
;; corresponding to the parsed current symbol at point
;;
;; (hi-lock-regexp-okay (find-tag-default-as-symbol-regexp))

;;** UI

(setq display-line-numbers-type nil)

;; doom--setq-outline-level-for-emacs-lisp-mode-h
;; doom--setq-outline-regexp-for-emacs-lisp-mode-h
;; doom--setq-tab-width-for-emacs-lisp-mode-h

;;** Dired

(setq dired-omit-files "^.DS_Store\\'\\|^.project\\(?:ile\\)?\\'\\|^.\\(svn\\)\\'\\|^.ccls-cache\\'\\|\\(?:\\.js\\)?\\.meta\\'\\|\\.\\(?:elc\\|o\\|pyo\\|swp\\|class\\)\\'")

;; TODO: get Doom to; not close all the direds(advice-add #'+doom-)
;;   - dirvish is somewhat responsible for this
(dirvish-override-dired-mode -1)

;;*** Recentf

(after! recentf
  (add-to-list 'recentf-exclude (rx (and line-start "/gnu/store"))))

;;** Alerts
(require 'notifications)
(use-package! alert
  :demand t
  :custom
  (alert-default-style 'libnotify)
  (alert-log-level 'normal))

;;** Confirmations

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

;;** Font

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

;;** Theme
(use-package ef-themes
  :defer t
  :init (setopt doom-theme nil)
  :hook (doom-init-ui-hook . (lambda () (ef-themes-load-random 'dark))))

;;** Windows
(use-package! ace-window
  :commands ace-window aw-show-dispatch-help
  :config
  ;; this logs to messages a bit too often, to engrain the functionality...
  (global-set-key [remap other-window] #'aw-show-dispatch-help)
  (map! :map ctl-x-map "C-o" #'ace-window))

;; TODO buf-move-up: keybinds are not set until used
(use-package! buffer-move
  :commands buf-move-up buf-move-down buf-move-left buf-move-right
  :config
  (map! "<C-S-up>" #'buf-move-up
        "<C-S-down>" #'buf-move-down
        "<C-S-left>" #'buf-move-left
        "<C-S-right>" #'buf-move-right))

;; from tecosaur: ask for buffer after basic window splits
;;
;; TODO: need to distinguish between interactive calls to split-window-*
;;
;; (defadvice! prompt-for-buffer (&rest _)
;;   :after '(split-window-below split-window-right)
;;   (consult-buffer))

;;** Tabs Windows

;;** Completion
;;*** Vertico

;; Doom defaults

;; (add-to-list 'vertico-multiform-categories
;;              '(file (+vertico-transform-functions . +vertico-highlight-directory)))
;; (add-to-list 'vertico-multiform-commands
;;              '(execute-extended-command (+vertico-transform-functions . +vertico-highlight-enabled-mode)))

;; NOTE: vertico--remote-p checks if path is remote (tramp)

;;**** vertico-multiform-{categories,commands}
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

;;*** Corfu

(setq corfu-auto-delay 0.5
      corfu-auto-prefix 3

      ;; the doom defaults
      global-corfu-modes '((not erc-mode circe-mode help-mode gud-mode vterm-mode) t)
      corfu-popupinfo-min-height 5
      corfu-popupinfo-max-height 15
      corfu-popupinfo-direction 'right  ; default list: '(right left down)
      corfu-popupinfo-delay '(1.0 0.5)

      corfu-preview-current nil)

;;*** Orderless

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

(use-package! orderless
  :custom
  (read-buffer-completion-ignore-case t))

;; `completion-category-defaults' is set to nil in doom's vertico/corfu modules

;;*** Consult
(use-package! consult-org-roam
  :after (org-roam consult)
  :custom
  (consult-org-roam-grep-func #'consult-ripgrep)
  (consult-org-roam-buffer-narrow-key consult-narrow-key)
  (consult-org-roam-buffer-after-buffers t))

;; TODO: CONF: consult-org-roam: (consult-customize ...) & (consult-org-roam-mode)

;;*** Marginalia
;; regex to prevent things from popping on screen
;; (setq marginalia-censor-variables nil)
;; TODO: CONF: extend `marginalia-annotator-registry'

;;*** Cape

(after! capf
  (map! :map dc/quick-map "SPC" #'cape-prefix-map))

;;*** Embark


;;* Org

;; + Structure via https://tecosaur.github.io/emacs-config
;; + my old config was mostly porting in Doom's org behavior, so there was a
;;   lot of duplication there

;;** Packages

(setopt org-directory "/data/org")

;;*** Org, itself


;;*** Visuals

;;**** org-modern
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

;;**** org-appear
;; TODO: PKG: configure org-appear tweaks? (later)
;; https://tecosaur.github.io/emacs-config/config.html#emphasis-markers

;;*** Extras
;; TODO: PKG: consider org-glossary

;;**** Importing with pandoc
;; TODO: PKG: configure org-pandoc-import

;;**** Recipes
;; TODO: PKG: configure org-chef (+ capture)

;;** Behavior

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

(remove-hook 'text-mode-hook #'visual-line-mode)
(add-hook 'text-mode-hook #'auto-fill-mode)

;; (setq org-list-demote-modify-bullet
;;       '(("+" . "-") ("-" . "+") ("*" . "+") ("1." . "a.")))

;;*** Citation

;;**** Citar

;; TODO: PKG: citar: expect to manage citar-bibliography as list
;; TODO: PKG: citar: set/manage citar-library-paths, citar-notes-paths

;;**** Zotero
;; TODO: PKG: oc-csl, renders/exports citations with Zotoro CSL styles (req. download)

;; - doom imports citation

;; (after! oc (setq org-cite-export-processors '((t csl))))
;; TODO: CONF: oc: org-cite-export-processors ;; => ((latex biblatex) (t csl))
;; TODO: CONF: org-ref => org-cite https://tecosaur.github.io/emacs-config/config.html#citation,code--7

;;*** cdlatex

;;*** Org Agenda
;; start with empty org-agenda-files
(setq org-agenda-files '()

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

(setq-default org-agenda-span 10
              org-agenda-start-on-weekday nil
              org-agenda-start-day "-3d"
              org-agenda-inhibit-startup t
              org-agenda-deadline-faces '((1.001 . error)
                                          (1.0 . org-warning)
                                          (0.5 . org-upcoming-deadline)
                                          (0.0 . org-upcoming-distant-deadline)))

;;*** Org Super Agenda

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


;;**** org-clock sounds
;; (and (file-exists-p dc/emacs-sound-theme-path)
;;      (setq-default org-clock-sound (expand-file-name "complete.oga"
;;                                                      dc/emacs-sound-theme-path)))


;;*** Capture

;;*** Roam

;;*** Snippets

;;** Visuals
;;*** Font Display

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

;;*** Reduced Text Indent
;; TODO: CONF: ORG: reduced text indent (maybe configure this later)

;;*** Symbols

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

;;*** Latex Fragments

(after! org
  (setq org-highlight-latex-and-related '(native script-entities))
  ;; doom already does this
  ;; (plist-put org-format-latex-options :scale 1.5)
  (require 'org-src)
  (add-to-list 'org-src-block-faces '("latex" (:inherit default :extend t))))



;;**** Latex Fragments (extras)

;; TODO: CONF: org/latex: auto-preview
;; (add-hook 'org-mode-hook #'org-latex-preview-auto-mode)

;; also: https://tecosaur.github.io/emacs-config/config.html#prettier-rendering

;;*** Org Plot

;; TODO: CONF: org-plot preamble: https://tecosaur.github.io/emacs-config/config.html#org-plot

;;** Roam

;; TODO: CONF: org-roam-dailies-capture-templates (req. dc/read-template ... & paths)
;;  - need to fix paths in ~/.emacs.g
;; TODO: CONF: rename org-roam-dailies-directory back to default...?

;; doom loads roam via :hook (org-load . +org-init-roam-h)
(setq org-roam-directory (expand-file-name "roam" org-directory)
      org-roam-dailies-directory "dailies/"
      ;; TODO: CONF: org-roam-extract-new-file-path "${slug}-%<%Y%m%d%H%M%S>-.org"
      org-roam-list-files-commands '(fd fdfind rg find)
      org-roam-db-gc-threshold most-positive-fixnum
      org-roam-mode-section-functions #'(org-roam-backlinks-section
                                         org-roam-reflinks-section)
      org-roam-completion-everywhere nil
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

;;*** Roam UI

;;*** Noter
;; TODO: PKG: org-noter (req. determining cd/aca-notes-path)

;;** Exports

;;*** General

;; to match the LaTeX article's five levels
(setq org-export-headline-levels 5)

(after! org
  (require 'ox-extra)
  (ox-extras-activate '(ignore-headlines)))

;;**** Acronym Formatting

;;*** HTML

;;*** LaTeX

;;*** Beamer

;;*** Reveal

;;** Babel

;;*** Google Translate

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
  (use-package! ob-mermaid :defer t))

;;* Programming
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

;; TODO: how to manage eglot/lsp
;; - i would actually prefer eglot more often than not.

;; TODO: determine how to manage functionality per guix-system & foriegn-dist
;; - i.e. guix-system no likey npm install bigbloobfromsky (neither do i)
;; - tools shud just be written in rust for umm... a great memory safety good

;; TODO: ponder deeply the deepest unasked questions of the cosmos
;; TODO: CONF: per-project `lsp-diagnostics-provider'? fly{check,make}

;;*** LSP Default Functionality

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

;;** Lisp

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

;;*** Emacs Lisp

;;*** Scheme

;;**** Geiser

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

(use-package! geiser-guile
  :defer t
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

(use-package! geiser-racket :defer t)   ; req. for lispy? even with master?

;;**** Arei

;; the arei package hooks itself already. .dir-locals.el may need an update.
;;
;; run guile-ares-rs server externally, then connect using sesman-start
(use-package! arei :defer t)

;; TODO: manage GUILE_LOAD_PATH for arei? it's incorrect on arch for now

;;*** Lispy
;; lispy-outline: "^[ 	]*;;;\\(;*\\**\\) [^ 	\n]"
;;   doom: "^;;\\(?:;[^#]\\|\\*+\\)"
(use-package! lispy
  :defer t
  :hook 'lisp-data-mode-hook
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

;;** Scripting

;;*** Shell

(use-package! flymake-shellcheck
  :commands flymake-shellcheck-load
  :config (add-hook 'sh-mode-hook 'flymake-shellcheck-load))

(use-package! vterm
  :defer t
  :custom
  (vterm-max-scrollback 1000)
  (vterm-set-bold-hightbright t))

;; TODO: per-project vterm
;; https://github.com/doomemacs/doomemacs/blob/master/modules/term/vterm/autoload.el

;;** Compiled Langs

(use-package! zig-mode
  ;; :custom (lsp-zig-zls-executable . "~/.fdsa")
  :defer t)

;;** Functional Langs

;;** Other Langs

;;** Compile


;;*** Compile Multi
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

;;* VCS
;;** Git
(use-package! git-link
  :commands git-link git-link-commit git-link-homepage
  :config (map! :leader
                "v M-g l" #'git-link
                "v M-g c" #'git-link-commit
                "v M-g h" #'git-link-homepage))

;;** Diff
;;*** Patches
;;*** Smerge
(after! hydra
  (require 'smerge-mode)
  (defhydra dw/smerge-panel ()
    "smerge"
    ("k" (smerge-prev) "prev change" )
    ("j" (smerge-next) "next change")
    ("u" (smerge-keep-upper) "keep upper")
    ("l" (smerge-keep-lower) "keep lower")
    ("q" nil "quit" :exit t)))

;;*** Emerge
;;
;;** Magit

(after! magit
  (use-package! magit-todos
    :config (magit-todos-mode 1))

  ;; magit-tbdiff commands interfaced via a transient
  (use-package! magit-tbdiff))

;;** Ghub
;;** Forge

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

;;** Forges
;;*** Sourcehut
;;*** Repo

;;* Tools

;;** Auth
;;
;;*** epa
(require 'epg)

;;*** pinentry

(setopt epg-user-id user-mail-address)
;; (setopt epg-pinentry-mode nil) ; cancel/ask/loopback
;; (setq epg-debug t)

;;*** auth-source-pass
;; via doom

;;** Docs
;; (external docs: dash/tldr)

;;*** Info
(use-package! info :defer t)
(use-package! info+
  :commands info
  :custom
  (Info-breadcrumbs-depth 4)
  (Info-breadcrumbs-depth-internal 6)
  (Info-breadcrumbs-in-header-flag t)
  ;; (Info-saved-history-file (expand-file-name
  ;;                           "info-history"
  ;;                           no-littering-var-directory))
  ;; TODO: (Info-apropos-manuals (dc/Info-manuals))

  :config
  (add-hook 'emacs-startup-hook #'Info-breadcrumbs-in-mode-line-mode)
  (add-hook 'emacs-startup-hook #'Info-persist-history-mode +1))

;;** Systems

;;*** Emacs
;; via tecosaur (open elp/etrace output in flamegraphs)
;;
;; open in: speedscope https://www.speedscope.app/ or chrome://tracing
(use-package! etrace :after elp)

;;*** Arch
(use-package! aurel :defer t)

;;*** Guix
;;
;; guix.el gets built into the guix profile for doomemacs, so it doesn't need
;; use-package.
;;

(setopt guix-load-path '((expand-file-name ".dotfiles/gh" (getenv "HOME"))))

;; TODO: GUIX: maybe set guix-load-compiled-path
;; see [[file:~/.emacs.doom/.local/straight/repos/emacs-guix/elisp/guix-repl.el::defun guix-repl-guile-args]]

;; TODO: defer guix?
(use-package! guix
  :init (require 'ffap))

;; NOTE: guix-load-path needs to be done outside of use-package! or hook for
;; some reasons -- e.g. guix-repl.el isn't loaded by use-package, where
;; guix-load-path is defined. it looks like it's intended to be managed by
;; GUILE_LOAD_PATH, (setq guix-load-path ...) or in .dir-locals.el

;;**** recutils: manipulate guix cli output
(use-package! rec-mode
  :defer t
  :config
  (add-to-list 'dc/org-babel-load-languages '(rec . t))
  (require 'ob-rec)
  (dc/org-babel-do-load-languages))

;;*** Nix

;;*** Unix

(use-package! elf-mode
  :commands elf-mode
  :magic ("ELF" . 'elf-mode))

(use-package! crontab-mode)
(use-package! syslog-mode
  :custom (syslog-setup-on-load t)
  :mode "\\(messages\\(\\.[0-9]\\)?\\|SYSLOG\\|\\.s?trace\\)\\'")

;; ("\\(messages\\(\\.[0-9]\\)?\\|SYSLOG\\|\\.s?trace\\)\\'" . syslog-mode)

;; TODO: PROCED: setup keybindings
(use-package! proced
  :commands proced
  :init (map! :map dc/quick-map "p" #'proced)
  :config (map! :map proced-mode-map "sh" #'proced-sort-header))

;;*** Linux
(use-package! repology
  :commands repology
  :init (map! :map dc/quick-map "r" #'repology))
(use-package! dts-mode :defer t)
(use-package! archive-rpm :defer t)

;;** Services

;; TODO: DAEMONS.EL: setup keybindings (daemons-systemd-toggle-user)
(use-package! daemons
  :commands daemons
  :custom ((daemons-systemd-is-user t)))

(use-package! systemd :defer t)
(use-package! journalctl-mode
  :commands journalctl
  :config (map! :leader "oj" #'journalctl))


;;*** Network
;; password-store.el? pass.el?
(use-package! terraform-mode
  :defer t
  :custom (terraform-format-on-save t))

;;**** ContainerD

(use-package! docker
  :commands docker
  :config (map! :map dc/quick-map "d" #'docker))

;;**** Terraform/HCL

;;**** ContainerD

;;**** K8S

;;**** SSH

(use-package! ssh-config-mode)

(use-package! x509-mode
  ;; TODO x509-mode-hook: jumping straight to x509 doesn't work well
  ;; :hook (x509-mode-hook . (lambda () (call-interactively 'x509-dwim)))
  :mode (rx "." (| "pem" "cer" "der" "crt" "crl") eos))

;;***** Tramp
;; TODO: TRAMP: check that guix emacs build prepends to tramp-remote-path
(use-package tramp :demand t
  :config
  (require 'tramp-container)
  (cl-dolist (p '("~/.guix-profile/bin"
                  "~/.guix-profile"
                  "/run/current-system/profile/bin"
                  "/run/current-system/profile/sbin"))
    (add-to-list 'tramp-remote-path p))
  :custom (tramp-default-method "ssh"))

;;** Data
;;*** Structure

;;**** XML
(require 'dom)
(use-package! esxml :demand t)

;;**** Protobuf
(use-package! protobuf :defer t)
;;*** Database

;;*** API
(use-package! graphql :after ghub)

;;*** Visualization
;; gnu plot, graphviz/plot, d2, mermaid, plantuml
;;** Misc

;;* Applications

;;* Social

;;** Pastebin

(use-package! 0x0 :defer t :init (require 'dc-0x0))
(after! 0x0
  (setopt 0x0-servers (dc/0x0-retention-policy)))

;;** Open Source
;;
;;*** Debbugs
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

;;* Keymaps

;; TODO: move setup for <f1> <f2> map to after use-package (and after! which-key?)

;;NOTE: some of these may need to be hooked (after! ...)

;;** Global Remaps

;;** Help Map

(map! :map 'help-map

      ;; can insert values with embark
      "M-v" #'getenv

      ;; not interactive, also not sure whether it works
      ;; "C-k" (apply-partially #'embark-bindings-at-point)

      "B" #'embark-bindings
      "M-b" #'embark-bindings-in-keymap
      "M-m" #'consult-minor-mode-menu)

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

;;** Search Map
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

;; FIXME: the hook gets set. the code works, but it doesn't run
(add-hook! 'server-mode-hook
           ;; (when server-mode) ;; still doesn't run
           (alert (format "Loaded Emacs Server\n\nConfig: %s\nSocket: %s" doom-user-dir
                          (expand-file-name server-name server-socket-dir)) :title "Doom:"))

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
