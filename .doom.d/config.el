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

;;** Emacs

;;*** User

(setq-default user-full-name "David Conner"
              user-mail-address (or (getenv "EMAIL") "noreply@te.xel.io"))

;;*** Paths

;; see doom-{emacs,core,data,docs,env,user,etc,cache,modules,...}-dir
;; (defvar dc/emacs-d (expand-file-name "~/.emacs.g/") "TODO: docs.")
;; (defvar dc/emacs-cache (expand-file-name "~/.cache/emacs/") "TODO: docs.")
;; (defvar dc/emacs-dw (expand-file-name "dw" dc/emacs-d) "TODO: docs.")
;; (defvar dc/emacs-doom-modules (expand-file-name "doom/modules" dc/emacs-d))
;; (defvar dc/emacs-modules (expand-file-name "modules" dc/emacs-d) "TODO: docs.")

;; don't accidentally edit doom's straight.el files
(def-project-mode! doom-straight-read-only-mode
  :match (rx-to-string (string-join (list "" (f-base doom-emacs-dir) ".local" "straight" "") "/"))
  :modes '(emacs-lisp-mode)
  :on-enter (setq-local buffer-read-only t))

;;**** Load Path

(add-to-list 'load-path (expand-file-name ".dotfiles/.emacs.d/lisp" (getenv "HOME")))
(require 'dw-settings)

;; TODO no-littering?
;;\\(?:;[^#]\\|\\*+\\)

;;**** Use Package

(setopt use-package-enable-imenu-support t)

;;** UI

(add-hook! 'doom-init-ui-hook
  (progn
    (menu-bar-mode +1)
    (scroll-bar-mode -1)
    (tool-bar-mode -1)))

(setq display-line-numbers-type nil)

;; doom--setq-outline-level-for-emacs-lisp-mode-h
;; doom--setq-outline-regexp-for-emacs-lisp-mode-h
;; doom--setq-tab-width-for-emacs-lisp-mode-h

;;*** Alerts
(require 'notifications)
(use-package! alert
  :demand t
  :custom
  (alert-default-style 'libnotify)
  (alert-log-level 'normal))

;;*** Font

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

;;*** Dired

(setq dired-omit-files "^.DS_Store\\'\\|^.project\\(?:ile\\)?\\'\\|^.\\(svn\\)\\'\\|^.ccls-cache\\'\\|\\(?:\\.js\\)?\\.meta\\'\\|\\.\\(?:elc\\|o\\|pyo\\|swp\\|class\\)\\'")

(dirvish-override-dired-mode -1)

;; TODO: get Doom to; not close all the direds(advice-add #'+doom-)

;;*** Theme
(use-package ef-themes
  :demand t
  :init (setopt doom-theme nil)
  :hook (doom-init-ui-hook . (lambda () (ef-themes-load-random 'dark))))

;;*** Windows
(use-package! ace-window
  :commands ace-window aw-show-dispatch-help
  :config
  (map! :map ctl-x-map "o" #'aw-show-dispatch-help "C-o" #'ace-window))

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

;;*** Tabs Windows

;;*** Completion
;;**** Vertico
;;**** Corfu

(setq corfu-auto-delay 0.5)

;;**** Orderless
;;**** Consult
(use-package! consult-org-roam
  :after (org-roam consult)
  :custom
  (consult-org-roam-grep-func #'consult-ripgrep)
  (consult-org-roam-buffer-narrow-key consult-narrow-key)
  (consult-org-roam-buffer-after-buffers t))

;; TODO: CONF: consult-org-roam: (consult-customize ...) & (consult-org-roam-mode)

;;**** Marginalia
;; regex to prevent things from popping on screen
;; (setq marginalia-censor-variables nil)
;;**** Cape
;;**** Embark


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
      org-startup-indented t)

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

;;* Dev
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

(setq-default lsp-keep-workspace-alive nil)

;; (after! lsp-ui
;;         (setq lsp-ui-doc-enable t))

;;** Lisp

;; fixes issues navigating by lispy outline, but with advice below,
;; +el-outline-regexp no longer overrides lispy-outline-level
(setq-hook! 'emacs-lisp-mode-hook
  +emacs-lisp-outline-regexp ";;\\(?:;[^#]\\|\\*+\\)"
  outline-regexp +emacs-lisp-outline-regexp)

;;*** Emacs Lisp

;;*** Scheme

;; the arei package hooks itself already. .dir-locals.el may need an update.
;;
;; run guile-ares-rs server externally, then connect using sesman-start
(use-package! arei :defer t)

;;*** Lispy
;; lispy-outline: "^[ 	]*;;;\\(;*\\**\\) [^ 	\n]"
;;   doom: "^;;\\(?:;[^#]\\|\\*+\\)"
(use-package! lispy
  :defer t
  :hook (lisp-data-mode . lispy-mode)
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

(use-package flymake-shellcheck
  :commands flymake-shellcheck-load
  :config (add-hook 'sh-mode-hook 'flymake-shellcheck-load))

(use-package vterm :defer t
  :custom
  (vterm-max-scrollback 1000)
  (vterm-set-bold-hightbright t))

;; TODO: per-project vterm
;; https://github.com/doomemacs/doomemacs/blob/master/modules/term/vterm/autoload.el

;;** Compiled

(use-package! zig-mode
  ;; :custom (lsp-zig-zls-executable . "~/.fdsa")
  :defer t)

;;** Langs

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
;;** Forges
;;*** Sourcehut
;;*** Repo


;;* Tools

;;** Auth
;;
;;*** epa
(require 'epg)

;;*** pinentry
(setopt epg-pinentry-mode 'loopback)    ;; cancel/ask
(setopt epg-user-id user-mail-address)
;; (setq epg-debug t)

;;*** auth-source-pass
;; via doom

;;** Docs
;; (external docs: dash/tldr)
;;** Systems

;;*** Emacs
;; via tecosaur (open elp/etrace output in flamegraphs)
;;
;; open in: speedscope https://www.speedscope.app/ or chrome://tracing
(use-package! etrace :after elp)

;;*** Arch
(use-package! aurel :defer t)

;;*** Guix
;;**** recutils: manipulate guix cli output
(use-package! rec-mode
  :defer t
  :config
  (add-to-list 'dc/org-babel-load-languages '(rec . t))
  (require 'ob-rec)
  (dc/org-babel-do-load-languages))

;;*** Nix, ContainerD

(use-package! docker
  :commands docker
  :config (map! "<f1> <f2> d" #'docker))

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
  :config (map! "<f1> <f2> d" #'proced
                :map proced-mode-map
                "sh" #'proced-sort-header))

;;*** Linux
(use-package! repology
  :commands repology
  :config (map! "<f1> <f2> r" #'repology))
(use-package! dts-mode :defer t)
(use-package! archive-rpm :defer t)

;;*** Network
;; password-store.el? pass.el?
(use-package! terraform-mode
  :defer t
  :custom (terraform-format-on-save t))

;;*** SSH

(use-package! ssh-config-mode)

(use-package! x509-mode
  ;; TODO x509-mode-hook: jumping straight to x509 doesn't work well
  ;; :hook (x509-mode-hook . (lambda () (call-interactively 'x509-dwim)))
  :mode (rx "." (| "pem" "cer" "der" "crt" "crl") eos))

;;**** Tramp
;; TODO: TRAMP: check that guix emacs build prepends to tramp-remote-path
(use-package tramp :demand t
  :config (require 'tramp-container)
  :custom (tramp-default-method "ssh"))

;;** Services

;; TODO: DAEMONS.EL: setup keybindings (daemons-systemd-toggle-user)
(use-package! daemons
  :commands daemons
  :custom ((daemons-systemd-is-user t)))

(use-package! systemd :defer t)
(use-package! journalctl-mode
  :commands journalctl
  :config (map! :leader "oj" #'journalctl))

;;** Data
;;*** Structure

;;**** XML
(require 'dom)
(use-package! esxml :demand t)

;;**** Protobuf
(use-package! protobuf :defer t)

;;*** Database
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
                (:prefix-map ("G" . "DEBBUGS")
                             "Gb" #'debbugs-gnu-bugs
                             "Gg" #'debbugs-gnu-guix-search
                             "Gs" #'debbugs-gnu-search
                             "Gp" #'debbugs-gnu-package)))

;;* Keys

;; TODO: move setup for <f1> <f2> map to after use-package (and after! which-key?)

;;** Global Remaps
(global-set-key (kbd "C-<prior>") #'tab-bar-switch-to-tab)
(global-set-key (kbd "C-<next>") #'tab-bar-switch-to-recent-tab)

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
      (:prefix-map ("f" . "FLY")
                   "c" #'consult-flycheck
                   "m" #'consult-flymake)
      "i" #'consult-imenu-multi
      "I" #'consult-imenu-multi ;; duplicate
      "k" #'consult-global-mark
      "m" #'consult-mark
      "o" #'consult-outline
      (:prefix-map ("r" . "ROAM")
                   "r" #'consult-org-roam-search
                   "b" #'consult-org-roam-backlinks
                   "f" #'consult-org-roam-file-find
                   "l" #'consult-org-roam-forward-links))

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
