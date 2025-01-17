;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

;;* Interface

;;** Search

;;** Buffers
(package! bufler)

;;** Minibuffer

;;** Look and Feel
;; https://github.com/kickingvegas/casual-suite
(package! casual-suite)
;; (package! casual-avy)
;; (pacakge! casual-symbol-overlay)
(package! ef-themes)
;; pomm
;; pulsar

;;** Editor
;; visual-fill-column?

;;** Highlighting
;; highlight-symbol?

;;** Bookmarks
(package! burly)
(package! activities)

;;** UI
(package! buffer-move)
(package! tmr)
(package! alert)

;;*** UI Components
;; tabspaces
;;*** Completion
;; consult-git-log-grep?
;;
;; doom includes consult's core + dir,lsp,flycheck,yasnippet

;; later?
;;
;; consult-eglot?
;; consult-flyspell?
;; consult-recoll?
(package! consult-org-roam)

;;** Docs
(package! tldr)

;;* Systems

;;** Unix

(package! crontab-mode)
(package! x509-mode)
(package! elf-mode)
(package! syslog-mode :recipe (:type git :flavor melpa :host github
                               :repo "dcunited001/syslog-mode"
                               :branch "dcunited001-read-only"))
;;** Emacs

(package! etrace
  :recipe (:host github :repo "aspiers/etrace"))
;; :pin "2291ccf2f2ccc80a6aac4664e8ede736ceb672b7"

;;** Linux

(package! aurel)
(package! repology)
(package! archive-rpm)
(package! dts-mode)

;;** Services
(package! journalctl-mode)
(package! systemd-mode)
(package! daemons)

;;** Guix

;;* VCS
;;** Git
(package! git-link)

;;** Magit
(package! magit-todos)
(package! magit-tbdiff)

;;* Programming

;;** Shell
(package! flymake-shellcheck)

;;** LSP

;;** Web

(package! html-ts-mode :recipe (:host github :repo "mickeynp/html-ts-mode"))
(package! astro-ts-mode :recipe (:host github :repo "Sorixelle/astro-ts-mode"))

;;*** Tailwind
;; TODO: add web packages (tailwind, liquid, etc)

;; https://github.com/merrickluo/lsp-tailwindcss
(package! lsp-tailwindcss :recipe (:host github :repo "merrickluo/lsp-tailwindcss"))

;; (use-package lsp-tailwindcss
;;   :init
;;   (setq lsp-tailwindcss-add-on-mode t))
;; (add-hook 'before-save-hook 'lsp-tailwindcss-rustywind-before-save)

;;** Lisp

;;*** Common Lisp

;;*** Scheme

;;**** Geiser

;;**** Arei
;; packages: queue, sesman, arei
(package! queue)
(package! arei :recipe (:type git :repo "https://git.sr.ht/~abcdw/emacs-arei"))

;;*** Clojure

;;** Lang

;;*** Rust
;; TODO: PKG cargo-transient (dep on project.el)
;;*** C++
;;**** CMake
;; TODO: PKG project-cmake (dep on project.el)
;;**** QML
;;*** Go

;;*** Python

;;*** Scala
;;*** TCL
;; TODO: PKG tcl-ts-mode? (local)


;;* Org

;; (package! org-modern)
(package! org-super-agenda)
(package! ob-nix)
(package! ob-graphql)


;;* Tools

;;** Auth
;; (package! auth-source-pass) ; already in doom
;; (package! oauth2)

;;** Crypto

(package! ssh-config-mode)

;;** Data

;;*** Structure

(package! protobuf-mode)
(package! esxml)

;;*** Database

;;*** API

;;**** Restclient

;; doom: restclient.el
;;
;; this seems to include restclient-jq, whereas my config used the
;; restclient.jq from restclient.el

;;**** OpenAPI
;; (package! swagg)

;;**** GraphQL
;; doom: graphql-mode

;;*** Visualization

;; doom: graphviz-dot-mode

;;**** D2
(package! d2-mode)

;;**** Mermaid
(package! mermaid-mode)
(package! ob-mermaid)

;;**** Smiles
(package! smiles-mode)
(package! ob-smiles)

;;** Writing

;;*** Kanji
;; TODO: (package! kanji-mode)

;;*** Translate
(package! google-translate)
(package! ob-translate)

;; * Social

(package! 0x0 :recipe (:host sourcehut :repo "willvaughn/emacs-0x0"))

;;** Open Source
(package! debbugs)

;; To install SOME-PACKAGE from MELPA, ELPA or emacsmirror:
;; (package! some-package)

;; To install a package directly from a remote git repo, you must specify a
;; `:recipe'. You'll find documentation on what `:recipe' accepts here:
;; https://github.com/radian-software/straight.el#the-recipe-format
;; (package! another-package
;;   :recipe (:host github :repo "username/repo"))

;; If the package you are trying to install does not contain a PACKAGENAME.el
;; file, or is located in a subdirectory of the repo, you'll need to specify
;; `:files' in the `:recipe':
;; (package! this-package
;;   :recipe (:host github :repo "username/repo"
;;            :files ("some-file.el" "src/lisp/*.el")))

;; If you'd like to disable a package included with Doom, you can do so here
;; with the `:disable' property:
;; (package! builtin-package :disable t)

;; You can override the recipe of a built in package without having to specify
;; all the properties for `:recipe'. These will inherit the rest of its recipe
;; from Doom or MELPA/ELPA/Emacsmirror:
;; (package! builtin-package :recipe (:nonrecursive t))
;; (package! builtin-package-2 :recipe (:repo "myfork/package"))

;; Specify a `:branch' to install a package from a particular branch or tag.
;; This is required for some packages whose default branch isn't 'master' (which
;; our package manager can't deal with; see radian-software/straight.el#279)
;; (package! builtin-package :recipe (:branch "develop"))

;; Use `:pin' to specify a particular commit to install.
;; (package! builtin-package :pin "1a2b3c4d5e")


;; Doom's packages are pinned to a specific commit and updated from release to
;; release. The `unpin!' macro allows you to unpin single packages...
;; (unpin! pinned-package)
;; ...or multiple packages
;; (unpin! pinned-package another-pinned-package)
;; ...Or *all* packages (NOT RECOMMENDED; will likely break things)
;; (unpin! t)
