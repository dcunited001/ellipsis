;;; init.el -*- lexical-binding: t; -*-

(doom! :input
       ;; bidi chinese japanese layout

       :completion
       ;; (company +childframe)
       (vertico +icons)
       corfu
       ;; company helm ido ivy

       :ui
       ;; deft doom-quit indent-guides ligatures nav-flash neotree tabs
       ;; treemacs unicode workspaces zen
       doom
       doom-dashboard
       (emoji +unicode)
       hl-todo
       hydra
       modeline
       ophints
       (popup +defaults)
       ;; (vc-gutter +pretty)
       vi-tilde-fringe

       window-select
       ;; TODO vc-gutter: probably remove it

       :editor
       ;; (evil +everywhere) god objed parinfer rotate-text word-wrap
       ;; TODO: fold: probably remove
       file-templates fold lispy multiple-cursors snippets
       (format +onsave +lsp)

       :emacs
       ;; ibuffer
       (dired +icons) electric (undo +tree) vc

       :term
       ;; eshell shell term
       vterm

       :checkers
       ;; (spell +flyspell) grammar
       syntax

       :tools
       ;; ansible ein prodigy tmux upload
       biblio
       (debugger +lsp)
       direnv
       (docker +lsp)
       editorconfig ;; ... how did i miss this?
       (eval +overlay)

       ;; TODO implement (unless (getenv "DOOMLSP") +eglot)
       
       (lsp +peek)                      ; +peek doesn't do much in the code
       (lookup +docsets)
       (magit +forge)
       make
       (pass +auth)
       pdf
       rgb
       taskrunner
       terraform
       tree-sitter
       ;; TODO: collab: this is a new module

       :os
       ;; TODO: tty?

       :lang
       ;; agda beancount common-lisp coq crystal csharp (dart +flutter) dhall
       ;; elixir elm erlang ess factor faust fortran fsharp fstar gdscript
       ;; idris kotlin lean ledger nim ocaml php purescript
       ;; racket raku rst sml solidity swift terra
       (cc +lsp)
       (clojure +lsp +tree-sitter)
       common-lisp
       data
       emacs-lisp
       (go +lsp)
       (graphql +lsp)
       (graphviz)
       (haskell +lsp)
       hy
       json
       (java +lsp)
       (javascript +lsp)
       (julia +lsp)
       (latex +cdlatex +latexmk)
       lua ;; TODO: (lua +lsp)
       markdown
       (nix +tree-sitter)
       (org +jupyter +pandoc +pretty +noter +gnuplot +present +roam2) ; +crypt?
       plantuml
       (python +lsp +tree-sitter) ;; TODO: +poetry +pyright +pyenv
       qt
       (rest +jq)
       (ruby +lsp) ;; TODO: +rails +chruby +rbenv +rvm
       (rust +lsp)
       (scala +lsp)
       (scheme +guile +racket)
       (sh +lsp)

       (web +lsp +tree-sitter)
       (yaml +lsp)
       (zig +lsp +treesitter)

       :email
       ;; (mu4e +org +gmail) notmuch (wanderlust +gmail)

       :app
       ;; calendar emms everywhere irc (rss +org) twitter

       :config
       ;; literate
       (default +bindings +smartparens))

;; NOTE Press 'SPC h d h' (or 'C-h d h' for non-vim users) to access Doom's
;;      documentation. There you'll find a link to Doom's Module Index where all
;;      of our modules are listed, including what flags they support.

;; NOTE Move your cursor over a module's name (or its flags) and press 'K' (or
;;      'C-c c k' for non-vim users) to view its documentation. This works on
;;      flags as well (those symbols that start with a plus).
;;
;;      Alternatively, press 'gd' (or 'C-c c d') on a module to browse its
;;      directory (for easy access to its source code).
