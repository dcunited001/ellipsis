;;; init.el -*- lexical-binding: t; -*-

(doom! :input
       ;; bidi chinese japanese layout

       :completion
       (vertico +icons)
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
       (vc-gutter +pretty)
       vi-tilde-fringe

       ;; TODO window-select: maybe add it
       ;; TODO vc-gutter: probably remove it

       :editor
       ;; (evil +everywhere) god objed parinfer rotate-text word-wrap
       ;; TODO: fold: probably remove
       file-templates fold (format +onsave) lispy multiple-cursors snippets


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
       ;; ansible ein (pass +auth) prodigy tmux upload
       biblio
       (debugger +lsp)
       direnv
       (docker +lsp)
       editorconfig ;; ... how did i miss this?
       (eval +overlay)
       (lsp +peek)                      ; +peek doesn't do much in the code
       lookup
       (magit +forge)
       make
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
       ;; racket raku rst (scheme +guile) sml solidity swift terra
       (cc +lsp)
       (clojure +lsp)
       data
       emacs-lisp
       (go +lsp)
       (graphql +lsp)
       (haskell +lsp)
       hy
       json
       (java +lsp)
       (javascript +lsp)
       (julia +lsp)
       (latex +cdlatex +latexmk)
       lua
       markdown
       nix
       (org +jupyter +pandoc +pretty +noter)
       plantuml
       (python +lsp) ;; TODO: +poetry +pyright +pyenv
       qt
       rest
       (ruby +lsp) ;; TODO: +rails +chruby +rbenv +rvm
       (rust +lsp)
       (scala +lsp)
       (sh +lsp)

       (web +lsp)
       (yaml +lsp)
       ;; TODO: zig: add if i try it

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
