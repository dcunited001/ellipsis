;; -*- mode: common-lisp -*-

(in-package #:nyxt-user)

;;** Keybindings

;;*** Noob Bindings

;; a list of bindings to focus on learning
(defvar *dc/noob-keymap* (make-keymap "noob-map"))

(define-key *dc/noob-keymap*
            "f1 m" 'nyxt/mode/message:list-messages
            "f1 f2 b" 'nyxt/mode/history:buffer-history-tree
            "f1 f2 B" 'nyxt/mode/buffer-listing:buffers-panel
            "f1 f2 h" 'nyxt/mode/history:history-tree
            "f1 f2 ." 'nyxt/mode/document:headings-panel
            "f1 f2 w" 'nyxt/mode/watch:watch-mode
            "f1 f2 p" 'nyxt/mode/preview:preview-mode
            "f1 f2 k" 'delete-current-buffer

            ;; only possible from macro editor
            ;; "f1 f2 M" 'nyxt/mode/macro-edit/save-macro
            "f1 f2 M" 'nyxt/mode/macro-edit:edit-macro)

(define-mode dc/noob-mode ()
             "Dummy mode for custom noob bindings in *dc/noob-keymap*."
             ((keyscheme-map
               (nkeymaps/core:make-keyscheme-map
                nyxt/keyscheme:emacs *dc/noob-keymap*))))
