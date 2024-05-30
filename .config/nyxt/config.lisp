;; -*- mode: common-lisp -*-
;;* Nyxt
(in-package #:nyxt-user)

;; configs

;; https://github.com/shaunsingh/nix-darwin-dotfiles/tree/main/configs/nyxt

;; packages
;; https://github.com/aartaka/nx-dark-reader
;; https://github.com/migalmoreno/nx-router
;; https://github.com/migalmoreno/nx-tailor/blob/master/tailor.lisp

;; emacs
;; https://github.com/ag91/emacs-with-nyxt/blob/master/emacs-with-nyxt.el
;; cl indentation: https://discourse.atlas.engineer/t/emacs-dir-locals-el-tutorial/555

;; discourse
;; https://discourse.atlas.engineer/t/better-hint-selectiors-for-gmail/750



;;** Modules

;;*** nyxt-user

;; (defmacro define-nyxt-user-system[-and-load]
;;   name
;;   &rest args
;;   &key depends-on components &allow-other-keys)

(define-nyxt-user-system-and-load
 nyxt-user/basic-config
 :components ("status"))

;; (define-nyxt-user-system-and-load nyxt-user/basic-config
;;   :components ("keybinds" "passwd" "status" "commands" "hsplit" "style" "unpdf" "objdump" "github"))

;;** Editor

;;*** Theme

;;**** Tailor

;; to use with my config, uncomment the define-configuration below to append the
;; mode to all web-buffers. or run M-x tailor-mode to append mode to bufffer.
;;
;; once mode is set in a buffer, then run M-x load-theme

;; to ensure timer switching works at startup and through the SBCL timers
;;
;; must run before loading the package
(local-time:reread-timezone-repository)
(setf local-time:*default-timezone*
      (local-time:find-timezone-by-location-name "America/New_York"))

;; package will define a mode called tailor
(define-nyxt-user-system-and-load
 nyxt-user/tailor
 :depends-on (nx-tailor)
 ;; configured in nyxt-user:tailor (./tailor.lisp)
 :components ("tailor.lisp"))

;; this will add tailor-mode to all web-buffers
;; (define-configuration web-buffer
;;   ((default-modes `(tailor:tailor-mode ,@%slot-default%))))

;; old theme code: just set dark mode
;; Alter the instance of the browser's theme slot
;; (defmethod customize-instance ((browser browser) &key)
;;            (setf (slot-value browser 'theme)
;;                  theme:+dark-theme+)
;;            (setf (slot-value browser 'external-editor-program)
;;                  ;; '("gmacsclient" "-c")
;;                  '("alacritty --command vim")))

;;** Profiles

;; to start an alternate configuration (this is easier)
;; nyxt --profile nosave --socket /tmp/nyxt-nosave.socket

;; also, I tried a few things to fully separate out an alternate config, but
;; couldn't quite get it to work. it's simpler to use defmethod to dispatch on
;; the profile type to customize each (e.g. custom theme per profile)

;;** Keybindings

;;*** Mouse Bindings

(defvar *dc/mouse-keymap* (make-keymap "mouse-map"))

;; these commands are handy, but it's better to use Nyxt's unique features
;; (hints, etc). also, without beginning a mouse binding with a keystroke, it's
;; unclear how many events are in the current sequence. (hence the button12
;; mapping)
(define-key *dc/mouse-keymap*
            "button13 button1" 'nyxt/mode/document:jump-to-heading

            ;; "button13 button3" 'nyxt/mode/document:headings-panel
            "button12" 'nothing)

(define-mode dc/mouse-mode ()
             "Dummy mode for custom mouse bindings in *dc/mouse-keymap*."
             ((keyscheme-map
               (nkeymaps/core:make-keyscheme-map
                nyxt/keyscheme:emacs *dc/mouse-keymap*))))

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

;; Move C-space
(define-configuration
 buffer
 ((override-map
   (let ((map (make-keymap "override-map")))
     (define-key map "f1 f5" 'execute-command "C-space" 'nothing)))))

;;** Defaults

(define-configuration

 (:modable-buffer :prompt-buffer :editor-buffer)
 "Sets emacs-mode and mouse/noob bindings everywhere."
 ((default-modes `(:emacs-mode :dc/noob-mode ,@%slot-value%))))

;; :keywords are handled differently than 'symbols. (also delineate abc:keywords
;; and abc::keywords)
;; https://stackoverflow.com/questions/8567155/why-colons-precede-variables-in-common-lisp

;;** Bookmarks

;; see bookmarks.lisp

(defmethod files:resolve ((profile nyxt:nyxt-profile) (file nyxt/mode/bookmark:bookmarks-file))
           "Reroute the bookmarks to the config directory."
           #p"~/.config/nyxt/bookmarks.lisp")

;;** Search

;; see search-engines.lisp
