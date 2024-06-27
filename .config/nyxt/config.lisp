;; -*- mode: common-lisp -*-
;;* Nyxt
(in-package #:nyxt-user)

;;** Modules

;;*** nyxt-user

;; (defmacro define-nyxt-user-system[-and-load]  name
;;   &rest args
;;   &key depends-on components &allow-other-keys)

(define-nyxt-user-system-and-load nyxt-user/basic-config
  :components ("status" "search-engines"))

;;** Profiles

;; to start an alternate configuration (this is easier)
;; nyxt --profile nosave --socket /tmp/nyxt-nosave.socket

;; also, I tried a few things to fully separate out an alternate config, but
;; couldn't quite get it to work. it's simpler to use defmethod to dispatch on
;; the profile type to customize each (e.g. custom theme per profile)

;;** Browser

(define-configuration browser
  ((restore-session-on-startup-p :never-restore)))

;;** UI

;;*** Prompt

;; when results only present a single source, hide it
(define-configuration prompt-buffer
  ((hide-single-source-header-p t)))

;;*** Editor

;; (define-configuration browser
;;   ((external-editor-program (list "terminator" "-x" "vim"))))

;; otherwise access via a handle on a (browser browser)
;;
;; (defmethod-instalce
;; (setf (slot-value browser 'external-editor-program)
;;       '("alacritty --command vim"))

;;*** Theme

;;**** Invader

;; ASDF won't compile compile this or any other modules

;; (define-nyxt-user-system-and-load "nyxt-user/invader-proxy"
;;   :depends-on ("invader"))

;;** Keys

(define-configuration input-buffer
  ((override-map
    (let ((map (make-keymap "override-map")))
      (define-key map "f1 f5" 'execute-command "C-space" 'nothing)))))

;;** Bookmarks

;; see bookmarks.lisp

(defmethod files:resolve ((profile nyxt:nyxt-profile) (file nyxt/mode/bookmark:bookmarks-file))
  "Reroute the bookmarks to the config directory."
  #p"~/.config/nyxt/bookmarks.lisp")

;;** Search

;; see search-engines.lisp


;;** Defaults

(define-configuration buffer
  ;; (:web-buffer :prompt-buffer :editor-buffer)
  "Sets emacs-mode and mouse/noob bindings everywhere."
  ;; ((default-modes `(:emacs-mode :dc/noob-mode ,@%slot-value%)))
  ((default-modes `(:emacs-mode ,@%slot-value%))))
