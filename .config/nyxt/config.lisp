;; -*- mode: common-lisp -*-
;;* Nyxt
(in-package #:nyxt-user)

;;** Modules

;;*** nyxt-user

;; (defmacro define-nyxt-user-system[-and-load]  name
;;   &rest args
;;   &key depends-on components &allow-other-keys)

(define-nyxt-user-system-and-load nyxt-user/basic-config
  :components ("status" "search-engines" "keys"))
;; (reset-asdf-registries)
;; (define-nyxt-user-system-and-load nyxt-user/invader-proxy
;;     :depends-on ("invader"))

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

;; ISSUE: ASDF won't compile compile this or any other modules

;; CAUSE: the SBCL compiling this wasn't the SBCL that Guix got Nyxt got
;; compiled with. A Guix profile needs to propagate SBCL (or something) and
;; some environment constraints on CL/SBCL/ASDF are needed

;; TODO: still not loading

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
  ((default-modes `(:dc/noob-mode :emacs-mode ,@%slot-value%))))


;;*** Default Modes

;;  reduce-tracking-mode
;;  no-procrastinate-mode
;;  force-https-mode
;;  emacs-mode
;;  bookmark-frequent-visits-mode
;;  certificate-exception-mode
;;  autofill-mode
;;  spell-check-mode
;;  search-buffer-mode
;;  hint-mode
;;  document-mode
;;  password-mode
;;  bookmark-mode
;;  annotate-mode
;;  history-mode

;;*** UI

;; reading-line-mode
;; + small-web-mode :: for gopher/gemini (a major mode, i think)
;; + style-mode :: 

;;*** Nyxt UI

;; repeat-mode

;;*** Policy

;; no-sound-mode
;; no-webgl-mode

;;*** Automation

;; + bookmark-frequent-visits-mode
;; + bookmarklets
;; + user-script-mode

;;*** Systems & Security

;; + watch-mode :: refresh every N minutes
;; + proxy-mode :: analyze/intercept traffic (for non-malicious purposes)
;; + remembrance-mode
;; + process-mode :: display formatted content from system commands
;; + process-mode + tts-mode :: alerts

;;*** Programming

;; preview-mode

;;*** Common Lisp

;; macro-edit-mode
;; editor-mode
;; repl-mode

;; history-migration-mode
;; (does this import or append another history source)

;;*** Interactive

;; expedition-mode
