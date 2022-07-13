(in-package #:nyxt-user)

#+nyxt-3 (reset-asdf-registries)

(defvar *web-buffer-modes*
  '(nyxt/emacs-mode:emacs-mode
    nyxt/auto-mode:auto-mode
    nyxt/blocker-mode:blocker-mode
    nyxt/force-https-mode:force-https-mode
    nyxt/reduce-tracking-mode:reduce-tracking-mode
    ;; nyxt/user-script-mode:user-script-mode
    )
  "The modes to enable in web-buffer by default.
Extension files (like dark-reader.lisp) are to append to this list.
Why the variable? Because one can only set `default-modes' once, so I
need to dynamically construct a list of modes and configure the slot
only after it's done.")

;; #+nyxt-3
;; (define-nyxt-user-system-and-load nyxt-user/basic-config
;;   :components ("keybinds" "passwd" "status" "commands" "style"))

;; #+nyxt-2
;; (dolist (file (list
;;                (nyxt-init-file "keybinds.lisp")
;;                (nyxt-init-file "passwd.lisp")
;;                (nyxt-init-file "status.lisp")
;;                (nyxt-init-file "commands.lisp")))
;;   (load file))
;; (define-configuration status-buffer ((height 40)))

;; -F1 s to
(define-configuration buffer
    ((default-modes (append '(nyxt::emacs-mode)
                            %slot-default%))
     ;; (current-zoom-ratio 0.75)
     ))


;; browser inherits from buffer
(define-configuration browser
    ((session-restore-prompt :never-restore))
  )
