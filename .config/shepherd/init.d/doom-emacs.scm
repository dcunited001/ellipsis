(use-modules (shepherd support))
;; (define (dc/chemacs-profile-name profile-name)
;;   (string-join (list profile-name (gethostname) (getenv "USER")) "-"))

(define doom-emacs
  (make <service>
    #:provides '(doom-emacs)
    #:docstring "Emacs server running the Doom chemacs profile"
    #:respawn? #t

    #:start (make-forkexec-constructor
             '("emacs" "--fg-daemon" "--with-profile" "doom")
             #:log-file (string-append
                         (mkdtemp "/tmp/emacs-XXXXXX")
                         "/doom-emacs-"
                         (strftime "%Y-%m-%d-" (gmtime (current-time)))
                         (gethostname) ".log"))
    ;; #:handle-termination (exec-command
    ;;                      '("notify-send"
    ;;                        "Shepherd: Emacs"
    ;;                       "Doom Emacs has terminated."))
    #:stop (make-kill-destructor)))
(register-services doom-emacs)
