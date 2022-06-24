(use-modules (shepherd support))

(define doom-emacs-debug
  (make <service>
    #:provides '(doom-emacs-debug)
    #:docstring "Debug emacs server init"

    #:start (make-forkexec-constructor
             '("emacs" "--fg-daemon" "--debug-init")
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
(register-services doom-emacs-debug)
