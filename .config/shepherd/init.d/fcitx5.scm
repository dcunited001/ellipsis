(use-modules (shepherd support))

(define fcitx5
  (make <service>
    #:provides '(fcitx5)
    #:docstring "Emacs server running the default chemacs profile"
    #:respawn? #t

    #:start (make-forkexec-constructor
             '("fcitx5" "-D")
             #:log-file (string-append
                         (mkdtemp "/tmp/shepherd-XXXXXX")
                         "/fcitx5-"
                         (strftime "%Y-%m-%d-" (gmtime (current-time)))
                         (gethostname) ".log"))
    #:stop (make-kill-destructor)))
(register-services fcitx5)
