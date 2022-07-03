(use-modules (shepherd support))

(define fcitx5
  (make <service>
    #:provides '(fcitx5)
    #:docstring "FCITX5 input method daemon."
    #:respawn? #t

    #:start (make-forkexec-constructor
             ;; '("fcitx5" "-D" "--enable anthy")
             '("fcitx5" "-D")
             #:log-file (string-append
                         (mkdtemp "/tmp/shepherd-XXXXXX")
                         "/fcitx5-"
                         (strftime "%Y-%m-%d-" (gmtime (current-time)))
                         (gethostname) ".log"))
    #:stop (make-kill-destructor)))
(register-services fcitx5)
