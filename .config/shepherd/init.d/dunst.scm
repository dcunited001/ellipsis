(use-modules (shepherd service))

(define dunst
  (let* ((service-cmd
         (list "/home/dc/.guix-extra-profiles/desktop/desktop/bin/dunst"))
        (log-time (strftime "%Y-%m-%d-" (gmtime (current-time))))
        (log-file (string-append (mkdtemp "/tmp/dunst-XXXXXX")
                                 "/dunst-"
                                 log-time
                                 (gethostname)
                                 ".log")))

    (service '(dunst)
             #:start (make-forkexec-constructor service-cmd #:log-file log-file)
             #:stop  (make-kill-destructor)
             #:respawn? #f)))

(register-services (list dunst))
