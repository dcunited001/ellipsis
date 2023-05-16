(use-modules (shepherd service))

(define xdg-desktop-portal
  (let* ((service-cmd
         (list "/home/dc/.guix-extra-profiles/xdg/xdg/libexec/xdg-desktop-portal"))
        (log-time (strftime "%Y-%m-%d-" (gmtime (current-time))))
        (log-file (string-append (mkdtemp "/tmp/xdg-desktop-portal-XXXXXX")
                                 "portal-"
                                 log-time
                                 (gethostname)
                                 ".log")))

    (service '(xdg-desktop-portal)
             #:start (make-forkexec-constructor service-cmd #:log-file log-file)
             #:stop  (make-kill-destructor)
             #:respawn? #f)))

(register-services (list xdg-desktop-portal))
