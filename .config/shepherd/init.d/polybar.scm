(use-modules (shepherd service))

(define polybar
  (let* ((service-cmd (list "/home/dc/.guix-extra-profiles/desktop/desktop/bin/polybar"))
         (log-time (strftime "%Y-%m-%d-" (gmtime (current-time))))
         (log-file (string-append (mkdtemp "/tmp/polybar-XXXXXX")
                                  "/polybar-"
                                  log-time
                                  (gethostname) ".log")))

    (service '(polybar)
             #:start (make-forkexec-constructor service-command #:log-file log-file)
             #:stop  (make-kill-destructor)
             #:respawn? #f)))

(register-services (list polybar))

;; shepherd should handle both stdout/stderr
;; polybar panel 2>&1 > /tmp/polybar.1.log
