(use-modules (shepherd service)
             ;; (ice-9 pretty-print)
             )

(define xsettingsd
  (let* ((service-cmd
          (list "/home/dc/.guix-extra-profiles/desktop/desktop/bin/xsettingsd"))
         (log-time (strftime "%Y-%m-%d-" (gmtime (current-time))))
         (log-file (string-append (mkdtemp "/tmp/xsettingsd-XXXXXX")
                                  "/xsettingsd-"
                                  log-time
                                  (gethostname)
                                  ".log")))
    ;; (pretty-print log-file)
    (service '(xsettingsd)
             #:start (make-forkexec-constructor service-cmd #:log-file log-file)
             #:stop  (make-kill-destructor)
             #:respawn? #f)))

(register-services (list xsettingsd))
