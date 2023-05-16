(use-modules (shepherd service))

(define xdg-desktop-portal-gtk
  (let* ((service-cmd
         (list "/home/dc/.guix-extra-profiles/xdg/xdg/libexec/xdg-desktop-portal-gtk" "-r"))
        (log-time (strftime "%Y-%m-%d-" (gmtime (current-time))))
        (log-file (string-append (mkdtemp "/tmp/xdg-desktop-portal-XXXXXX")
                                 "/portal-gtk-"
                                 log-time
                                 (gethostname)
                                 ".log")))

    (service '(xdg-desktop-portal-gtk)
             #:start (make-forkexec-constructor service-cmd #:log-file log-file)
             #:stop  (make-kill-destructor)
             #:respawn? #f)))

(register-services (list xdg-desktop-portal-gtk))
