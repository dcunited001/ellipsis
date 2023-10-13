(use-modules (shepherd service))

;; (profile (string-append "--profile="
;;                         (getenv "GUIX_EXTRA")
;;                         "/emacs-g/emacs-g"))
         
;; doesn't work. needed to create a script in ~/.bin
;; (stop-cmd
;;  (list "guix" "shell" profile "--"
;;        "emacsclient" "-e" "'(kill-emacs)'"))         

(define gmacs
  (let* ((start-cmd (list "gmacs" "--" "--fg-daemon"))
         (stop-cmd (list "gmacskill"))
         (log-time (strftime "%Y-%m-%d-" (gmtime (current-time))))
         (log-file (string-append (mkdtemp "/tmp/emacs-XXXXXX")
                                  "/emacs-g-"
                                  log-time
                                  (gethostname)
                                  ".log")))
    (service '(gmacs)
             #:start (make-forkexec-constructor start-cmd #:log-file log-file)
             #:stop (make-forkexec-constructor stop-cmd)
             #:respawn? #f)))

(register-services (list gmacs))
