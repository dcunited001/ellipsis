(use-modules (shepherd service))
(use-modules (ice-9 pretty-print))
;; (define (dc/chemacs-profile-name profile-name)
;;   (string-join (list profile-name (gethostname) (getenv "USER")) "-"))

(pretty-print (getenv "GUIX_EXTRA"))
(pretty-print (string-append "--profile=" (getenv "GUIX_EXTRA") "/emacs-g/emacs-g"))


(define gmacs
  (let* ((profile (string-append "--profile=" (getenv "GUIX_EXTRA") "/emacs-g/emacs-g"))
         (preserve "^EMAIL$")
         (service-cmd
          (list "guix" "shell" "-E" preserve "-p" profile "--" "emacs" "--fg-daemon"))
         (log-time (strftime "%Y-%m-%d-" (gmtime (current-time))))
         (log-file (string-append (mkdtemp "/tmp/emacs-XXXXXX")
                                  "/emacs-g-"
                                  log-time
                                  (gethostname)
                                  ".log")))
    (pretty-print (apply string-append (list "guix" "shell" "-E" preserve "-p" profile "--" "emacs" "--fg-daemon")))
    (pretty-print log-time)
    (pretty-print log-file)

    (service '(gmacs)
             ;; #:documentation "Emacs server running the default chemacs profile via guix shell."
             #:start (make-forkexec-constructor service-cmd #:log-file log-file)
             #:stop (make-kill-destructor)
             #:respawn? #f)))

(register-services (list gmacs))
