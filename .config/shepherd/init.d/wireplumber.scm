(use-modules (shepherd support))

;; TODO: setsid error when using handle-termination
;; #:handle-termination (exec-command '("notify-send" "-i error" "Shepherd: Wireplumber" "Wireplumber stopped")

(define wireplumber
  (make <service>
    #:provides '(wireplumber)
    #:respawn? #f
    #:stop (make-kill-destructor)
    #:start (make-forkexec-constructor
             (list (string-append (getenv "GUIX_EXTRA")
                                  "/pipewire/pipewire/bin/wireplumber"))
             #:log-file (string-append
                         (mkdtemp "/tmp/wireplumber-XXXXXX")
                         "/wireplumber-"
                         (strftime "%Y-%m-%d-" (gmtime (current-time)))
                         (gethostname) ".log")
             #:environment-variables
             (append (list "DISABLE_RTKIT=1")
                     (default-environment-variables)))))

(register-services wireplumber)
