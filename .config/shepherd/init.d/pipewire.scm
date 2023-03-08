(use-modules (shepherd support))

;; TODO: setsid error when using handle-termination
;; #:handle-termination (exec-command '("notify-send" "-i error" "Shepherd: Pipewire" "Pipewire stopped")

(define pipewire
  (make <service>
    #:provides '(pipewire)
    #:respawn? #f
    #:stop (make-kill-destructor)
    #:start (make-forkexec-constructor
             (list (string-append (getenv "GUIX_EXTRA")
                                "/pipewire/pipewire/bin/pipewire"))
             #:log-file (string-append
                         (mkdtemp "/tmp/pipewire-XXXXXX")
                         "/pipewire-"
                         (strftime "%Y-%m-%d-" (gmtime (current-time)))
                         (gethostname) ".log")
             #:environment-variables
             (append (list "DISABLE_RTKIT=1")
                     (default-environment-variables)))))

(register-services pipewire)
