(use-modules (shepherd support))

;; TODO: setsid error when using handle-termination
;; #:handle-termination (exec-command '("notify-send" "-i error" "Shepherd: Pipewire-Pulse" "Pipewire-Pulse stopped")

(define pipewire-pulse
  (make <service>
    #:provides '(pipewire-pulse)
    #:respawn? #f
    #:stop (make-kill-destructor)
    #:start (make-forkexec-constructor
             (list (string-append (getenv "GUIX_EXTRA")
                                "/pipewire/pipewire/bin/pipewire-pulse"))
             #:log-file (string-append
                         (mkdtemp "/tmp/pipewire-pulse-XXXXXX")
                         "/pipewire-pulse-"
                         (strftime "%Y-%m-%d-" (gmtime (current-time)))
                         (gethostname) ".log")
             #:environment-variables
             (append (list "DISABLE_RTKIT=1")
                     (default-environment-variables)))))

(register-services pipewire-pulse)
