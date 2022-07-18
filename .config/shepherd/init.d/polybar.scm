(use-modules (shepherd support))

(define polybar
  (make <service>
    #:provides '(polybar)
    #:respawn? #f
    #:start (make-forkexec-constructor
             '("/home/dc/.guix-extra-profiles/desktop/desktop/bin/polybar")
             #:log-file (string-append
                         (mkdtemp "/tmp/polybar-XXXXXX")
                         "/polybar-"
                         (strftime "%Y-%m-%d-" (gmtime (current-time)))
                         (gethostname) ".log"))
    ;; TODO: setsid error when using handle-termination
    ;; #:handle-termination (exec-command '("notify-send" "-i error" "Shepherd: Polybar" "Polybar stopped"))
    #:stop  (make-kill-destructor)))
(register-services polybar)

;; shepherd should handle both stdout/stderr
;; polybar panel 2>&1 > /tmp/polybar.1.log
