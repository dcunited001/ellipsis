(use-modules (shepherd support))

(define xsettingsd
  (make <service>
    #:provides '(xsettingsd)
    #:respawn? #f
    #:start (make-forkexec-constructor
             '("/home/dc/.guix-extra-profiles/desktop/desktop/bin/xsettingsd")
             #:log-file (string-append
                         (mkdtemp "/tmp/xsettings-XXXXXX")
                         "/xsettingsd-"
                         (strftime "%Y-%m-%d-" (gmtime (current-time)))
                         (gethostname) ".log"))
    ;; TODO: setsid error when using handle-termination
    ;; #:handle-termination (exec-command '("notify-send" "-i error" "Shepherd: XSettingsD" "XSettingsD stopped"))
    #:stop  (make-kill-destructor)))
(register-services xsettingsd)
