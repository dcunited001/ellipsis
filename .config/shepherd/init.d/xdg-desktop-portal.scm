(use-modules (shepherd support))

(define xdg-desktop-portal
  (make <service>
    #:provides '(xdg-desktop-portal)
    #:respawn? #f
    #:start (make-forkexec-constructor
             '("/home/dc/.guix-extra-profiles/xdg/xdg/libexec/xdg-desktop-portal")
             #:log-file (string-append
                         (mkdtemp "/tmp/xdg-XXXXXX")
                         "/xdg-desktop-portal-"
                         (strftime "%Y-%m-%d-" (gmtime (current-time)))
                         (gethostname) ".log"))
    ;; TODO: setsid error when using handle-termination
    ;; #:handle-termination (exec-command '("notify-send" "Shepherd: XDG" "XDG Desktop Portal stopped"))
    #:stop  (make-kill-destructor)))
(register-services xdg-desktop-portal)
