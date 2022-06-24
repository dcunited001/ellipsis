(use-modules (shepherd support))

(define xdg-desktop-portal-gtk
  (make <service>
    #:provides '(xdg-desktop-portal-gtk)
    #:respawn? #f
    #:start (make-forkexec-constructor
             '("/home/dc/.guix-extra-profiles/xdg/xdg/libexec/xdg-desktop-portal-gtk")
             #:log-file (string-append
                         (mkdtemp "/tmp/xdg-XXXXXX")
                         "/xdg-desktop-portal-gtk-"
                         (strftime "%Y-%m-%d-" (gmtime (current-time)))
                         (gethostname) ".log"))
    ;; TODO: setsid error when using handle-termination
    ;; #:handle-termination (exec-command '("notify-send" "Shepherd: XDG" "XDG Desktop Portal GTK stopped"))
    #:stop  (make-kill-destructor)))
(register-services xdg-desktop-portal-gtk)
