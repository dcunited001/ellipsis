(use-modules (shepherd support))

(define dunst
  (make <service>
    #:provides '(dunst)
    #:respawn? #f
    #:start (make-forkexec-constructor
             '("/home/dc/.guix-extra-profiles/desktop/desktop/bin/dunst")
             #:log-file (string-append
                         (mkdtemp "/tmp/dunst-XXXXXX")
                         "/dunst-"
                         (strftime "%Y-%m-%d-" (gmtime (current-time)))
                         (gethostname) ".log"))
    ;; TODO: setsid error when using handle-termination
    ;; #:handle-termination (exec-command '("notify-send" "-i error" "Shepherd: Dunst" "Dunst stopped"))
    #:stop  (make-kill-destructor)))
(register-services dunst)

;; shepherd should handle both stdout/stderr
;; dunst 2>&1 > /tmp/dunst.1.log
