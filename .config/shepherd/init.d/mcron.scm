(use-modules (shepherd service))

(define mcron
  (service '(mcron)
    #:start (make-forkexec-constructor '("mcron"))
    #:stop  (make-kill-destructor)
    #:respawn? #t))

(register-services (list mcron))
