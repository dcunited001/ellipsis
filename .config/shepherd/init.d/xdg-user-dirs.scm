(use-modules (shepherd service))

(define xdg-user-dirs
  (service '(xdg-user-dirs)
           #:start (make-forkexec-constructor '("xdg-user-dirs-update"))
           #:stop (make-kill-destructor)
           #:one-shot? #t))

(register-services (list xdg-user-dirs))
