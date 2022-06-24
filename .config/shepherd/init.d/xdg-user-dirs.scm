(use-modules (shepherd support))

(define xdg-user-dirs
  (make <service>
    #:provides '(xdg-user-dirs)
    #:one-shot? #t
    #:start (make-forkexec-constructor '("xdg-user-dirs-update"))
    #:stop (make-kill-destructor)))
(register-services xdg-user-dirs)
