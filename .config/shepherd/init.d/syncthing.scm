(use-modules (shepherd service))

(define syncthing
  (let* ((service-cmd '("syncthing" "-no-browser")))
    (service '(syncthing)
             #:documentation "Run `syncthing' without calling the browser"
             #:start (make-forkexec-constructor service-cmd)
             #:stop (make-kill-destructor)
             #:respawn? #f)))

(register-services syncthing)
