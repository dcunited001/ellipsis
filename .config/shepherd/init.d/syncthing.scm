(use-modules (shepherd support))
(define syncthing
  (make <service>
    #:provides '(syncthing)
    #:docstring "Run `syncthing' without calling the browser"
    #:respawn? #f
    #:start (make-forkexec-constructor
             '("syncthing" "-no-browser"))
    #:stop (make-kill-destructor)))
(register-services syncthing)
