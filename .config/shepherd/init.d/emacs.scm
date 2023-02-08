(use-modules (shepherd support))
;; (define (dc/chemacs-profile-name profile-name)
;;   (string-join (list profile-name (gethostname) (getenv "USER")) "-"))

(define emacs
  (make <service>
    #:provides '(emacs)
    #:docstring "Emacs server running the default chemacs profile"
    #:respawn? #t

    #:start (make-forkexec-constructor
             '("guix" "shell" "--manifest=$HOME/.config/guix/manifests/emacs-g.scm" "--"
               "emacs" "--fg-daemon")
             #:log-file (string-append
                         (mkdtemp "/tmp/emacs-XXXXXX")
                         "/emacs-g-"
                         (strftime "%Y-%m-%d-" (gmtime (current-time)))
                         (gethostname) ".log"))
    #:stop (make-kill-destructor)))
(register-services emacs)
