;; TODO: gpg-agent: get make-system-de/constructor working
(define gpg-agent-debug-pinentry
  (let* ((gpg-path "/home/dc/.guix-extra-profiles/desktop/desktop/bin/")
         (gpg-agent (string-append gpg-path "start-gpg-debug-pinentry"))
         (gpgconf (string-append gpg-path "gpgconf")))
    
    (service '(gpg-agent-debug-pinentry)
             #:start (make-system-constructor gpg-agent)
             #:stop (make-system-destructor
                     (string-join (list gpgconf "--kill" "gpg-agent") " "))
             #:respawn? #f)))


(register-services (list gpg-agent-debug-pinentry))
