;(use-modules (shepherd service))

;; (define gpg-agent
;;   (let* ((service-constr '("gpg-connect-agent" "/bye"))
;;          (service-deconstr '("gpgconf" "--kill" "gpg-agent")))
;;     (service '(gpg-agent)
;;              #:start (make-system-constructor service-constr)
;;              #:stop (make-system-deconstructor service-deconstr)
;;              #:respawn? #f)))

(define gpg-agent
  (let* ((gpg-path "/home/dc/.guix-extra-profiles/gpg-agent/gpg-agent/bin/")
         (gpg-connect-agent (string-append gpg-path "gpg-connect-agent"))
         (gpgconf (string-append gpg-path "gpgconf")))
    
    (service '(gpg-agent)
             #:start (make-system-constructor
                      (string-join (list gpg-connect-agent "/bye") " "))
             #:stop (make-system-deconstructor
                     (string-join (list gpgconf "--kill" "gpg-agent") " "))
             #:respawn? #f)))


(register-services (list gpg-agent))
