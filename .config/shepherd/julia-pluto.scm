;; [[file:../../Bash.org::*Pluto][Pluto:2]]
(use-modules (shepherd support)
             (gnu services)
             (gn services pluto))

(register-services
 (service pluto-service-type
          (pluto-configuration
           (port 9876))))

(action 'shepherd 'daemonize)
(for-each start '(pluto-service))
;; Pluto:2 ends here
