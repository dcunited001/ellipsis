;; https://git.sr.ht/~akagi/guixrc
(define-module (dc home services notification)
  #:use-module (gnu services)
  #:use-module (gnu services configuration)
  #:use-module (gnu home services)
  #:use-module (gnu home services shepherd)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix import utils)
  #:use-module (rde serializers ini)
  ;; #:use-module (gnu packages dunst) ;; not a module
  #:export (home-dunst-configuration
            home-dunst-service-type))

(define-configuration/no-serialization home-dunst-configuration
  (package
    (package dunst)
    "Dunst package to use")
  (config
   (ini-config '())
   ""))

(define (add-dunst-configuration config)
  (let ((cfg (home-dunst-configuration-config config)))
    `(("dunst/dunstrc"
       ,(apply mixed-text-file
               "config"
               (serialize-ini-config
                cfg
                #:format-ini-section (lambda (s) (format #f "[~a]\n" s))))))))


(define add-dunst-package
  (compose list home-dunst-configuration-package))

(define home-dunst-service-type
  (service-type
   (name 'home-dunst)
   (extensions
    (list (service-extension
           home-xdg-configuration-files-service-type
           add-dunst-configuration)
          ;; (service-extension
          ;;  home-environment-variables-service-type
          ;;  home-dunst-environment-variables-service)
          ;; (service-extension
          ;;  home-shepherd-service-type
          ;;  home-dunst-shepherd-service)
          (service-extension
           home-profile-service-type
           add-dunst-package)))
   (compose identity)
   (default-value (home-dunst-configuration))
   (description "Configure the Dunst notification daemon")))
