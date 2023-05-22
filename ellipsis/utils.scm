(define-module (ellipsis utils)
  #:use-module (guix gexp)

  #:use-module (gnu packages)
  #:use-module (gnu services)
  #:use-module (gnu services configuration)

  #:use-module (ice-9 pretty-print)

  #:export (util)

  ;; TODO: look into #:replace
  )

(define util "util")

;; (define-public (remove-pulseaudio-service services-list)
;;   (remove (lambda (service)
;;             (eq? (service-kind service) pulseaudio-service-type))
;;           services-list))

;; (define-public (remove-gdm-service services-list)
;;   (remove (lambda (service)
;;             (eq? (service-kind service) gdm-service-type))
;;           services-list))
