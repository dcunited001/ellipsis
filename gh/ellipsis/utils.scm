(define-module (ellipsis utils)
  #:use-module (guix gexp)

  #:use-module (json)

  ;; #:use-module (ice-9 pretty-print)
  #:use-module (srfi srfi-1)

  #:export (read-json-config))

;; TODO: look into #:replace

(define (read-json-config file)
  (call-with-input-file file
    json->scm))

;; (define-public (remove-pulseaudio-service services-list)
;;   (remove (lambda (service)
;;             (eq? (service-kind service) pulseaudio-service-type))
;;           services-list))

;; (define-public (remove-gdm-service services-list)
;;   (remove (lambda (service)
;;             (eq? (service-kind service) gdm-service-type))
;;           services-list))



;; =================

;; wasn't sure where to put this code, but i don't want to write it in the
;; repl everytime. i need to work on packages with nonguix module dependencies
;; and they're not in my guile path


(define dc/guix-site
  ;; (list "/home/dc/.config/guix/current/lib/guile/3.0/site")
  (list "/home/dc/.config/guix/current/share/guile/site/3.0"))
(define dc/guix-site-ccache
  (list "/home/dc/.config/guix/current/lib/guile/3.0/site-ccache"))
(define dc/load-path '())
(define dc/load-compiled-path '())
(define (dc/insert-into n list el)
  (unless (<= n (length list))
    (throw "argument error" ))
  (append (take list n) el (drop list n)))

;; (dc/insert-into 5 '(1 2 3 4 5) '(a b c))

(define (dc/geiser-update-load-path n site)
  ;; cache the load-path but only once
  (unless dc/load-path
    (set! dc/load-path %load-path))

  (let ((lp (dc/insert-into n %load-path site)))
    (set! %load-path lp)
    %load-path))

(define (dc/geiser-update-load-compiled-path n site-ccache)
  ;; cache the load-path but only once
  (unless dc/load-compiled-path
    (set! dc/load-compiled-path %load-compiled-path))

  (let ((lcp (dc/insert-into n %load-compiled-path site-ccache)))
    (set! %load-compiled-path lcp)
    %load-compiled-path))

;; (dc/geiser-update-load-path 2 dc/guix-site)
;; (dc/geiser-update-load-compiled-path 2 dc/guix-site-ccache)
