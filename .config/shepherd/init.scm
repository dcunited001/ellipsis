;; strace -o strace5 -e trace=all \
;;   shepherd -s test.sock \
;;     -c ../init.scm \
;;     -l test.log \
;;     --pid=test.pid

(use-modules
 (shepherd service)
 (shepherd service monitoring)
 ;; (shepherd service repl)
 (fibers)
 ;; ((ice-9 ftw) #:select (scandir))
 )

;; Send shepherd into the background
;; - this must occur before any services are started
(perform-service-action root-service 'daemonize)

(define %autostart-services
  (list "mcron"
        "xsettingsd"
        ;"syncthing"
        ;"dunst"
        ;"xdg-desktop-portal-gtk"
	;"polybar"
        ))

;(let ((init-d "/home/dc/.config/shepherd/init.d/"))
  ;;(load (string-append init-d "mcron.scm"))
  ;;(load (string-append init-d "xsettingsd.scm"))
  ;;(load (string-append init-d "syncthing.scm"))
  ;;(load (string-append init-d "dunst.scm"))
  ;;(load (string-append init-d "xdg-desktop-portal-gtk.scm"))
;  )

(register-services (list (monitoring-service #:period (* 15 60))))
(start-service (lookup-service 'monitoring))

(let* ((init-d (string-append (dirname (current-filename)) "/init.d/"))
       (scm-files (map (lambda (s) (string-append init-d s ".scm"))
                       %autostart-services)))
  (for-each load scm-files))

;(start-in-the-background '(mcron))
(start-in-the-background (map string->symbol %autostart-services))
