;;; Copyright © 2025 David Conner <aionfork@gmail.com>

(define-module (dc home config)
  #:use-module (ice-9 format)
  #:use-module (srfi srfi-1)
  #:use-module (guix channels)

  #:export (%home
            %dotfiles-directory
            %files-directory))

(define %home
  (and=> (getenv "HOME")
         (lambda (home)
           home)))

(define %dotfiles-directory
  (string-append %home "/.dotfiles" "/gh"))

(define %files-directory
  (string-append %home "/.dotfiles" "/gh/f"))

(define %data-directory "/data")

;;; config.scm ends here

;; (begin
;;   (use-modules
;;    (srfi srfi-34)
;;    (system repl error-handling))
;;   (define
;;     (make-user-module)
;;     (let ((m (make-fresh-user-module)))
;;       (module-use!
;;        m
;;        (resolve-interface
;;         '(shepherd service)))
;;       m))
;;   (register-services
;;    (map
;;     (lambda (file)
;;       (save-module-excursion
;;        (lambda ()
;;          (set-current-module
;;           (make-user-module))
;;          (load file))))
;;     '("/gnu/store/21xnmn4k2h8iwjvaq4jm7k9h9mdim9m9-shepherd-transient.scm"
;;       "/gnu/store/ifpfx9cwh58a2ihn9m89ry6g8cjsvwhw-shepherd-timer.scm"
;;       "/gnu/store/305inimrzz0fqmqk65c238slrmajprqw-shepherd-log-rotation.scm"
;;       "/gnu/store/gkpvb96gv2mbdlqnx4xi0a59mxr98r6z-shepherd-gpg-agent-ssh-agent.scm")))
;;   (perform-service-action
;;    root-service
;;    'daemonize)
;;   (format
;;    #t
;;    "Starting services...~%")
;;   (let ((services-to-start '(transient
;;                              timer
;;                              log-rotation
;;                              gpg-agent
;;                              ssh-agent)))
;;     (start-in-the-background
;;      services-to-start)
;;     (redirect-port
;;      (open-input-file "/dev/null")
;;      (current-input-port))))
