(use-modules
 (shepherd service)
 (shepherd service monitoring)
 ;; (shepherd service repl)
 ;; ((ice-9 ftw) #:select (scandir))
 )

;;* Shepherd

;;** Daemonize the Root Service

;; Send shepherd into the background
;; - this must occur before any services are started
(perform-service-action root-service 'daemonize)

;;** Register Services

;;*** mcron

(define mcron
  (service '(mcron)
    #:start (make-forkexec-constructor '("mcron"))
    #:stop  (make-kill-destructor)
    #:respawn? #t))

(register-services (list mcron))
(start-in-the-background '(mcron))

;;*** monitoring

(register-services (list (monitoring-service #:period (* 15 60))))
(start-service (lookup-service 'monitoring))

;;*** xsettingsd

(define xsettingsd
  (let* ((service-cmd
          (list "/home/dc/.guix-extra-profiles/desktop/desktop/bin/xsettingsd"))
         (log-time (strftime "%Y-%m-%d-" (gmtime (current-time))))
         (log-file (string-append (mkdtemp "/tmp/xsettingsd-XXXXXX")
                                  "/xsettingsd-"
                                  log-time
                                  (gethostname)
                                  ".log")))
    ;; (pretty-print log-file)
    (service '(xsettingsd)
             #:start (make-forkexec-constructor service-cmd
                                                #:log-file log-file)
             #:stop  (make-kill-destructor)
             #:respawn? #f)))

(register-services (list xsettingsd))
(start-in-the-background '(xsettingsd))

;;*** dunst

(define dunst
  (let* ((service-cmd
         (list "/home/dc/.guix-extra-profiles/desktop/desktop/bin/dunst"))
        (log-time (strftime "%Y-%m-%d-" (gmtime (current-time))))
        (log-file (string-append (mkdtemp "/tmp/dunst-XXXXXX")
                                 "/dunst-"
                                 log-time
                                 (gethostname)
                                 ".log")))

    (service '(dunst)
             #:start (make-forkexec-constructor service-cmd
                                                #:log-file log-file)
             #:stop  (make-kill-destructor)
             #:respawn? #f)))

(register-services (list dunst))

;;*** polybar

(define polybar
  (let* ((service-cmd (list "/home/dc/.guix-extra-profiles/desktop/desktop/bin/polybar"))
         (log-time (strftime "%Y-%m-%d-" (gmtime (current-time))))
         (log-file (string-append (mkdtemp "/tmp/polybar-XXXXXX")
                                  "/polybar-"
                                  log-time
                                  (gethostname) ".log")))

    (service '(polybar)
             #:start (make-forkexec-constructor service-cmd
                                                #:log-file log-file)
             #:stop  (make-kill-destructor)
             #:respawn? #f)))

(register-services (list polybar))

;; shepherd does not load these without error
;; (load (string-append (dirname (current-filename)) "/init.d/polybar.scm"))
;; (load "/home/dc/.dotfiles/.config/shepherd/init.d/polybar.scm")

;;*** Gnome Keyring Daemon

(define gnome-keyring-daemon
  (let* ((service-cmd '("gnome-keyring-daemon"
                        "--start"
                        "-f"
                        "--components=secrets")))
    (service '(gnome-keyring-daemon)
             #:start (make-forkexec-constructor service-cmd)
             #:stop (make-kill-destructor)
             #:respawn? #f)))

(register-services (list gnome-keyring-daemon))

;;*** XDG Desktop Portal

(define xdg-desktop-portal-gtk
  (let* ((service-cmd
         (list "/home/dc/.guix-extra-profiles/xdg/xdg/libexec/xdg-desktop-portal-gtk" "-r"))
        (log-time (strftime "%Y-%m-%d-" (gmtime (current-time))))
        (log-file (string-append (mkdtemp "/tmp/xdg-desktop-portal-XXXXXX")
                                 "/portal-gtk-"
                                 log-time
                                 (gethostname)
                                 ".log")))

    (service '(xdg-desktop-portal-gtk)
             #:start (make-forkexec-constructor service-cmd
                                                #:log-file log-file)
             #:stop  (make-kill-destructor)
             #:respawn? #f)))

(register-services (list xdg-desktop-portal-gtk))

;;*** Syncthing

(define syncthing
  (let* ((service-cmd '("syncthing"
                        "-no-browser")))
    (service '(syncthing)
             ;; #:documentation "Run `syncthing' without calling the browser"
             #:start (make-forkexec-constructor service-cmd)
             #:stop (make-kill-destructor)
             #:respawn? #f)))

(register-services (list syncthing))


;;*** Gmacs

(define gmacs
  (let* ((start-cmd (list "gmacs" "--" "--fg-daemon"))
         (stop-cmd (list "gmacskill"))
         (log-time (strftime "%Y-%m-%d-" (gmtime (current-time))))
         (log-file (string-append (mkdtemp "/tmp/emacs-XXXXXX")
                                  "/emacs-g-"
                                  log-time
                                  (gethostname)
                                  ".log")))
    (service '(gmacs)
             #:start (make-forkexec-constructor start-cmd #:log-file log-file)
             #:stop (make-forkexec-constructor stop-cmd)
             #:respawn? #f)))

(register-services (list gmacs))

;;*** GPG Agent

;; TODO: gpg-agent: get make-system-de/constructor working
(define gpg-agent
  (let* ((gpg-path "/home/dc/.guix-extra-profiles/desktop/desktop/bin/")
         (gpg-connect-agent (string-append gpg-path "gpg-connect-agent"))
         (gpgconf (string-append gpg-path "gpgconf")))
    
    (service '(gpg-agent)
             #:start (make-system-constructor
                      (string-join (list gpg-connect-agent "/bye") " "))
             #:stop (make-system-destructor
                     (string-join (list gpgconf "--kill" "gpg-agent") " "))
             #:respawn? #f)))


(register-services (list gpg-agent))

;;** Start Services

;; monitoring, mcron and xsettingsd have already be started

(start-in-the-background '(dunst
                           polybar
                           ;; gmacs ;;
                           xdg-desktop-portal-gtk
                           gnome-keyring-daemon
                           ;;gpg-agent
                           ))

;;*** Other Services

;; - start syncthing on demand
;; - start gmacs on demand
;; - GDM starts XDG Desktop Portal (nope, it's not starting)
