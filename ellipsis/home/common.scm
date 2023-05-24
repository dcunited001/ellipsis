(define-module (ellipsis home common)
  #:use-module (gnu home)
  #:use-module (srfi srfi-1)

  #:use-module (gnu packages)
  #:use-module (gnu packages linux)

  #:use-module (gnu services)

  #:use-module (gnu home services)
  #:use-module (gnu home services shepherd)
  #:use-module (gnu home services xdg)
  #:use-module (gnu home services shells)

  #:use-module (guix gexp))



;; echo > $dirsfile
;; echo "XDG_DOCUMENTS_DIR=\"$_DATA/xdg/Documents\"" >> $dirsfile
;; echo "XDG_MUSIC_DIR=\"$_DATA/xdg/Music\"" >> $dirsfile
;; echo "XDG_PICTURES_DIR=\"$_DATA/xdg/Pictures\"" >> $dirsfile
;; echo "XDG_VIDEOS_DIR=\"$_DATA/xdg/Videos\"" >> $dirsfile
;; echo "XDG_TEMPLATES_DIR=\"$_DATA/xdg/Templates\"" >> $dirsfile



;; (home-environment
;;  ;; Below is the list of packages that will show up in your
;;  ;; Home profile, under ~/.guix-home/profile.
;;  (packages (specifications->packages
;;             (list "emacs-native-comp" "guile" "guix"
;;                   "glibc-locales")))

;;  ;; Below is the list of Home services.  To search for available
;;  ;; services, run 'guix home search KEYWORD' in a terminal.
;;  (services
;;   (list

;;    )))


;; ;; (simple-service 'sway-config
;; ;;                 home-files-service-type
;; ;;                 (list `("sway.config" ,(local-file))))
