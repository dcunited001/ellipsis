(define-module (dc home kratos)
  #:use-module (gnu home services desktop)
  #:use-module (gnu home services gnupg)
  #:use-module (gnu home services dotfiles)
  #:use-module (gnu home services shells)
  #:use-module (gnu home services sound)
  #:use-module (gnu home services)
  #:use-module (gnu home)

  #:use-module (gnu packages admin)
  #:use-module (gnu packages)
  #:use-module (gnu packages package-management)
  #:use-module (gnu services guix)
  #:use-module (gnu services shepherd)
  #:use-module (gnu services)
  #:use-module (gnu)

  #:use-module (guix channels)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix profiles)
  #:use-module (guix ui)
  #:use-module (srfi srfi-1)

  #:use-module (dc common)
  #:use-module (dc config)
  #:use-module (dc home services alacritty))

(define %host-name "kratos")

(define home-manifest
  (specifications->packages
   (append
    gpg-packages
    (list "pinentry-qt5")
    desktop-packages
    fontconfg-packages
    ;; gtk-packages
    ;; gtk-theme-packages
    ;; dmenu-packages
    ;; fcitx5-packages
    ;; printer-packages
    ;; terminator-packages
    ;; udiskie-packages
    (list "guile-next"
          "guile-ares-rs"
          "glibc-locales"
          "guile-colorized"

          "xsettingsd"
          "dconf"

          "keepassxc"))))

;; TODO: gpg-agent: reopen configuration instead of defining a new one
(define dc-gpg-agent-configruation
  (home-gpg-agent-configuration
   ;; (pinentry-program (file-append pinentry-gtk2 "/bin/pinentry-gtk-2"))
   (pinentry-program (file-append pinentry- "/bin/pinentry-qt5"))

   (ssh-support? #t)
   (default-cache-ttl 60)
   (default-cache-ttl-ssh 60)
   (max-cache-ttl 600)
   (max-cache-ttl-ssh 600)
   (extra-content "
no-allow-external-cache
no-allow-mark-trusted
no-allow-emacs-pinentry
no-allow-loopback-pinentry")))

(define kratos-home-environment
  (home-environment
    (services
     (append
      (list
       (simple-service 'wayland-environment-variables
                       home-environment-variables-service-type
                       wayland-environment)
       (simple-service 'dc-shell-profile
                       home-shell-profile-service-type
                       (list ""))
       (simple-service 'gtk-environment-variables
                       home-environment-variables-service-type
                       gtk-environment)
       (service home-gpg-agent-service-type
                dc-gpg-agent-configuration)
       (service home-bash-service-type
                (home-bash-configuration
                 (aliases me-aliases)
                 (bashrc (list (local-file
                                "/home/dc/.guix-home-test/.bashrc"
                                "bashrc")))
                 (bash-profile (list (local-file
                                      "/home/dc/.guix-home-test/.bash_profile"
                                      "bash_profile")))
                 (bash-logout (list (local-file
                                     "/home/dc/.guix-home-test/.bash_logout"
                                     "bash_logout")))))

       ;; NOTE: not really sure this a great pattern
       (service (alacritty-service-type dc-alacritty-xdg-files))

       ;; NOTE: stowing this will likely conflict (unless abcdw's power level is over 9,000,000)
       (service home-dotfiles-service-type
                (home-dotfiles-configuration
                 (directories (list %dotfiles-directory))))

       )
      (list dc-channels-service)
      %base-home-services))))


;; potentially necessary for some steam tweaks
;; ("SDL_DYNAMIC_API" . "/usr/lib/libSDL2-2.0.so")

;; XDG_DATA_DIRS
;;
;; /home/dc/.nix-profile/share
;; /flatpak/agenda/.local/flatpak/exports/share
;; /flatpak/steam/.local/flatpak/exports/share
;; /flatpak/dc/.local/share/flatpak/exports/share
;; /home/dc/.guix-profile/share
;; /home/dc/.local/share/flatpak/exports/share
;; /flatpak/steam/.local/flatpak/exports/share
;; /flatpak/agenda/.local/flatpak/exports/share
;; /var/lib/flatpak/exports/share
;; /usr/local/share


;; TODO: emacs-arch-integration service
;; systemd service
;; desktop files

;; extensions?

;; TODO: return a home-environment



;; TODO: home-xdg-mime-application-service-type

;; Example config
;;
;;  (home-xdg-mime-applications-configuration
;;   (added '((x-scheme-handler/magnet . torrent.desktop)))
;;   (default '((inode/directory . file.desktop)))
;;   (removed '((inode/directory . thunar.desktop)))
;;   (desktop-entries
;;    (list (xdg-desktop-entry
;;           (file "file")
;;           (name "File manager")
;;           (type 'application)
;;           (config
;;            '((exec . "emacsclient -c -a emacs %u"))))
;;          (xdg-desktop-entry
;;           (file "text")
;;           (name "Text editor")
;;           (type 'application)
;;           (config
;;            '((exec . "emacsclient -c -a emacs %u")))
;;           (actions
;;            (list (xdg-desktop-action
;;                   (action 'create)
;;                   (name "Create an action")
;;                   (config
;;                    '((exec . "echo hi"))))))))))
