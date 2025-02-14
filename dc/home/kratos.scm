;;; Copyright © 2025 David Conner <aionfork@gmail.com>

(define-module (dc home kratos)
  #:use-module (dc home config)
  #:use-module (dc home common)
  #:use-module (dc home services alacritty)

  #:use-module (gnu home services desktop)
  #:use-module (gnu home services dotfiles)
  #:use-module (gnu home services gnupg)
  #:use-module (gnu home services pm)
  #:use-module (gnu home services shells)
  #:use-module (gnu home services sound)
  #:use-module (gnu home services xdg)
  #:use-module (gnu home services)
  #:use-module (gnu home)

  #:use-module (gnu packages)

  #:use-module (gnu services guix)
  #:use-module (gnu services shepherd)
  #:use-module (gnu services configuration)
  #:use-module (gnu services)
  #:use-module (gnu)

  #:use-module (guix channels)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix profiles)
  #:use-module (guix ui)

  #:use-module (rde home services video)
  #:use-module (rde serializers ini)

  #:use-module (srfi srfi-1))

(use-package-modules gnupg password-utils)

(define %host-name "kratos")
(define %xdg "/data/xdg")

(define home-packages
  (append
   gpg-packages
   desktop-packages
   fontconfig-packages
   ;; gtk-packages
   ;; gtk-theme-packages
   ;; dmenu-packages
   ;; fcitx5-packages
   ;; printer-packages
   ;; terminator-packages
   ;; udiskie-packages
   (list keepassxc
         pwsafe)
   guile-packages))

(define kratos-xdg-user-directories
  ;; TODO: ensure XDG_CONFIG_HOME is set (see .xdg_shim.eg.sh)
  (home-xdg-user-directories-configuration
   (music (string-append %xdg "/Music"))
   (videos (string-append %xdg "/Videos"))
   (pictures (string-append %xdg "/Pictures"))
   (documents (string-append %xdg "/Documents"))
   (download (string-append %xdg "/Downloads"))
   (desktop (string-append %xdg "/Desktop"))
   (publicshare (string-append %xdg "/Public"))
   (templates (string-append %xdg "/Templates"))))

;; TODO: gpg-agent: reopen configuration instead of defining a new one
(define kratos-gpg-agent-configuration
  (home-gpg-agent-configuration
   (pinentry-program (file-append pinentry-qt5 "/bin/pinentry-qt5"))
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

(define kratos-alacritty-service
  (alacritty-service-type dc-alacritty-xdg-files))

(define kratos-wayland-environment-variables
  (simple-service 'wayland-environment-variables
                  home-environment-variables-service-type
                  wayland-environment))

(define (kratos-home-environment)
  (home-environment
    (packages home-packages)
    (services
     (append
      (list
       (simple-service 'dc-shell-profile
                       home-shell-profile-service-type
                       (list ""))
       (simple-service 'gtk-environment-variables
                       home-environment-variables-service-type
                       gtk-environment)
       (service home-gpg-agent-service-type kratos-gpg-agent-configuration)
       (service home-bash-service-type
                (home-bash-configuration
                 ;; (aliases '())
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
       (service kratos-alacritty-service dc-alacritty-xdg-files)
       (service home-xdg-user-directories-service-type kratos-xdg-user-directories)

       ;; NOTE: stowing this will likely conflict (unless abcdw's power level is over 9,000,000)
       (service home-dotfiles-service-type
                (home-dotfiles-configuration
                 (source-directory ".")
                 (directories (list %dotfiles-directory)))))
      (list dc-channels-service)
      %base-home-services))))

(kratos-home-environment)

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
