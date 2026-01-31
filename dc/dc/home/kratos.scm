;;; Copyright © 2025 David Conner <aionfork@gmail.com>

(define-module (dc home kratos)
  #:use-module (dc home config)
  #:use-module (dc home common)
  #:use-module (dc home services)
  #:use-module (dc home services i3)
  #:use-module (dc home services screen)
  #:use-module (dc home services alacritty)

  #:use-module (ellipsis utils)

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

  #:use-module (srfi srfi-1)

  #:export (kratos-home-environment))

;;; use modules
;;;; use-package-modules
(use-package-modules gnupg password-utils)

;;;; use-service-modules

;;; config
(define %host-name "kratos")
(define %xdg "/data/xdg")

;;;; packages
(define home-packages
  (append
   essential-packages
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
   yubikey-packages
   notification-packages
   ;; git-stack-bin (from ellipsis)
   (list keepassxc pwsafe)
   guile-packages))

;;;; XDG
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

;; =============================================
;;; Environment

(define (kratos-env-base env-defaults)
  (define env-wayland
    '(("XDG_SESSION_TYPE" . "wayland")
      ("QT_QPA_PLATFORM" . "wayland-egl")))

  (define env-apps
    '(("EMAIL" . "aionfork@gmail.com")  ; interpreted by emacs.
      ("BROWSER" . "firefox")
      ("VISUAL" . "doomclient -- -c")
      ("EDITOR" . "doomclient -- -nw")
      ("ALTERNATE_EDITOR" . "vim")))

  (define env-paths
    '(("_ECTO" . "/data/ecto")
      ("_REPO" . "/data/repo")
      ("_LANG" . "/data/lang")
      ("_STEAM" . "/flatpak/steam")
      ("_WALLPAPERS" . "/data/xdg/Wallpapers/anime")
      ("DOOMDIR" . "$HOME/.doom.d")))

  (define julia-env
    '(("JULIA_SHELL" . "/bin/sh")
      ("JULIA_EDITOR" . "$EDITOR")))

  (define qt-env
    '(("XDG_SESSION_TYPE" . "wayland")
      ("QT_QPA_PLATFORM" . "wayland-egl")
      ;; potential necessary for styling/theming
      ("QT_QPA_PLATFORMTHEME" . "qt5ct")
      ("QT_WAYLAND_FORCE_DPI" . "physical")
      ("QT_WAYLAND_DISABLE_WINDOWDECORATION" . #t)))

  ;; GDK_BACKEND may interfere with program startup (e.g. chromium and
  ;; electron apps). maybe set per app instead of globally.

  (define env-gtk
    '(("GTK2_RC_FILES" . "$HOME/.gtkrc-2.0")
      ("GDK_BACKEND" . "wayland")
      ;; disables accessibility?
      ("NO_AT_BRIDGE" . #t)))

  ;; SDL_VIDEODRIVER may interfere with startup of old sdl games

  (define env-sdl
    '(("SDL_VIDEODRIVER" . "wayland")   ; override per-app for old SDL games
      ("SDL_DYNAMIC_API" . "/usr/lib/libSDL2-2.0.so")))
                                        ; steam tweaks
  (define env-xdg
    '(("XDG_CURRENT_DESKTOP" . "KDE")))

  (alist-append-uniq
   env-defaults
   env-wayland
   env-apps
   env-paths
   env-gtk
   env-xdg))

;; ---------------------------------------------
;;;; Environment Base Service

;; This environment service should register first

(define kratos-env-base-service
  (simple-service
   'kratos-env-base-service
   home-environment-variables-service-type
   (kratos-env-base %dc-env-base)))

;; =============================================
;;; Services

;; ---------------------------------------------
;;;; GPG

;; TODO: gpg-agent: reopen configuration instead of defining a new one
(define kratos-gpg-agent-configuration
  (home-gpg-agent-configuration
   (pinentry-program (file-append pinentry-qt "/bin/pinentry-qt"))
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

;; =============================================
;;; Applications

;;;; alacritty
(define kratos-alacritty-service
  (alacritty-service-type dc-alacritty-xdg-files))

;;;; mpv
(define kratos-mpv-configuration
  (home-mpv-configuration
   ;; (input-conf '())
   (mpv-conf '((global ((vo . gpu)
                        (hwdec . vaapi)
                        (profile . gpu-hq)
                        (scale . ewa_lanczossharp)
                        (cscale . ewa_lanczossharp)))))))

(define kratos-application-services
  ;; NOTE: not really sure this a great pattern
  (list (service kratos-alacritty-service dc-alacritty-xdg-files)
        dc-zathura-service
        screen-service-type
        (service home-mpv-service-type kratos-mpv-configuration)))



;; =============================================
;;; Home Configuration

(define (kratos-home-environment)
  (home-environment
    (packages home-packages)
    (services
     (append
      (list
       (simple-service 'kratos-shell-profile
                       home-shell-profile-service-type
                       (list))
       (service home-gpg-agent-service-type kratos-gpg-agent-configuration)
       (service home-bash-service-type dc-bash-configuration)
       (service home-inputrc-service-type dc-inputrc-configuration)
       (service i3-service-type)

       (service home-xdg-user-directories-service-type
                kratos-xdg-user-directories)

       ;; NOTE: stowing this will likely conflict
       ;;   (unless abcdw's power level is over 9,000,000)
       (service home-dotfiles-service-type
                (home-dotfiles-configuration
                 (source-directory ".")
                 (directories (list (string-append %dotfiles-directory "/df"))))))

      kratos-application-services
      (list
       dc-mcron-service
       dc-channels-service)
      %base-home-services))))

;; (kratos-home-environment)

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
