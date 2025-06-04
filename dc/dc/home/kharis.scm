;;; Copyright © 2025 David Conner <aionfork@gmail.com>

(define-module (dc home kharis)
  #:use-module (gnu home services desktop)
  #:use-module (gnu home services dotfiles)
  #:use-module (gnu home services gnupg)
  #:use-module (gnu home services pm)
  #:use-module (gnu home services shells)
  #:use-module (gnu home services sound)
  #:use-module (gnu home services xdg)
  #:use-module (gnu home services)
  #:use-module (gnu home)

  #:use-module (gnu packages admin)
  #:use-module (gnu packages package-management)
  #:use-module (gnu packages)
  #:use-module (gnu services guix)
  #:use-module (gnu services shepherd)
  #:use-module (gnu services)
  #:use-module (gnu)

  #:use-module (guix channels)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix profiles)
  #:use-module (guix ui)

  #:use-module (dc home config)
  #:use-module (dc home common)
  #:use-module (dc home services alacritty)

  #:use-module (srfi srfi-1))

(define %host-name "kharis")

(define %kharis-environment
  `(("SHELL" . ,(file-append bash "/bin/bash"))
    ("MAIL" . "geary")
    ("LESSHISTFILE" . "$XDG_CACHE_HOME/.lesshst")
    ("_JAVA_AWT_WM_NONREPARENTING" . #t)

    ("EMAIL" . "aionfork@gmail.com")    ; interpreted by emacs.
    ("BROWSER" . "firefox")
    ("VISUAL" . "emacsclient -- -c")
    ("EDITOR" . "doomclient -- -nw")
    ("ALTERNATE_EDITOR" . "vim")

    ;; TODO: set by gtk_greeter
    ;; ("XDG_SESSION_TYPE" . "wayland")
    ;;
    ;; should also be set by GTK_USER_SESSION (ENV)
    ;;
    ;; ("XDG_CURRENT_DESKTOP" . "  -> hyperland or sway <- ")

    ("QT_QPA_PLATFORM" . "wayland-egl")
    ("QT_QPA_PLATFORMTHEME" . "qt5ct")
    ("QT_WAYLAND_FORCE_DPI" . "physical")
    ("QT_WAYLAND_DISABLE_WINDOWDECORATION" . #t)

    ("_ECTO" . "/data/ecto")
    ("_REPO" . "/data/repo")
    ("_LANG" . "/data/lang")
    ("_WALLPAPERS" . "/data/xdg/Wallpapers/anime")
    ("DOOMDIR" . "$HOME/.doom.d")

    ("GTK2_RC_FILES" . "$HOME/.gtkrc-2.0")
    ("GDK_BACKEND" . "wayland")
    ("MOZ_DBUS_REMOTE" . #t)
    ("NO_AT_BRIDGE" . #t)))

(define home-manifest
  (specifications->packages
   (append
    x11-packages
    gpg-packages
    desktop-packages
    gtk-packages
    gtk-theme-packages
    fontconfig-packages
    dmenu-packages
    fcitx5-packages
    printer-packages
    terminator-packages
    udiskie-packages
    (list "guile-next"
          "guile-ares-rs"
          "glibc-locales"
          "guile-colorized"

          "xsettingsd"
          "dconf"

          "keepassxc"))))

(define kharis-batsignal-service
  (service home-batsignal-service-type
           (home-batsignal-configuration
            ;; TODO: HOME: KHARIS: batsignal notifications icon (requires gexp?)
            (full-level 98))))

(define (kharis-home-environment)
  (home-environment
    (services
     (append
      (list
       ;; (simple-service 'wayland-environment-variables
       ;;                 home-environment-variables-service-type
       ;;                 wayland-environment)
       (simple-service 'kharis-environment-variables
                       home-environment-variables-service-type
                       %kharis-environment)
       (simple-service 'dc-shell-profile
                       home-shell-profile-service-type
                       (list ""))
       (simple-service 'gtk-environment-variables
                       home-environment-variables-service-type
                       gtk-environment)
       (service home-gpg-agent-service-type dc-gpg-agent-configuration)
       dc-bash-configuration
       dc-home-shell-aliases-service
       ;; dc-home-systemd-aliases-service

       kharis-batsignal-service
       ;; NOTE: not really sure this a great pattern
       (service (alacritty-service-type dc-alacritty-xdg-files))

       (service home-dotfiles-service-type
                (home-dotfiles-configuration
                 (directories (list (string-append %dotfiles-directory "/df"))))))
      (list dc-channels-service)
      %base-home-services))))
