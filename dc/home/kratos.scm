(use-modules (gnu home services desktop)
             (gnu home services gnupg)
             (gnu home services dotfiles)
             (gnu home services shells)
             (gnu home services sound)
             (gnu home services)
             (gnu home)
             (gnu packages admin)
             (gnu packages)
             (gnu packages package-management)
             (gnu services guix)
             (gnu services shepherd)
             (gnu services)
             (gnu)
             (guix channels)
             (guix gexp)
             (guix packages)
             (guix profiles)
             (guix inferior)
             (guix ui)
             (srfi srfi-1)

             (dc config))

(define %host-name "kratos")

(define %udiskie-packages
  (list "udiskie"))

(define %gtk-packages
  ;; gsettings-desktop-schemas

  (list "xsettingsd"
        "dconf"))

(define %gtktheme-packages
  (list "arc-icon-theme"
        "matcha-theme"
        "hicolor-icon-theme"
        "gnome-icon-theme"
        "gnome-backgrounds"
        "papirus-icon-theme"
        "breeze-icons"
        "yad"))

(define %dmenu-packages
  (list "dmenu"
        "rofi"))

(define %gpg-packages
  (list "pinentry-gtk2"
        "gnupg"))

(define %terminator-packages
  (list "terminator"))

(define %printer-packages
  (list "system-config-printer"))

(define %x11-packages
  ;; trash-cli?
  (list "xset"
        "xrdb"
        "xhost"
        "xss-lock"
        "xscreensaver"
        "xrandr"
        "arandr"
        "autorandr"))

(define %desktop-packages
  (list "xdg-utils"
        "xdg-user-dirs"
        "libinput"))

(define %fcitx5-packages
  (list "fcitx5"
        "anthy"
        "fcitx5-anthy"
        "fcitx5-configtool"
        "fcitx5-chinese-addons"
        "fcitx5-material-color-theme"
        "fcitx5-gtk"
        "fcitx5-gtk4"
        "fcitx5-qt"))

(define %wayland-environment
  ;; on guix system, agreety will set XDG_SESSION_TYPE
  '(("XDG_SESSION_TYPE" . "wayland")
    ("QT_QPA_PLATFORM" . "wayland-egl")))

(define %gtk-environment
  '(("GTK2_RC_FILES" . "$HOME/.gtkrc-2.0")))

(define wayland-environment
  ;; necessary for sway
  `(("XDG_CURRENT_DESKTOP" . "KDE")

    ;; potential necessary for styling/theming
    ("QT_QPA_PLATFORMTHEME" . "qt5ct")
    ("QT_WAYLAND_FORCE_DPI" . "physical")
    ("QT_WAYLAND_DISABLE_WINDOWDECORATION" . "1")

    ;; necessary on a per-app basis or for the entire wm session,
    ("SDL_VIDEODRIVER" . "wayland")
    ("SDL_IM_MODULE" . "fcitx")))

(define %home-manifest
  (specifications->packages
   (append
    %gpg-packages
    %desktop-packages
    ;; %gtk-packages
    ;; %dmenu-packages
    ;; %fcitx5-packages
    ;; %printer-packages
    ;; %terminator-packages
    ;; %udiskie-packages
    (list "guile-next"
          "guile-ares-rs"
          "glibc-locales"
          "guile-colorized"

          "xsettingsd"
          "dconf"

          "keepassxc"))))

(home-environment
  (packages (list htop))
  (services
   (append
    (list
     (service home-bash-service-type
              (home-bash-configuration
               (guix-defaults? #t)
               (bash-profile)))
     (simple-service 'wayland-environment-variables
                     home-environment-variables-service-type
                     %wayland-environment)
     (simple-service 'gtk-environment-variables
                     home-environment-variables-service-type
                     %gtk-environment)
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
     (service home-dotfiles-service-type
              (home-dotfiles-configuration
               (directories (list %dotfiles-directory)))))
    %base-home-services)))


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
