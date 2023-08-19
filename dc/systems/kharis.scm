;;* Module: kharis
(define-module (dc systems kharis)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 format)

  #:use-module (gnu)
  #:use-module (gnu system)
  #:use-module (gnu system nss)
  #:use-module (gnu system setuid)

  #:use-module (nongnu packages linux)
  #:use-module (nongnu system linux-initrd)

  #:use-module (dc systems base)
  #:use-module (ellipsis home config))

(use-service-modules guix admin sysctl pm nix avahi dbus cups
                     desktop linux mcron networking xorg ssh
                     security-token docker audio virtualization)

(use-package-modules nfs certs shells ssh tls gnupg security-token
                     bash emacs emacs-xyz gnome networking libusb
                     fonts cups audio xorg xdisorg linux file-systems
                     version-control package-management freedesktop
                     cryptsetup hardware guile vim)

;; not sure what and=> is, but accepts (value procedure)
;; and seems to return a default for the maybe? or Some<T> pattern
(define-public %home
  (and=> (getenv "HOME")
         (lambda (home)
           home)))

;; TODO: in (ellipsis home utils), read these from (string-append %host-name ".json")

(define-public %_df
  (or (getenv "_df")
      (dc-default-path %home ".dotfiles")))

(define-public %_data
  (or (getenv "_data") "/data"))
(define-public %_lang
  (or (getenv "_lang")
      (dc-default-path %_data "lang")))
(define-public %_ecto
  (or (getenv "_ecto")
      (dc-default-path %_data "ecto")))
(define-public %_repo
  (or (getenv "_repo")
      (dc-default-path %_data "repo")))

(define-public %_flatpak
  (or (getenv "_flatpak") "/flatpak"))
(define-public %_steam
  (or (getenv "_steam")
      (dc-default-path %_flatpak "steam")))
(define-public %_agenda
  (or (getenv "_agenda")
      (dc-default-path %_flatpak  "agenda")))
(define-public %_wallpapers
  (or (getenv "_wallpapers")
      (dc-default-path %_data "xdg/Wallpapers/anime")))

(define-public %DOOMDIR
  (or (getenv "DOOMDIR")
      (dc-default-path ".doom.d")))

(define %host-name "kharis")

;; system-specific users should go here
(define %dc-users '())

(define-public %kharis-shell-keyboard
  (keyboard-layout
   "us" "altgr-intl"
   #:model "pc105"
	 ;; see gitlab.freedesktop.org/xkeyboard-config/xkeyboard-config/-/issue/344
   #:options '("caps:ctrl_modifier"
               ;; "ctrl:swapcaps_hyper" ; in 1.3.0 (hyper as Mod3)
               ;; "ctrl:hyper_capscontrol" ; in 1.5.0 (hyper as Mod4)
               "lv3:ralt_alt"
               "lv3:menu_switch")))

(define-public %kharis-tlp-service
  (service tlp-service-type
           (tlp-configuration
            (cpu-boost-on-ac? #t)
            (tlp-default-mode "AC") ;; this is the default
            (sound-power-save-on-bat 0)
            (nmi-watchdog? #t)
            (cpu-scaling-min-freq-on-bat 1700000)
            (cpu-scaling-max-freq-on-bat 2100000)
            (cpu-scaling-min-freq-on-ac 2100000)
            (cpu-scaling-max-freq-on-ac 2100000)
            (wifi-pwr-on-bat? #t))))

(define-public %kharis-openssh-service
  (service openssh-service-type
           (openssh-configuration
            (openssh openssh-sans-x)
            (port-number (string->number
                          (or (getenv "_OPENSSH_PORT") "22")))
            (password-authentication? #f)
            (allow-agent-forwarding? #f)
            (allow-tcp-forwarding? #t)
            (accepted-environment '("COLORTERM"))
            (authorized-keys
             `(("dc" ,(local-file ".ssh/dc.authorized_keys")))))))

;; to have mouse at console (more annoying than anything tbh)
(define-public %kharis-gpm-service
  (service gpm-service-type
           (gpm-configuration
            ;; defaults, should work for IBM trackpoints
            (options '("-m" "/dev/input/mice" "-t" "ps2")))))

(define system
  (operating-system
    (host-name %host-name)
    (timezone "America/New_York")
    (locale "en_US.UTF-8")

    (kernel linux)
    (firmware (cons* linux-firmware
                     amd-microcode
                     ;; realtek-firmware
                     %base-firmware))

    ;; (kernel-loadable-modules (wacom))
    (kernel-loadable-modules (list v4l2loopback-linux-module))

    (keyboard-layout %kharis-shell-keyboard)

    (bootloader (bootloader-configuration
                 (bootloader grub-efi-bootloader)
                 (targets '("/boot/efi"))
                 (keyboard-layout keyboard-layout)))

    ;; TODO: check whether I need to add my own user
    ;; - not showing up in /etc/group on kharis
    (groups %dc-base-groups)
    (users (append (list (dc-user (cons* "libvirt"
                                         "docker"
                                         %dc-my-groups)))
                   %dc-users
                   %base-user-accounts))

    ;; kharis would only need xf86-video-amdgpu for xorg
    ;; - otherwise the driver is included in the kernel
    ;; https://www.reddit.com/r/linux_gaming/comments/105sy93/comment/j3z2oct/?utm_source=share&utm_medium=web2x&context=3
    (packages (append %dc-desktop-packages
                      %base-packages))

    ;; TODO: customize console-font-service-type to have kanji in tty though
    ;; configuring kmscon is likely different.
    ;;
    ;; - hopefully this makes for things like ubiquitous modeline/tab config
    ;;
    ;; - kanji are easier to arrange in a grid, though switching between
    ;; single/double-width characters are another thing
    ;;
    ;;

    ;; "Install the given fonts on the specified ttys (fonts are per virtual
    ;; console on GNU/Linux).  The value of this service is a list of tty/font
    ;; pairs.  The font can be the name of a font provided by the @code{kbd}
    ;; package or any valid argument to @command{setfont}, as in this example:"
    ;; `(("tty1" . "LatGrkCyr-8x16")
    ;;   ("tty2" . ,(file-append font-tamzen
    ;;                           "/share/kbd/consolefonts/TamzenForPowerline10x20.psf"))
    ;;   ("tty3" . ,(file-append font-terminus
    ;;                           "/share/consolefonts/ter-132n"))) ; for HDPI


    (services
     (append
      (modify-services %base-services
        ;; (delete console-font-service-type)
        (delete login-service-type)
        (delete mingetty-service-type))
      (list

       (service colord-service-type)
       %dc-extra-file-env
       %dc-extra-file-ld-linux
       (dc-extra-file-flatpak)

       (service
        greetd-service-type
        (greetd-configuration
         (greeter-supplementary-groups (list "video" "input"))
         (terminals
          (list
           ;; TTY1 is the graphical login screen for Sway
           (greetd-terminal-configuration
            (terminal-vt "1")
            (terminal-switch #t)
            (default-session-command
              (greetd-wlgreet-sway-session
               ;; TODO background
               (sway-configuration
                (plain-file "sway-greet.conf" (string-append
                                               "output * bg /data/xdg/Wallpapers/" %host-name "-greetd.jpg fill\n"))))))

           ;; Set up remaining TTYs for terminal use
           (greetd-terminal-configuration
            (terminal-vt "2")
            (default-session-command
              (greetd-agreety-session
               (command (file-append bash "/bin/bash"))
               (command-args '("-l")))))
           (greetd-terminal-configuration (terminal-vt "3"))
           (greetd-terminal-configuration (terminal-vt "4"))
           (greetd-terminal-configuration (terminal-vt "5"))
           ;; (greetd-terminal-configuration
           ;;  (terminal-vt "5")
           ;;  (default-session-command
           ;;    (greetd-wlgreet-session
           ;;     (command (file-append bash "/bin/bash"))
           ;;     (command-args '("-l")))))
           (greetd-terminal-configuration (terminal-vt "6"))
	         (greetd-terminal-configuration (terminal-vt "7"))
           (greetd-terminal-configuration (terminal-vt "8"))
           ;; (greetd-terminal-configuration
	         ;;   (terminal-vt "9")
	         ;;   (default-session-command (file-append bash "/bin/bash")))
	         ))))

       %dc-nonguix-substitutes-service

       polkit-wheel-service
       (simple-service
        'mount-setuid-helpers
        setuid-program-service-type
        (map (lambda (program)
               (setuid-program
                (program program)))
             (list (file-append nfs-utils "/sbin/mount.nfs")
                   (file-append ntfs-3g "/sbin/mount.ntfs-3g")
                   (file-append
                    (specification->package "swaylock")
                    "/bin/swaylock"))))

       %dc-ntp-service
       %dc-network-manager-service

       (service wpa-supplicant-service-type)
       (service modem-manager-service-type)
       (service bluetooth-service-type
                (bluetooth-configuration
                 (auto-enable? #t)))
       (service usb-modeswitch-service-type)

       (service avahi-service-type)
       (service udisks-service-type)
       (service upower-service-type)
       (service geoclue-service-type)
       (service polkit-service-type)
       (service elogind-service-type
                (elogind-configuration
                 (handle-lid-switch-external-power 'suspend)))

       ;; TODO: equivalent?
       (dbus-service)
       ;; (service dbus-root-service-type)

       ;; Manage the fontconfig cache
       fontconfig-file-system-service

       (service thermald-service-type)
       %kharis-tlp-service
       %dc-ras-daemon-service
       %kharis-gpm-service

       %dc-nntp-service
       %kharis-openssh-service

       ;; for yubikey
       (service pcscd-service-type)

       ;; testing removing the fido2 functionality to restore yubikey
       (udev-rules-service 'fido2 libfido2 #:groups '("plugdev"))
       (udev-rules-service 'u2f libu2f-host #:groups '("plugdev"))
       (udev-rules-service 'yubikey yubikey-personalization)

       %dc-docker-service
       %dc-libvirt-service
       %dc-virtlog-service

       ;; scanning
       (service sane-service-type)

       ;; printing
       (service cups-pk-helper-service-type)
       %dc-cups-service

       %dc-pam-limits-service

       ;; Add udev rules for a few packages
       (udev-rules-service 'pipewire-add-udev-rules pipewire)
       (udev-rules-service 'brightnessctl-udev-rules brightnessctl)

       %dc-unattended-upgrade-service-type)))

    (mapped-devices
     (list (mapped-device
            (source (uuid "c6684f7e-a5e2-4096-a7d0-a0970221c971"))
            (targets (list "pde"))
            (type luks-device-mapping))

           ;; TODO: change these label names (to work across systems)
           (mapped-device
            (source "matrix")
            (targets (list "matrix-root"
                           "matrix-swapvol"
                           "matrix-home"
                           "matrix-flatpak"
                           "matrix-data"))
            (type lvm-device-mapping))))

    (file-systems (cons*
                   (file-system
                     (device (file-system-label "kharisRoot"))
                     (mount-point "/")
                     (type "btrfs")
                     (flags '(no-atime))
                     (options "space_cache=v2")
                     (needed-for-boot? #t)
                     (dependencies mapped-devices))

                   (file-system
                     (device (file-system-label "Home"))
                     (mount-point "/home")
                     (type "ext4")
                     (needed-for-boot? #f)
                     (dependencies mapped-devices))

                   (file-system
                     (device (file-system-label "Data"))
                     (mount-point "/data")
                     (type "ext4")
                     (needed-for-boot? #f)
                     (dependencies mapped-devices))

                   (file-system
                     (device (file-system-label "Flatpak"))
                     (mount-point "/flatpak")
                     (type "ext4")
                     (needed-for-boot? #f)
                     (dependencies mapped-devices))

                   (file-system
                     (device (file-system-label "Steam"))
                     (mount-point "/flatpak/steam")
                     (type "ext4")
                     (needed-for-boot? #f))

                   ;; /boot/efi needs to be enumerated here
                   ;;   in addition to the (bootloader...) declaration
                   (file-system
                     ;; (device (uuid "B184-6A00" 'fat))
                     ;; or: (device (file-system-label "KHARISEFI"))
                     (device "/dev/nvme0n1p1")
                     (mount-point "/boot/efi")
                     (type "vfat"))
                   %base-file-systems))

    (swap-devices (list (swap-space
                         (target (file-system-label "kharisSwap"))
                         (dependencies mapped-devices))))))

system
