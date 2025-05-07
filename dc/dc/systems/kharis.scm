;;* Module: kharis
(define-module (dc systems kharis)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 format)

  #:use-module (gnu)
  #:use-module (gnu system)
  #:use-module (gnu system nss)
  #:use-module (gnu system setuid)
  #:use-module (gnu system privilege)

  #:use-module (nongnu packages linux)
  #:use-module (nongnu system linux-initrd)

  #:use-module (ellipsis packages python-xyz)
  #:use-module (ellipsis services security-token)

  #:use-module (dc systems base))

(use-service-modules guix admin sysctl pm nix avahi dbus cups
                     desktop linux mcron networking ssh ;; xorg?
                     security-token docker audio virtualization
                     containers)

(use-package-modules nfs certs shells ssh tls gnupg security-token
                     bash emacs emacs-xyz gnome networking libusb
                     fonts cups audio xorg xdisorg linux file-systems
                     version-control package-management freedesktop rsync
                     cryptsetup hardware guile vim golang golang-crypto)

;; (and=> (getenv "HOME") (lambda (home) (string-append home home)))
(define-public %home
  (and=> (getenv "HOME")
         (lambda (home)
           home)))

;; TODO: in (ellipsis home utils), read these from (string-append %host-name ".json")

(define %host-name "kharis")

;; jhash=$(echo jupyter-lab | sha256sum | head -c8)
;; echo $((0x$jhash % 1000)) # probably a bad idea all around
(define %kharis-service-users
  `((jupyter-lab ,(user-account (name "jupyter-lab") (groups)))))

;;;  can still download astral-uv from guix, but the paths just need to be
;;;  compat with the runtime... though i haven't actually read
;;;  astral-sh/python-build-standalone
;;;
;;; as long as i can connect to a Jupyter-lab kernel runtime from emacs, then
;;; that's fine.

;; system-specific users should go here
(define %kharis-users
  (append
   (list (dc-user
          (cons* "libvirt" "docker" %dc-my-groups)))
   %kharis-service-users
   %base-user-accounts))

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

(define-public %kharis-tlp-conf
  (tlp-configuration
   (cpu-boost-on-ac? #t)
   (tlp-default-mode "AC") ;; this is the default
   (sound-power-save-on-bat 0)
   (nmi-watchdog? #t)
   (cpu-scaling-min-freq-on-bat 1700000)
   (cpu-scaling-max-freq-on-bat 2100000)
   (cpu-scaling-min-freq-on-ac 2100000)
   (cpu-scaling-max-freq-on-ac 2100000)
   (wifi-pwr-on-bat? #t)))

(define-public %kharis-openssh-conf
  (openssh-configuration
   (openssh openssh-sans-x)
   (port-number (string->number
                 (or (getenv "_OPENSSH_PORT") "22")))
   (password-authentication? #f)
   (allow-agent-forwarding? #f)
   (allow-tcp-forwarding? #t)
   (accepted-environment '("COLORTERM"))
   (authorized-keys
    `(("dc" ,(local-file ".ssh/dc.authorized_keys"))))))

;; to have mouse at console (more annoying than anything tbh)
(define-public %kharis-gpm-conf
  (gpm-configuration
   ;; defaults, should work for IBM trackpoints
   (options '("-m" "/dev/input/mice" "-t" "ps2"))))

;; TODO: (send-notification-command "a/herd/service/or/root/script")
;; logs: /var/log/earlyoom
(define-public %kharis-earlyoom-conf
  (earlyoom-configuration
   (minimum-available-memory 10) ;; default

   ;; noswap, but both mem/swap
   ;; must be below threshold for
   ;; oom to act
   (minimum-free-swap 1)
   (prefer-regexp "syncthing|firefox")
   (show-debug-messages? #t)))

;; TODO: this won't work without a gexp.  it may not be a great idea
;; (define-public %kharis-upgrade-configuration
;;   (unattended-upgrade-configuration
;;    (inherit %dc-unattended-upgrade-configuration)
;;    (channels #~(@ (guix describe) (current-channels)))))

(define-public %kharis-greetd-conf
  (greetd-configuration
   (greeter-supplementary-groups (list "video" "input"))
   (terminals
    (list
     ;; TTY7 is the graphical login screen for Sway
     ;; Set up remaining TTYs for terminal use
     (greetd-terminal-configuration (terminal-vt "1"))
     (greetd-terminal-configuration (terminal-vt "2"))
     (greetd-terminal-configuration (terminal-vt "3"))
     (greetd-terminal-configuration (terminal-vt "4"))
     (greetd-terminal-configuration (terminal-vt "5"))
     (greetd-terminal-configuration (terminal-vt "6"))
     (greetd-terminal-configuration
      (terminal-vt "7")
      (terminal-switch #t)
      (default-session-command
        (greetd-wlgreet-sway-session
         ;; TODO background
         (sway-configuration
          (plain-file "sway-greet.conf"
                      (string-append
                       "output * bg /data/xdg/Wallpapers/"
                       %host-name "-greetd.jpg fill\n"))))))
     (greetd-terminal-configuration (terminal-vt "8"))))))

(define system
  (operating-system
    (host-name %host-name)
    (timezone "America/New_York")
    (locale "en_US.UTF-8")

    (kernel linux)
    (firmware (cons* linux-firmware
                     ;; amd-microcode
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
    (users %kharis-users)

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

    ;; "Install the given fonts on the specified ttys (fonts are per virtual
    ;; console on GNU/Linux).  The value of this service is a list of tty/font
    ;; pairs.  The font can be the name of a font provided by the @code{kbd}
    ;; package or any valid argument to @command{setfont}, as in this example:"
    ;; `(("tty1" . "LatGrkCyr-8x16")
    ;;   ("tty2" . ,(file-append font-tamzen
    ;;                           "/share/kbd/consolefonts/TamzenForPowerline10x20.psf"))
    ;;   ("tty3" . ,(file-append font-terminus
    ;;                           "/share/consolefonts/ter-132n"))) ; for HDPI

    (privileged-programs
     (append (list (privileged-program
                    (program (file-append (specification->package "swaylock")))
                    (setuid? #t)))
             %dc-privileged-programs))

    (services
     (append
      (modify-services %base-services
        ;; (delete console-font-service-type)
        (delete agetty-service-type)
        (delete mingetty-service-type))

      (list
       %dc-extra-file-env
       %dc-extra-file-ld-linux
       (service greetd-service-type %kharis-greetd-conf)
       %dc-nonguix-substitutes-service

       polkit-wheel-service

       ;; networking
       %dc-ntp-service
       %dc-network-manager-service
       (service wpa-supplicant-service-type)
       (service modem-manager-service-type)
       %dc-nntp-service
       (service openssh-service-type %kharis-openssh-conf)

       ;; (service zerotier-one-service-type)

       ;; peripherals
       (service usb-modeswitch-service-type)
       (service bluetooth-service-type
                (bluetooth-configuration
                 (auto-enable? #t)))
       (service fprintd-service-type)
       (service pcscd-service-type)
       ;; connect phone to laptop
       ;; (simple-service 'mtp udev-service-type (list libmtp))

       (service avahi-service-type)
       (service udisks-service-type)
       (service upower-service-type)
       (service accountsservice-service-type)
       (service geoclue-service-type)
       (service polkit-service-type)
       (service elogind-service-type
                (elogind-configuration
                 (handle-lid-switch-external-power 'suspend)))
       (service dbus-root-service-type)

       ;; print/scan
       (service sane-service-type)
       (service cups-pk-helper-service-type)
       %dc-cups-service
       (service colord-service-type)

       ;; desktop
       fontconfig-file-system-service
       (service x11-socket-directory-service-type)

       ;; hardware
       (service thermald-service-type)
       (service tlp-service-type %kharis-tlp-conf)
       %dc-auditd-service
       %dc-ras-daemon-service
       (service earlyoom-service-type %kharis-earlyoom-conf)
       (service gpm-service-type %kharis-gpm-conf)
       (yubikey-udev-rules)

       ;; containers
       %dc-docker-service
       %dc-containerd-service
       %dc-oci-container-service
       %dc-rootless-podman-service

       ;; vm's
       %dc-libvirt-service
       %dc-virtlog-service ;; printing

       ;; (service sysctl-service-type) ; configure kernel params @ boot
       %dc-pam-limits-service

       ;; enable pipewire via guix home instead
       ;; (service pulseaudio-service-type)
       (service alsa-service-type)

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
