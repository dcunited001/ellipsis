;;; Module
;;
;; TODO: herd can't query service status or restart services. installation
;; needs to occur at login. sshd rejects connections (can't restart service
;; after IP address assignment). GDM doesnt permit login.
;;
(define-module (ellipsis systems nonguix-install-amd)
  #:use-module (srfi srfi-1)
  #:use-module (gnu)
  #:use-module (gnu system)
  #:use-module (gnu system nss)
  #:use-module (gnu system pam)

  #:use-module (nongnu packages linux)
  #:use-module (nongnu system linux-initrd)
  #:use-module (nongnu packages vpn)

  #:use-module (ellipsis packages gnupg)
  #:use-module (ellipsis packages tls)
  #:use-module (ellipsis packages emacs-xyz)
  #:use-module (ellipsis packages password-utils)
  #:use-module (ellipsis packages security-token)
  #:use-module (ellipsis packages golang-crypto)

  ;; get a list of channels
  #:use-module (guix describe)

  ;; gnutls packages
  #:use-module (gnu packages tls)

  #:export (nonguix-install-amd))

;; networking is [probably] needed for loopback
(use-service-modules networking ssh security-token authentication
                     desktop linux mcron networking xorg)
(use-package-modules curl wget rsync vim emacs emacs-xyz
                     wm freedesktop xdisorg fontutils fonts
                     networking linux time mtools acl hardware
                     package-management
                     lsof file-systems disk version-control
                     ssh gnupg cryptsetup security-token tls certs libusb
                     screen password-utils golang golang-crypto)

(define %my-channels (current-channels))

(define %system-groups
  (cons* (user-group (name "realtime") (system? #t))
         ;; created by service
         ;; (user-group (system? #t) (name "docker"))
         (user-group (name "plugdev") (system? #t))
         (user-group (name "yubikey") (system? #t))
         (user-group (name "fuse") (system? #t))
         (user-group (name "cgroup") (system? #t))
         (user-group (name "users") (id 1100))
         (user-group (name "dc") (id 1000))
         (remove (lambda (g) (equal? (user-group-name g) "users"))
                 %base-groups)))

(define %my-groups
  '("wheel"  ;; sudo
    "netdev" ;; network devices
    "kvm"
    "tty"
    "input"
    "fuse"
    "realtime" ;; Enable RT scheduling
    "lp"       ;; control bluetooth and cups
    "audio"    ;; control audio
    "video"    ;; control video
    ;; TODO: configure udev for group
    "yubikey" ;; yubikey (udev)
    "plugdev" ;; libu2f-host (udev)
    "users"))

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

(define wayland-packages
  (list grimblast
        xdg-desktop-portal))

(define sway-packages
  (list sway
        swaylock))

(define hyprland-packages
  (list hyprland
        hyprlock
        hypridle
        hyprcursor
        xdg-desktop-portal-hyprland))

(define font-packages
  (list fontconfig
        font-google-roboto
        font-google-noto-emoji
        font-recursive
        font-microsoft-cascadia
        font-victor-mono
        font-jetbrains-mono
        font-intel-one-mono
        font-adwaita
        font-liberation
        font-dejavu
        font-awesome
        font-fira-code
        font-google-noto))

;;;; Image
(define nonguix-install-amd
  (operating-system
    (host-name "usbgpgtool")
    (timezone "America/New_York")
    (locale "en_US.UTF-8")

    (keyboard-layout %kharis-shell-keyboard)

    ;; to install on a system with just BIOS (e.g. a VM)
    (bootloader (bootloader-configuration
                 (bootloader grub-efi-bootloader)
                 (targets "/dev/sda")))
    (file-systems (cons (file-system
                          (device (file-system-label "usb-gpg-disk"))
                          (mount-point "/")
                          (type "btrfs")
                          (flags '(no-atime))
                          (options "space_cache=v2"))
                        %base-file-systems))

    ;; NONFREE
    (kernel linux)
    ;; linux-firmware contains everything anyways (amd-microcode and
    ;; amdgpu-firmware are redundant)
    ;; https://gitlab.com/nonguix/nonguix/-/issues/327
    (firmware (cons* ;; linux-firmware
               amd-microcode
               amdgpu-firmware
               realtek-firmware
               %base-firmware))

    (kernel-arguments '("modprobe.blacklist=radeon"
                        ;; "quiet" ;; .....
                        ;; "net.iframes=0"
                        ))

    ;; TODO: users/groups (autologin to tty

    (groups %system-groups)
    (users (append (list
                    (user-account
                     (uid 1000)
                     (name "dc")
                     (password (crypt "dc1234321" "$6$abc"))
                     (comment "Default User")
                     (group "dc")
                     (supplementary-groups %my-groups)))
                   %base-user-accounts))

    ;; misc packages:
    ;; f3: test flash storage
    ;; paperkey: print keys to paper
    ;; certdata2pem: convert between cert formats
    ;; datefudge: mock system time to set arbitrary cert start times
    ;; exfat-utils: work with FAT disks
    ;; pwsafe: manage passwords

    (packages
     (append

      %ugt-packages-cli
      %ugt-packages-net
      %ugt-packages-net-plus
      %ugt-packages-hardware
      %ugt-packages-age
      %ugt-packages-tls
      %ugt-packages-smartcard
      %ugt-packages-yubikey
      %ugt-packages-step
      %ugt-packages-gnupg
      %ugt-packages-secrets
      %ugt-packages-tpm

      emacs
      %ugt-packages-emacs

      wayland-packages
      sway-packages
      hyprland-packages
      font-packages

      (list
       zerotier)

      %base-packages))

    (services
     (append (list
              (service pcscd-service-type)
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
                         `(("dc"
                            ,(local-file
                              (string-append (getenv "HOME")
                                             "/.ssh/authorized_keys")))))))

              ;; testing removing the fido2 functionality to restore yubikey
              (udev-rules-service 'fido2 libfido2 #:groups '("plugdev"))
              (udev-rules-service 'u2f libu2f-host #:groups '("plugdev"))
              (udev-rules-service 'yubikey yubikey-personalization))

             ;; dbus complains about the name of plasma's notification
             ;;
             ;; service "should've been named
             ;; 'org.freedesktop.Notifications' or something
             ;;
             ;; (service plasma-desktop-service-type)
             (list (service gnome-desktop-service-type))

             (modify-services %desktop-services
               (gdm-service-type
                config => (gdm-configuration
                           (inherit config)
                           (wayland? #t)))
               (guix-service-type
                config => (guix-configuration
                           (inherit config)
                           (channels %my-channels)
                           (guix (guix-for-channels %my-channels))
                           (substitute-urls
                            (append (list "https://substitutes.nonguix.org")
                                    %default-substitute-urls))
                           (authorized-keys
                            (append
                             (list
                              (plain-file "nonguix.pub"
                                          "(public-key (ecc (curve Ed25519) (q #C1FD53E5D4CE971933EC50C9F307AE2171A2D3B52C804642A7A35F84F3A4EA98#)))"))
                             %default-authorized-guix-keys)))))))))

;; TODO: add gnupg service if configuration file is in place

nonguix-install-amd
