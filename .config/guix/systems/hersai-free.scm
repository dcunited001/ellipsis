;;* Module: hersai
(define-module (hersai-free)
  #:use-module (base-system-free)
  #:use-module (guix gexp)
  #:use-module (guix utils)
  #:use-module (gnu)
  #:use-module (gnu services)
  #:use-module (gnu services base)
  #:use-module (gnu services pm)
  #:use-module (gnu services virtualization)
  #:use-module (gnu services audio)
  #:use-module (gnu system)
  #:use-module (gnu system uuid)
  #:use-module (gnu system file-systems)
  #:use-module (gnu system mapped-devices)
  #:use-module (gnu packages firmware)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages xdisorg)

  ;;NONFREE
  ;;#:use-module (nongnu packages linux)
  )

;; (use-modules (base-system-free))
(use-service-modules desktop xorg)
(use-package-modules certs shells linux)

(define-public %hersai-desktop-packages
  (append (list
	   xf86-video-nouveau)
	  %dc-desktop-packages))

;;** desktop-services
;; TODO: docker is no longer added as a service by default
(define %hersai-desktop-services
  (modify-services
   dc-desktop-services

   (gdm-service-type config =>
                     (gdm-configuration
                      (inherit config)
                      (xorg-configuration
                       (xorg-configuration
                        (keyboard-layout %dc-default-shell-keyboard)
                        (modules (append (list xf86-input-wacom)
                                         %default-xorg-modules))
                        (extra-config (list %xorg-libinput-config)))
                       )
                      (default-user "dc")))

   ;; (guix-service-type config =>
   ;;                    (guix-configuration
   ;;                     (inherit config)
   ;;                     (substitute-urls
   ;;                      (append (list "https://substitutes.nonguix.org")
   ;;                              %default-substitute-urls))
   ;;                     (authorized-keys
   ;;                      (append (list (local-file "../nonguix.pub"))
   ;;                              %default-authorized-guix-keys))))

   ;; (fuse-desktop-type)
   ))

;;** operating-system
(define system
  (operating-system
   (inherit base-operating-system-free)
   (host-name "hersai")

   (kernel linux-libre)
   ;; (firmware %base-firmware) ;; includes openfwwf-firmware
   ;; NONFREE
   ;; (firmware (cons*
   ;; 	      linux-firmware
   ;; 	      %base-firmware))

   (mapped-devices
    (list (mapped-device
	   (source (uuid "a5281fe1-a2d1-44d7-bd6d-80e1e564510e"))
           (targets (list "hersaipv"))
           (type luks-device-mapping))

          (mapped-device
           (source "hersaivg")
           (targets (list "hersaivg-rootvol"
			  "hersaivg-swapvol"
		  "hersaivg-homevol"
			  "hersaivg-datavol"
			  "hersaivg-flatpakvol"))
           (type lvm-device-mapping))))

   (packages %hersai-desktop-packages)
   
   (services (cons*
	      
	      (service tlp-service-type
                       (tlp-configuration
                        (cpu-boost-on-ac? #t)
                        (tlp-default-mode "AC") ;; this is the default
                        (wifi-pwr-on-bat? #t)))

	      ;; (pam-limits-service ;; This enables JACK to enter realtime mode
              ;;  (list
              ;;   (pam-limits-entry "@realtime" 'both 'rtprio 99)
              ;;   (pam-limits-entry "@realtime" 'both 'memlock 'unlimited)))

	      (extra-special-file "/usr/bin/env"
				  (file-append coreutils "/bin/env"))

	      (service thermald-service-type)

	      ;; (service docker-service-type)
	      
	      (service libvirt-service-type ;; TODO how is libvirt configured?
                       (libvirt-configuration
                        (unix-sock-group "libvirt")
		        (tls-port "16555")))

	      (udev-rules-service 'pipewire-add-udev-rules pipewire)
	      
              (bluetooth-service #:auto-enable? #t)

	      %hersai-desktop-services))

   (groups (append (list (user-group (name "julia") (system? #t)))
                   %dc-groups))
    
   (users (append (list (dc-user %dc-my-groups)
                        (user-account
                         (name "julia")
                         (group "julia")
                         (system? #t)
                         (comment "Julia User")
                         (home-directory "/home/jovyan")
                         (shell (file-append shadow "/sbin/nologin"))))
                  %dc-users
                  %base-user-accounts))
   
   (file-systems (cons*

                  (file-system
                   (device (file-system-label "hersaiRoot"))
                   (mount-point "/")
                   (type "btrfs")
		   (flags '(no-atime))
		   (options "space_cache=v2")
                   (needed-for-boot? #t)
                   (dependencies mapped-devices))

                  (file-system
                   (device (file-system-label "hersaiHome"))
                   (mount-point "/home")
                   (type "ext4")
                   (needed-for-boot? #f)
                   (dependencies mapped-devices))

                  (file-system
                   (device (file-system-label "hersaiData"))
                   (mount-point "/data")
                   (type "ext4")
                   (needed-for-boot? #f)
                   (dependencies mapped-devices))

                  (file-system
                   (device (file-system-label "hersaiFlatpak"))
                   (mount-point "/flatpak")
                   (type "ext4")
                   (needed-for-boot? #f)
                   (dependencies mapped-devices))

                  ;; /boot/efi needs to be enumerated here
                  ;;   in addition to the (bootloader...) declaration
                  (file-system
                   (device "/dev/sda1")
                   (mount-point "/boot/efi")
                   (type "vfat"))
		  
                  %base-file-systems))


   (swap-devices (list (file-system-label "hersaiSwap")))))

system
