;;* Copyright
;;; Copyright © 2019 Alex Griffin <a@ajgrf.com>
;;; Copyright © 2019 Pierre Neidhardt <mail@ambrevar.xyz>
;;; Copyright © 2019 David Wilson <david@daviwil.com>
;;; Copyright © 2021 David Conner
;;;
;;; This program is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation, either version 3 of the License, or
;;; (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;* Description
;; Generate a bootable image (e.g. for USB sticks, etc.) with:
;; $ guix system disk-image nongnu/system/install.scm

;;*= Module usb-install
(define-module (usb-install-nonfree)
  #:use-module (gnu)
  #:use-module (gnu system)
  #:use-module (gnu system install)

  #:use-module (gnu packages version-control)
  #:use-module (gnu packages vim)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages emacs)
  #:use-module (gnu packages emacs-xyz)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages mtools)
  #:use-module (gnu packages package-management)
  #:use-module (gnu packages file-systems)
  
  #:use-module (nongnu packages linux)
  #:use-module (nongnu system linux-initrd)
  #:use-module (nongnu system install)
  
  #:export (dc/guix-iso))

(use-service-modules networking ssh)
(use-package-modules certs shells linux)

(define isoguix-home
  (getenv "ISOGUIX_HOME"))

(define isoguix-authorized-keys
  `(("root" ,(local-file (string-concatenate (list isoguix-home "/.ssh/isoguix.pub")))))
  )

(define isoguix-openssh-extra-content
  (local-file (string-concatenate (list isoguix-home "/.ssh/sshd_config_extra"))))

(define %iso-services
  (modify-services
   %base-services
   
   (guix-service-type
    config => (guix-configuration
	       (inherit config)
	       (extra-options '("-c6"))
	       (substitute-urls
		(append (list "https://substitutes.nonguix.org")
			%default-substitute-urls))
	       (authorized-keys
		(append (list (local-file "/etc/guix/nonguix.pub"))
			%default-authorized-guix-keys))))

   )) 
  
;;** installation-os-nonfree
(define dc/guix-iso
  (operating-system
   (inherit installation-os)
   
   (host-name "eerse")
   (timezone "America/New_York")
   (locale "en_US.utf8")

   (keyboard-layout
    (keyboard-layout "us" "altgr-intl"
                     #:model "pc105"
                     #:options '("caps:escape")))   
   
   (kernel linux)

   ;; FAILS? INITRD
   ;; (initrd microcode-initrd)

   (firmware (list linux-firmware))
   ;; (firmware (cons* linux-firmware %base-firmware))

   ;; (kernel-arguments '("quiet" "modprobe.blacklist=radeon" "net.iframes=0"))
   
   (packages
    (append (list exfat-utils
                  fuse-exfat
                  git
                  curl
                  stow
                  vim
		  lvm2

		  emacs-no-x-toolkit
		  emacs-better-defaults
		  emacs-auto-complete
		  emacs-hydra
		  emacs-modus-themes
		  emacs-dash
		  emacs-lispy
		  emacs-geiser
		  emacs-geiser-guile
		  emacs-guix
		  emacs-yasnippet
		  emacs-yasnippet-snippets)
            (operating-system-packages installation-os)))

   (services (cons*

	      (service openssh-service-type
		       (openssh-configuration
			(port-number 2222)
			(password-authentication? #f)
			(permit-root-login 'prohibit-password)
			(authorized-keys isoguix-authorized-keys)
			(allow-agent-forwarding? #f)
			;; (allow-tcp-forwarding? #f)
			))
	      
	      ;; service network-manager-service-type
	      (service dhcp-client-service-type)
	      %iso-services)
	      
	     )
   ))

dc/guix-iso

;;; NOTE: this requires (gnu) and (srfi srfi-1) above, but will
;;; for some reason leave the installer without the cow-store service
    ;; (services
    ;;  (modify-services
    ;;      %base-services
    ;;    (guix-service-type config =>
    ;;                       (guix-configuration
    ;;                        (inherit config)
    ;;                        (substitute-urls
    ;;                         (append (list "https://substitutes.nonguix.org")
    ;;                                 %default-substitute-urls))
    ;;                        (authorized-keys
    ;;                         (append (list (local-file "../nonguix.pub"))
    ;;                                 %default-authorized-guix-keys))))
    ;;    ))
