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
(define-module (nongnu system usb-install)
  #:use-module (gnu system)
  #:use-module (gnu system install)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages vim)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages emacs)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages mtools)
  #:use-module (gnu packages package-management)
  #:use-module (gnu packages file-systems)
  #:use-module (nongnu packages linux)
  #:export (installation-os-nonfree))

;;** installation-os-nonfree
(define installation-os-nonfree
  (operating-system
   (inherit installation-os)
   (kernel linux)
   (firmware (list linux-firmware))

   (kernel-arguments '("quiet" "modprobe.blacklist=radeon" "net.iframes=0"))

   (packages
    (append (list exfat-utils
                  fuse-exfat
                  git
                  curl
                  stow
                  vim
                  emacs-no-x-toolkit)
            (operating-system-packages installation-os)))))

    installation-os-nonfree
