;;; Copyright © 2025 David Conner <aionfork@gmail.com>

(define-module (dc home services)
  #:use-module (dc home common )
  #:use-module (dc home config)

  #:use-module (ellipsis utils)

  #:use-module (gnu home services shells)
  #:use-module (gnu home services)
  #:use-module (gnu packages)
  #:use-module (gnu services base)
  #:use-module (gnu services configuration)
  #:use-module (gnu services shepherd)
  #:use-module (gnu services)
  #:use-module (gnu)

  #:use-module (guix gexp)
  #:use-module (srfi srfi-1)
  #:export (%dc-base-environment
            dc-inputrc-configuration
            dc-bash-configuration
            dc-zathura-service
            dc-screenrc-service))

;;; Commentary:
;;;
;;; This module provides a service definition for the services service.
;;;
;;; Code:

(define %dc-base-environment
  (alist-append-uniq
   wayland-environment
   gtk-environment))

;; =============================================
;;; Shells

;; ---------------------------------------------
;;; .profile
(define dc-shell-profile-service
  (simple-service 'dc-shell-profile
                  home-shell-profile-service-type
                  (list )))

;; ---------------------------------------------
;;; Readline
;;
;; bind '"\e/":dabbrev-expand'
;; M-tab for `dynamic-complete-history` (currently \e\C-i)

(define dc-inputrc-configuration
  (home-inputrc-configuration
   ;; (conditional-constructs ...)
   ;; (extra-content ...)
   (key-bindings
    '(("C-@" . "set-mark")
      ;; ("Meta-\"" " . "set-mark")
      ("C-w" . "kill-region")
      ("M-w" . "copy-region-as-kill")))
   (variables
    ;; TODO: inputrc: determine whether colored-completion-prefix could
    ;; possibly interfere with tramp. Though it requires interactive
    '( ;; ("colored-completion-prefix" . #t)
      ("bell-style" . "visible")))))

;; ---------------------------------------------
;;; Bash

;; TODO: bat-service https://github.com/hiecaq/guix-config?tab=readme-ov-file#bat

(define dc-bashrc-noninteractive-return
  ;; should be first
  (plain-file "bashrc-noninteractive-return"
              "[[ $- != *i* ]] && return"))

(define dc-bash-configuration
  (home-bash-configuration
   ;; (aliases '())
   ;; TODO: colors.sh and prompt.sh (probably throw the prompt away)
   (bashrc
    (list
     dc-bashrc-noninteractive-return
     (local-file (string-append %files-directory "/.bashrc") "bashrc")
     (local-file (string-append %files-directory "/bash/rc/colors.sh"))
     (local-file (string-append %files-directory "/bash/rc/aliases.sh"))
     (local-file (string-append %files-directory "/bash/rc/functions.sh"))
     ;; (local-file (string-append %files-directory "/bash/rc/completions.sh"))
     (local-file (string-append %files-directory "/bash/rc/git-prompt.sh"))
     (local-file (string-append %files-directory "/bash/rc/prompt.sh"))))
   (bash-profile
    (list
     (local-file (string-append %files-directory "/.bash_profile")
                 "bash_profile")))
   (bash-logout
    (list
     (local-file (string-append %files-directory "/.bash_logout")
                 "bash_logout")))))

;; ---------------------------------------------
;;; Base Shell Services

;; (define %dc-base-shell-services
;;   (list (service home-bash-service-type dc-bash-configuration)
;;         (service home-inputrc-service-type dc-inputrc-configuration)))

;; =============================================
;;; Applications
;;

;; ---------------------------------------------
;;; Zathura
;;
;; TODO: HOME: zathura: after deciding on a PDF reader, extend XDG mime/desktop
(define dc-zathura-service
  (simple-service
   'dc-zathura-service
   home-xdg-configuration-files-service-type
   (list `("zathura/zathurarc"
           ,(local-file
             (string-append %files-directory "/.config/zathura/zathurarc"))))))

(define dc-screenrc-service
  (simple-service
   'dc-screenrc-service
   home-files-service-type
   (list `(".screenrc"
           ,(local-file (string-append %files-directory "/.screenrc") "screenrc")))))

;; =============================================
;;; Diffoscope/Reprotest
;;
(use-package-modules diffoscope)
(define dc-diffoscope-packages (list diffoscope reprotest))

;; TODO diffoscope setup (req. service?)
