;;; Copyright © 2025 David Conner <aionfork@gmail.com>

(define-module (dc home services)
  #:use-module (dc home common )
  #:use-module (dc home config)

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
            dc-zathura-service))

;;; Commentary:
;;;
;;; This module provides a service definition for the services service.
;;;
;;; Code:

;; =============================================
;;; Environments

;; basically, merge

(define (alist-append-uniq . rest)
  (fold
   (lambda (el li)
     (assoc-set! li (car el) (cdr el)))
   '() (apply append rest)))

;; (or (and (assoc)))
;; (acons (car el) (cdr el) li)

(define %dc-base-environment
  (alist-append-uniq

   wayland-environment
   gtk-environment))

;; =============================================
;;; Shells

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

(define dc-bash-configuration
  (home-bash-configuration
   ;; (aliases '())
   ;; TODO: colors.sh and prompt.sh (probably throw the prompt away)
   (environment-variables
    '(("RESTORE" . "$(echo -en '\001\033[0m\002')")
      ("RED" . "$(echo -en '\001\033[00;31m\002')")
      ("YELLOW" . "$(echo -en '\001\033[00;33m\002')")
      ("LGREEN" . "$(echo -en '\001\033[01;32m\002')")
      ("LYELLOW" . "$(echo -en '\001\033[01;33m\002')")
      ("LCYAN" . "$(echo -en '\001\033[01;36m\002')")))
   (bashrc (list (local-file (string-append %dotfiles-directory "/.bashrc") "bashrc")
                 (local-file (string-append %dotfiles-directory "/bash/rc/aliases.sh"))
                 (local-file (string-append %dotfiles-directory "/bash/rc/functions.sh"))
                 ;;                  "
                 ;; if [ \"$TERM\" = \"dumb\" ]; then
                 ;;   PS1='$ '
                 ;; else
                 ;;   PS1=\"${LYELLOW}\A ${LGREEN}\u${RED}@${LCYAN}\h ${RED}:: ${YELLOW}\w\"
                 ;;   PS1+=\"${RESTORE}\"
                 ;; fi
                 ;; "
                 ))
   (bash-profile (list (local-file (string-append %dotfiles-directory "/.bash_profile") "bash_profile")))
   (bash-logout (list (local-file (string-append %dotfiles-directory "/.bash_logout") "bash_logout")))))

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
           ,(local-file (string-append %files-directory "/.config/zathura/zathurarc"))))))

;; =============================================
;;; Diffoscope/Reprotest
;;
(use-package-modules diffoscope)
(define dc-diffoscope-packages (list diffoscope reprotest))

;; TODO diffoscope setup (req. service?)

;; =============================================
;;; Garuda Readline:
;;
;; - Guix System seems to draw it's inputrc bindings from readline defaults
;; - Garuda patches readline defaults with this:
;;
;; - both consistently use the \e escape syntax, but home-inputrc-service-type
;;   replaces this with "M-" meta syntax
;;
;; ---------------------------------------------
;;
;; # do not bell on tab-completion
;; # set bell-style none

;; set meta-flag on
;; set input-meta on
;; set convert-meta off
;; set output-meta on

;; $if mode=emacs

;; # for linux console and RH/Debian xterm
;; "\e[1~": beginning-of-line
;; "\e[4~": end-of-line
;; "\e[5~": beginning-of-history
;; "\e[6~": end-of-history
;; "\e[7~": beginning-of-line
;; "\e[3~": delete-char
;; "\e[2~": quoted-insert
;; "\e[5C": forward-word
;; "\e[5D": backward-word
;; "\e\e[C": forward-word
;; "\e\e[D": backward-word
;; "\e[1;5C": forward-word
;; "\e[1;5D": backward-word

;; # for rxvt
;; "\e[8~": end-of-line

;; # for non RH/Debian xterm, can't hurt for RH/DEbian xterm
;; "\eOH": beginning-of-line
;; "\eOF": end-of-line

;; # for freebsd console
;; "\e[H": beginning-of-line
;; "\e[F": end-of-line
;; $endif
