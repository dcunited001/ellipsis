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
  #:export (dc-inputrc-configuration
            dc-shell-profile-configuration
            dc-bash-configuration
            dc-zathura-service))

;;; Commentary:
;;;
;;; This module provides a service definition for the services service.
;;;
;;; Code:

;; =============================================
;;; Shells
;;;

(define add-shell-aliases
  (fold
   (lambda (el acc) (append acc (cdr el)))
   '()
   '((shell .
      (("pathtr" . "tr '\\'':'\\'' '\\''\\n'\\''")
       ("shitbin" . "echo -e \"\\033c\"")))
     (defaultcmd .
       (("wget" . "wget -c ")))
     ;; emacs, info-standalone
     (emacs .
            (("imacs"
              . "emacs -f info-standalone --eval=\"(load-theme (intern \\\"wombat\\\"))\"")))
     (text  .
            (("wordcat" . "tee >(xargs -n1 cat) | wc -w")))
     (git . (("gkcfg" . "git stack --dump-config -")
             ("gkg" . "git stack")
             ("gksy" . "git stack sync")))
     (gnupg .
            (("gpga" . "gpg --armor")
             ("gpgk" . "gpg-connect-agent killagent /bye")
             ("gpguptty" . "gpg-connect-agent updatestartuptty /bye")
             ("gpgrel" . "gpg-connect-agent reloadagent /bye")))
     (hardware . (("hw" . "hwinfo --short")
                  ("iotopa" . "iotop -oa")
                  ("grubup" . "sudo update-grub")))
     (disk .  (("dfs" . "df -h | tail -n+2 | sort -nk5")
               ("dfsh" . "df -sh | tail -n+2 | sort -nk5")))
     (archive . (("tarnow" . "tar -acf ")
                 ("untar" . "tar -zxvf ")))
     ;; alias renoice="renice --priority 15 $(pgrep emacs-29)"
     (process . (("ps_nice" . "ps axo pid,comm,nice,cls --sort=-nice")
                 ("ps_pri" . "ps -eo pri k +pri h | uniq -c")
                 ("psid" . "ps -opid,uid,command h")
                 ("psmem" . "ps auxf | sort -nr -k 4")
                 ("psmem10" . "ps auxf | sort -nr -k 4 | head -10")
                 ("psnice" . "ps -o pid,comm,nice")
                 ("pspri"
                  . "ps -eo pid,tid,class,rtprio,ni,pri,psr,pcpu,stat,wchan:14,comm k pri")
                 ("ptrgb" . "pstree -C age -pT")))
     (tree . (("treef" . "tree --prune -aP")))
     (jq . (("jqr" . "jq -r ")
            ("jqrj" . "jq -rj ")))
     (node . (("nodenpm_lsparse"
               . "npm ls -g --parseable | grep node_modules | sed -e '\\''s/.*node_modules\\///g'\\''")))
     (html . (("tyxy"
               . "tidy --quiet yes --tidy-mark no --vertical-space yes -indent -xml"))))))

;; ???
;; ("vdir" . "vdir --color=auto")

(define dc-home-shell-aliases-service
  (simple-service 'dc-home-bash-aliases
                  home-bash-service-type
                  (home-bash-extension
                   (aliases add-shell-aliases))))

;; sysu cat doom
;; sysu show -p Type $doom
;; sysu show -vp Type $doom # only values
;; alias sysed='systemctl --user edit --drop-in=$overridename $svc'

(define add-systemd-aliases
  '(("jctlu" . "journalctl --user -u")
    ("jctlb" . "journalctl -p 3 -xb")
    ("sysu" . "systemctl --user")
    ("sysdpath" . "systemd-path system-shared")
    ("sysupath" . "systemd-path user-shared")))

(define dc-home-systemd-aliases-service
  (simple-service 'dc-home-bash-aliases
                  home-bash-service-type
                  (home-bash-extension
                   (aliases add-systemd-aliases))))

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
  (plain-file "bashrc_noninteractive_return"
              "[[ $- != *i* ]] && return"))

;; Generic bash configuration

(define dc-bash-configuration
  (home-bash-configuration
   (aliases add-shell-aliases)
   (bashrc
    (list
     ;; needs to be sourced first
     dc-bashrc-noninteractive-return))
   (bash-profile
    (list
     (plain-file "bash_profile" "\
if [ -f ~/.profile ] ; then source ~/.profile; fi
if [ -f ~/.bashrc ]    ; then source ~/.bashrc; fi")))
   (bash-logout
    (list
     (local-file (string-append %files-directory "/bash/bash_logout")
                 "bash_logout")))))

;; ---------------------------------------------
;;; Base Shell Services

;; (define %dc-base-shell-services
;;   (list (service home-bash-service-type dc-bash-configuration)
;;         (service home-inputrc-service-type dc-inputrc-configuration)))

(define dc-shell-profile-configuration
  (list (local-file (string-append %files-directory "/profile")
                    "profile")))

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

;; =============================================
;;; Diffoscope/Reprotest
;;
(use-package-modules diffoscope)
(define dc-diffoscope-packages (list diffoscope reprotest))

;; TODO diffoscope setup (req. service?)
