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
            dc-home-systemd-aliases-service
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
      (("pathtr" . "tr \":\" \"\n\"")
       ("shitbin" . "echo -e \"\\033c\"")
       ;; env+via =~ env+path ... get it?
       ("envia" . "grep -e '^[A-Za-z0-9]*PATH=' | sort | uniq")
       ("covia" . "sed -e 's/:/\n:\t/g'") ;; `env | envia | covia'
       ("nenvia" . "grep -ve '^[A-Za-z0-9_]*PATH=' | sort | uniq")
       ;; diff environments: `someother_env | diffia`
       ;; ... just get the env there (mktemp, etc)
       ("diffia" . "envia | covia | diff - <(env | envia | covia)")
       ("difnia" . "nenvia | diff - <(env | nenvia)")))
     (bash .
           (("bashbinds"
             . "bind -p | grep -ve '^#' | tr -d ':\"' | sed -e 's/\\\\e/M /g' | sed -e 's/\\\\C-/C /g'  | grep -v 'self-insert' | sort -k2")
            ("bashbindtree"
             . "bind -P | cut -f1 -d' ' | tr '_' '/' | tree --noreport --fromfile .")))
     (defaultcmd .
       (("wget" . "wget -c ")))
     ;; emacs, info-standalone
     (emacs .
            (("imacs"
              . "emacs -f info-standalone --eval=\"(load-theme (intern \\\"wombat\\\"))\"")))
     (hypr . (("hcx" . "hyprctl")
              ("hcbinds"
               . "hyprctl binds | tr -d '\n' | sed -e 's/bind/\nbind/g'")
              ("hcbdoops"
               . "hcbinds | sort | cut -f2-5 | uniq -c | grep -vE '^\s+1'")))
     ;; (hyprdc . (("hbkey" "for i in (0 32 33 36 37  64 65 68 69 )")
     ;;            ("hbcode" "hyprctl")))
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
                  ("diffusb" . "diff <(lsusb) <(sleep 3; lsusb)")
                  ("iotopa" . "iotop -oa")
                  ("grubup" . "sudo update-grub")))
     (disk .  (("dfs" . "df -h | tail -n+2 | sort -nk5")
               ("dfsh" . "df -sh | tail -n+2 | sort -nk5")))
     (archive . (("tarnow" . "tar -acf ")
                 ("untar" . "tar -zxvf ")))
     ;; alias renoice="renice --priority 15 $(pgrep emacs-29)"
     (process
      . (("ps_nice"
          . "ps -o pid,ppid,uname,cls,nice,pri,rtprio,comm,args --sort=-nice")
         ("ps_pri" . "ps -eo pri k +pri h | uniq -c")
         ("psid" . "ps -opid,uid,command h")
         ("psmem" . "ps auxf | sort -nr -k 4")
         ("psmem10" . "ps auxf | sort -nr -k 4 | head -10")
         ("psnice"
          . "ps axo pid,ppid,uname,cls,nice,pri,rtprio,comm,args --sort=-nice")
         ("pspri"
          . "ps -eo pid,tid,class,rtprio,ni,pri,psr,pcpu,stat,wchan:14,comm k pri")
         ("psz" . "ps axo s,pid,ppid,uname,comm,args | grep -e'^Z'")
         ("ptrgb" . "pstree -C age -pT")))
     (tree . (("treef" . "tree --prune -aP")))
     (jq . (("jqr" . "jq -r ")
            ("jqrj" . "jq -rj ")))
     (repl . (("sqli" . "rlwrap sqlite3")
              ("gxpl" . "rlwrap guix repl")))
     (node . (("nodenpm_lsparse"
               . "npm ls -g --parseable | grep node_modules | sed -e '\\''s/.*node_modules\\///g'\\''")))
     (html . (("tyxy"
               . "tidy --quiet yes --tidy-mark no --vertical-space yes -indent -xml"))))))

;; wich => cat `which fdsa` ;; can't bc quote
;; fd '.*\.desktop' $(echo $XDG_DATA_DIRS | pathtr) 2>/dev/null | tree --fromfile .
;; locate "/data/org/roam/*org" | wordcat => 963,372
;; locate "/data/dev/texelio/*md" | wordcat => 996,169

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
  '(("jctl" . "isd")
    ("jour" . "isd")
    ("jctlu" . "journalctl --user")
    ("jctlu7" . "journalctl --user -p7")
    ("jctlb" . "journalctl -xb")
    ("jctlb7" . "journalctl -xb -p7")
    ("systat" . "systemctl status")
    ("sycat" . "SYSTEMD_COLORS=256 systemctl cat --no-pager")
    ("sysu" . "isd")
    ("syu" . "systemctl --user")
    ("syustat" . "systemctl --user status")
    ("syucat" . "SYSTEMD_COLORS=256 systemctl --user cat --no-pager")
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

;; (local-file (string-append %files-directory "/.bashrc") "bashrc")
;; (local-file (string-append %files-directory "/bash/rc/colors.sh"))
;; (local-file (string-append %files-directory "/bash/rc/aliases.sh"))
;; (local-file (string-append %files-directory "/bash/rc/functions.sh"))
;; ;; (local-file (string-append %files-directory "/bash/rc/completions.sh"))
;; (local-file (string-append %files-directory "/bash/rc/git-prompt.sh"))
;; (local-file (string-append %files-directory "/bash/rc/prompt.sh"))

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
