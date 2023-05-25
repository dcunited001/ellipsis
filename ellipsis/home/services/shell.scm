(define-module (ellipsis home services shell)
  #:use-module (gnu home)
  #:use-module (srfi srfi-1)
  #:use-module (gnu packages)
  #:use-module (gnu services)
  #:use-module (gnu home services)
  #:use-module (gnu home services shells)
  #:use-module (guix gexp))



;; TODO: will color aliases affect tramp connectivity?

(define-public systemd-bash-configuration
  (home-bash-configuration
   (aliases '(("jctl" . "journalctl -p 3 -xb")
              ("jctlu" . "journalctl --user -u")
              ("sysu" . "systemctl --user")))))

(define-public term-bash-configuration
  (home-bash-configuration
   (aliases '(("shitbin" . "echo -e \"\\033c\"")))))

(define-public shell-bash-configuration
  (home-bash-configuration
   (aliases '(("diff" . "diff --color=auto")
              ("dir" . "dir --color=auto")
              ("egrep" . "egrep --color=auto")
              ("fgrep" . "fgrep --color=auto")

              ("hw" . "hwinfo --short")
              ("ls" . "ls --color=auto")
              ("pathtr" . "tr '\\'':'\\'' '\\''\\n'\\''")
              ("psmem" . "ps auxf | sort -nr -k 4")
              ("psmem10" . "ps auxf | sort -nr -k 4 | head -10")
              ("rip" . "expac --timefmt='\\''%Y-%m-%d %T'\\'' '\\''%l\\t%n %v'\\'' | sort | tail -200 | nl")

              ("screen" . "screen -h 2000")
              ("tarnow" . "tar -acf ")
              ("untar" . "tar -zxvf ")
              ("upd" . "/usr/bin/update")
              ("vdir" . "vdir --color=auto")
              ("wget" . "wget -c ")))))

;; ("emacs-debug-wayland" . "WAYLAND_DEBUG=1 emacs --fg-daemon > $HOME/.cache/log/emacs.wayland.`date +%Y-%m%d-%H%M`.log 2>&1")
