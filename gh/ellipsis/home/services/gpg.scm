(define-module (ellipsis home services gpg)
  #:use-module (gnu home)
  #:use-module (srfi srfi-1)

  #:use-module (gnu packages)
  #:use-module (gnu packages gnupg)

  #:use-module (gnu services)

  #:use-module (gnu home services)
  #:use-module (gnu home services shells)

  #:use-module (guix gexp))

(define-public gnupg-bash-configuration
  (home-bash-configuration
   (aliases '(("gpga" . "gpg --armor")
              ("gpgk" . "gpg-connect-agent killagent /bye")
              ("gpgrel" . "gpg-connect-agent reloadagent /bye")
              ("gpguptty" . "gpg-connect-agent updatestartuptty /bye")
              ("grep" . "grep --color=auto")
              ("grubup" . "sudo update-grub")))))
