(define-module (dc systems kharis)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 format)

  #:use-module (ellipsis home config))

;; not sure what and=> is, but accepts (value procedure)
;; and seems to return a default for the maybe? or Some<T> pattern
(define-public %home
  (and=> (getenv "HOME")
         (lambda (home)
           home)))

(define-public %_df
  (or (getenv "_df")
      (dc-default-path %home ".dotfiles")))

(define-public %_data
  (or (getenv "_data") "/data"))
(define-public %_lang
  (or (getenv "_lang")
      (dc-default-path %_data "lang")))
(define-public %_ecto
  (or (getenv "_ecto")
      (dc-default-path %_data "ecto")))
(define-public %_repo
  (or (getenv "_repo")
      (dc-default-path %_data "repo")))

(define-public %_flatpak
  (or (getenv "_flatpak") "/flatpak"))
(define-public %_steam
  (or (getenv "_steam")
      (dc-default-path %_flatpak "steam")))
(define-public %_agenda
  (or (getenv "_agenda")
      (dc-default-path %_flatpak  "agenda")))
(define-public %_wallpapers
  (or (getenv "_wallpapers")
      (dc-default-path %_data "xdg/Wallpapers/anime")))

(define-public %DOOMDIR
  (or (getenv "DOOMDIR")
      (dc-default-path ".doom.d")))
