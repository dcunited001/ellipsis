(define-module (dc packages)
  #:use-module (dc channels)
  #:use-module (gnu packages)
  #:use-module (gnu packages package-management)
  #:use-module (gnu packages guile)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (guix git)
  #:use-module (guix utils)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix channels)
  #:use-module (guix build-system channel)
  #:use-module (srfi srfi-1)

  #:export (guix-from-my-channels
            channels-package))

(define (get-guix-channel channels)
  (car
   (filter (lambda (x) (equal? (channel-name x) 'guix)) channels)))

(define-public guix-from-my-channels
  (let ((commit (channel-commit (get-guix-channel my-channels))))
    (package
      (inherit guix)
      (version (string-append "1.4.0-" (string-take commit 7)))
      (source
       (git-checkout
        (url "https://codeberg.org/guix/guix-mirror.git")
        (commit commit)))
      (arguments
       (substitute-keyword-arguments (package-arguments guix)
         ((#:tests? _)
          #f)
         ((#:phases phases)
          #~(modify-phases #$phases (delete 'check)))
         ((#:configure-flags flags #~'())
          #~(append
             #$flags
             (list
              #$(string-append "--with-channel-commit=" commit))))))

      (inputs (modify-inputs (package-inputs guix)
                (replace "guile" guile-next))))))

(define-public (guix-for-channels channels)
  "Return a package corresponding to CHANNELS."
  (package
    (inherit guix-from-my-channels)
    (source (find guix-channel? channels))
    (build-system channel-build-system)
    (arguments
     `(#:channels ,(remove guix-channel? channels)))
    (inputs '())
    (native-inputs '())
    (propagated-inputs '())))

(define-public channels-package
  (guix-for-channels my-channels))
