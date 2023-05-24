(define-module (ellipsis home config)
  #:use-module (srfi srfi-1)
  ;; #:use-module (srfi srfi-9)
  #:use-module (guix records)

  #:export (el-cfg
            el-cfg?
            make-el-cfg))

;; TODO extend records with dynamic accessors
;; TODO refactor config from dotfiles into records

;; records: srfi-9?
(define-record-type* <el-cfg>
  el-cfg
  make-el-cfg
  el-cfg?

  (home el-cfg-home)
  (df el-cfg-df)
  (data el-cfg-data)
  (lang el-cfg-lang)
  (ecto el-cfg-ecto)
  (repo el-cfg-repo)
  (flatpak el-cfg-flatpak)
  (wallpapers el-cfg-wallpapers))

;; for hardware
(define-record-type* <el-pc>
  el-pc
  make-el-pc
  el-pc?

  )

;; for display
(define-record-type* <el-ui>
  el-ui
  make-el-ui
  el-ui?

  )


(define-public (dc-default-path . components)
  (let ((thats-not-canon
         (case (length components)
           ((0) "/")
           ((1) (first components))
           (else (string-join components "/")))))
    (canonicalize-path thats-not-canon)))
