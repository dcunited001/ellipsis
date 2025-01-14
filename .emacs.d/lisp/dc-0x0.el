;;; dc-0x0.el --- 0x0 Functionality -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025 David Conner
;;
;; Author: David Conner <aionfork@gmail.com>
;; Maintainer: David Conner <aionfork@gmail.com>
;; Created: January 13, 2025
;; Modified: January 13, 2025
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex text tools unix vc wp
;; Homepage: https://github.com/dcunited001/dc-0x0
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  
;;
;;; Code:

(require 'cl-lib)

(defun dc/0x0-retention-policy ()
  "Change retention for 0x0 servers"

  ;; `(,tag ,@(mapcar plist-put cfg :max-age 7))
  ;; ok you can chain the setf

  ;; TODO make this accept a plist and merge
  (cl-loop for server in 0x0-servers
           with cfg with tag
           do (setf tag (car server)
                    cfg (cdr server)
                    cfg (plist-put cfg :max-age 7)
                    cfg (plist-put cfg :min-age 0))
           collect `(,tag ,@cfg)))

(provide 'dc-0x0)
;;; dc-0x0.el ends here
