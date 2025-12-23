;;; eldc-color.el -*- lexical-binding: t; -*-
;;
;; Copyright © 2025 David Conner
;;
;; Author: David Conner
;; Version: 0.0.1
;; Package-Requires: ((emacs "30.1"))
;; Keywords: lisp
;; 
;; This file is NOT part of GNU Emacs.
;; 
;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the “Software”), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:
;;
;; The above copyright notice and this permission notice shall be included in
;; all copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED “AS IS”, WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

;;; Commentary:

;;; Some common miscellaneous utilities designed to be shared between emacs
;;; profiles. 

;;; Code:

;; :constructor (...)
(cl-defstruct eldc-color-base "Abstract color"
              (r :type number)
              (g :type number)
              (b :type number)
              (a :type number))

(cl-defstruct eldc-color-hex "Abstract color"
              ;; :constructor (...)
              (r :type number)
              (g :type number)
              (b :type number)
              (a :type number))

;; (make-eldc-color-base :r 1 :g 2 :b 3 :a 4)
;; (copy-eldc-color-base)

(defun eldc-color-css-invert-rgb (x)
  (format "%06X" (- #xFFFFFF x)))

(defun eldc-color-css-convert-hex (color &rest keys)
  "Convert CSS hex to format"
  ;; plist allows multiple key entries
  (unless (plistp keys)
    (user-error "keys should be a plist"))
  (unless (or (floatp color) (integerp color) (stringp colorp))
    (user-error "color must be string, integer or float"))
  (let* ((to (or (plist-get keys :to) 'rgb))
         (from (or (plist-get keys :to) 'hex)))

    color ;; TODO: implement cl-defstruct & cl-defmethods
    ))

(eldc-color-css-convert-hex "fdsa" :to "asdf" :from "asdf")

;; convert #abc => #554433
;; (replace-regexp "\( +--.*\):#\(.\)\(.\)\(.\);$" "\1:#\2\2\3\3\4\4")

(defun eldc-color-css-invert-rgb-matches (&optional buffer)
  "replace matches in buffer, starting from current position... caveat
emptor: this code will change the hell out of every link/sha that even
resembles hex SMH"
  (interactive)
  (let ((regex "[[:xdigit:]]\\{6\\}")
        (acc))
    (save-excursion
      (with-current-buffer
          (or buffer (current-buffer))
        (save-restriction
          (widen)
          (goto-char 1)
          (while (search-forward-regexp regex nil t 1)
            (let* ((rgb (string-to-number (match-string 0) 16))
                   ;; (inv-rgb (format "%x" (invert-css-rgb rgb)))
                   (inv-rgb (eldc-color-css-invert-rgb rgb)))
              (push inv-rgb acc)
              (replace-match inv-rgb t))))))
    (nreverse acc)))

(provide 'eldc-color)

;;; eldc-color.el
