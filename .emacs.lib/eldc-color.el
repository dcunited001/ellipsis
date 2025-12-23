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
              (a 255 :type number))

(defun eldc-color-hex-from-str (str)
  "oof this is terrible. just let the vec4's be vec4's"
  (unless (and (string-match-p "^#?[0-9A-Fa-f]+$" str)
               (or (eq (length str) 6)
                   (eq (length str) 8)))
    (user-error "probably give it valid input"))
  (let* ((str (or (and (string-match-p "^#" str) (substring 1 str)) str))
         (str (or (and (eq (length str) 6) (format "%sFF" str)) str))
         ;; ... damit
         ;; (idx (vector [:r 0 :g 2 :b 4 (when (> 8 (length str)) [:a 6])]))
         (color-args
          (mapcar (lambda (i2)
                    (or (and (keywordp i2) i2)
                        (string-to-number
                         (substring str i2 (+ 2 i2)) 16)))
                  [:r 0 :g 2 :b 4 :a 6])))
    (apply #'make-eldc-color-hex color-args)))

;; (eldc-color-hex-from-str "FFBBCCFB")

(cl-defstruct (eldc-color-hex (:include eldc-color-base)) "Hex color")
;; :constructor (eldc-color-hex-from-str)
(cl-defstruct (eldc-color-rgb (:include eldc-color-base (a 255))) "RGB color")
(cl-defstruct (eldc-color-rgba (:include eldc-color-base)) "RGBA color")

;; as is, not really a great usage of defgeneric, since it only prints its own
;; type. there's probably a better way, using &optional or struct conversion
(cl-defgeneric eldc-color-to-string (color) "Return the color's string representation"
               (string-trim (pp-to-string (vector (eldc-color-base-r color)
                                                  (eldc-color-base-g color)
                                                  (eldc-color-base-b color)
                                                  (eldc-color-base-a color)))))

(cl-defmethod eldc-color-to-string ((color eldc-color-hex))
  (format "#%s%s%s" (eldc-color-base-r color)
          (eldc-color-base-g color)
          (eldc-color-base-b color)))

;; (when (< a 255) (eldc-color-base-a color))

(let ((ccc (eldc-color-hex-to-rgb-vec "#FFAACCD0"))
      (hexcolor (eldc-color-hex "#FFAACCD0")))
  (eldc-color-to-string ccc))

;; (cl-defstruct eldc-color-float "Float color")

;; (make-eldc-color-base :r 1 :g 2 :b 3 :a 4)
;; (copy-eldc-color-base)

(defun eldc-color-css-invert-rgb (x)
  (format "%06X" (- #xFFFFFF x)))

(defun eldc-color-hex-to-rgb-vec (color)
  (cond-let*
    ([hex (string-match "\\`#[0-9a-fA-F]\\{8\\}" color)]
     [hexint (string-to-number (substring color 1) 16)]
     [rgbvec (vector
              (mod (/ hexint (expt 2 24)) (expt 2 8))
              (mod (/ hexint (expt 2 16)) (expt 2 8))
              (mod (/ hexint (expt 2 8)) (expt 2 8))
              (mod hexint (expt 2 8)))]
     (apply #'make-eldc-color-base (list :r (elt rgbvec 0) :g (elt rgbvec 1) :b (elt rgbvec 2) :a (elt rgbvec 3))))
    ([hex (string-match "\\`#[0-9a-fA-F]\\{6\\}" color)]
     [hexint (string-to-number (substring color 1) 16)]
     [rgbvec (vector
              (mod (/ hexint (expt 2 16)) (expt 2 8))
              (mod (/ hexint (expt 2 8)) (expt 2 8))
              (mod hexint (expt 2 8)))]
     (apply #'make-eldc-color-base (list :r (elt rgbvec 0) :g (elt rgbvec 1) :b (elt rgbvec 2))))))

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

;; (eldc-color-css-convert-hex "fdsa" :to "asdf" :from "asdf")

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
