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
  (unless (or (and (string-match-p "^#[0-9A-Fa-f]+$" str)
                   (memq (length str) '(7 9)))
              (memq (length str) '(6 8)))
    (user-error "probably give it valid input"))
  (let* ((str (or (and (string-match-p "^#" str) (substring str 1)) str))
         (str (or (and (eq (length str) 6) (format "%sFF" str)) str))
         ;; ... damit
         ;; (idx (vector [:r 0 :g 2 :b 4 (when (> 8 (length str)) [:a 6])]))
         (color-args
          (mapcar (lambda (i2)
                    (or (and (keywordp i2) i2)
                        (string-to-number
                         (substring str i2 (+ 2 i2)) 16)))
                  [:r 0 :g 2 :b 4 :a 6])))
    (apply #'make-eldc-color-base color-args)))

;; (eldc-color-hex-from-str "FFBBCCFB")

(cl-defstruct (eldc-color-hex (:include eldc-color-base)) "Hex color")
;; :constructor (eldc-color-hex-from-str)
(cl-defstruct (eldc-color-rgb (:include eldc-color-base (a 255))) "RGB color")
(cl-defstruct (eldc-color-rgba (:include eldc-color-base)) "RGBA color")

;; (make-eldc-color-base :r 1 :g 2 :b 3 :a 4)
;; (copy-eldc-color-base)

;; as is, not really a great usage of defgeneric, since it only prints its own
;; type. there's probably a better way, using &optional or struct conversion

;; when (color &optional alpha), confused about how to include that in a
;; defmethod's closure
(cl-defgeneric eldc-color-to-string (color) "Return the color's string representation"
               (string-trim (pp-to-string (vector (eldc-color-base-r color)
                                                  (eldc-color-base-g color)
                                                  (eldc-color-base-b color)
                                                  (eldc-color-base-a color)))))

;; NOTE: cl-defmethod is defmacro that calls cl-defun
;; 
;; :extra "arbitrary-string?-hex
;; :extra "arbitrary-string?-rgb
;; :extra "arbitrary-string?-rgba

(cl-defmethod eldc-color-to-string :around ((color eldc-color-hex))
  (format "#%s%s%s%s"
          (eldc-color-hex-r color)
          (eldc-color-hex-g color)
          (eldc-color-hex-b color)
          (eldc-color-hex-a color)))

(cl-defmethod eldc-color-to-string :around ((color eldc-color-rgb))
  (format "rgb(%s, %s, %s)"
          (eldc-color-rgb-r color)
          (eldc-color-rgb-g color)
          (eldc-color-rgb-b color)))

(cl-defmethod eldc-color-to-string :around ((color eldc-color-rgba))
  (pp color)
  (format "rgba(%s, %s, %s, %s)"
          (eldc-color-rgba-r color)
          (eldc-color-rgba-g color)
          (eldc-color-rgba-b color)
          (eldc-color-rgba-a color)))

(defun eldc-color-to-list (color)
  (mapcar (lambda (sl)
            (cl-struct-slot-value (type-of color) sl color))
          '(r g b a)))

;; ((ct (color-struct-type (type-of color))))
;; (cl-struct-slot-value color-struct-type 'a color)

;; (when (< a 255) (eldc-color-base-a color))
(defvar -eldc-color-test nil)
(when -eldc-color-test
  (let* ((color (eldc-color-hex-from-str "#123456" )) ; #s(eldc-color-hex 18 52 86 255)
         (color-struct-type (type-of color)))
    (list :raw (cl-struct-slot-value color-struct-type 'a color)
          :hex (eldc-color-hex-a color)
          :base (eldc-color-base-a color)))

  (let ((colors (list "#FFAACC" "#FFAACCD0")))
    (->> colors
         (mapcar #'eldc-color-hex-from-str)

         ;; make-eldc-color-hex... /was/ type-specific in the apply from
         ;; eldc-color-to-string. idk, likely problematic design
         (mapcar #'eldc-color-to-string))))

(defun eldc-color-css-invert-rgb (x)
  (format "%06X" (- #xFFFFFF x)))

(defun eldc-color-hex-str-to-rgb-vec (str)
  "Requires the hash sign"
  (cond-let*
    ([hex (string-match "\\`#?[0-9a-fA-F]\\{8\\}" str)]
     [hexint (string-to-number (substring str -8) 16)]
     [rgbvec (vector
              (mod (/ hexint (expt 2 24)) (expt 2 8))
              (mod (/ hexint (expt 2 16)) (expt 2 8))
              (mod (/ hexint (expt 2 8)) (expt 2 8))
              (mod hexint (expt 2 8)))]
     (apply #'make-eldc-color-rgb (list :r (elt rgbvec 0) :g (elt rgbvec 1) :b (elt rgbvec 2) :a (elt rgbvec 3))))
    ([hex (string-match "\\`#?[0-9a-fA-F]\\{6\\}" str)]
     [hexint (string-to-number (substring str -6) 16)]
     [rgbvec (vector
              (mod (/ hexint (expt 2 16)) (expt 2 8))
              (mod (/ hexint (expt 2 8)) (expt 2 8))
              (mod hexint (expt 2 8)))]
     (apply #'make-eldc-color-rgba (list :r (elt rgbvec 0) :g (elt rgbvec 1) :b (elt rgbvec 2))))))

(when -eldc-color-test
  (->> (list "#FFAACC" "#FFAACCD0")
       (mapcar #'eldc-color-hex-str-to-rgb-vec)))
;; => (#s(eldc-color-rgba 255 170 204 255) #s(eldc-color-rgb 13 204 170 255))

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

(when -eldc-color-test
  (progn
    ;; eval with lispy
    (setq-local -fdsa '((1 10)))
    ;; repeatedly prepend with `(,(mod time 60) ,(random))
    (push (list (ts-sec (ts-now)) (random 120)) -fdsa)
    ;; repeatedly (* 10 (cdr (car -fdsa))) & set
    (setf (cadar -fdsa) (* 10 (cadar -fdsa)))))

;; -fdsa => ((42 8) (32 4500) (20 32000) (20 93) (19 27) (19 110) (1 10))

;; #123456
;; #87654321
;; #FFF12321
;; #32123FFF
;; 2112CDBA

(when -eldc-color-test
  (eldc-color-regexp-replace-colors
   :buffer (current-buffer)
   :regexp "[[:xdigit:]]\\{8\\}"
   :with-match (lambda (s) (string-reverse s)))

  ;; NOTE: still doesn't work bc i'm mis-interpreting etc, learning
  ;; malinformation & malexperience. i need
  ;; clarification.......... early. otherwise learn bad
  ;;
  ;; I put a ton of time into learning this defmethod stuff several times in
  ;; the past 3 years... only to NEVER actually write code to reinforce what
  ;; i've learned.
  (eldc-color-regexp-replace-colors
   :buffer (current-buffer)
   :regexp "[[:xdigit:]]\\{8\\}"
   :with-match
   (lambda (s)
     ;; generics are not dispatching as expected (around should override AFAIK?)
     ;; 
     ;; (eldc-color-to-string
     ;;  (eldc-color-hex-str-to-rgb-vec (string-reverse s)))
     (let* ((this-color (eldc-color-hex-str-to-rgb-vec (string-reverse s)))
            (fdsa (eldc-color-to-list this-color)))
       (apply #'format "rgba(%s, %s, %s, %s)" fdsa)))))

;; => 
;; (eldc-color-hex-str-to-rgb-vec "FFBBCCFB")

;; &key (buffer #'current-buffer) ; ... the `n+1' problem is expensive
;;
;; (cond ((functionp buffer) (funcall buffer))
;;       ((bufferp buffer) buffer)
;;       (t (current-buffer)))
;;
;; let* ((buffer (or buffer (current-buffer)))) ; unnecessarily complex closure

(cl-defun eldc-color-regexp-replace-colors
    (&key buffer (regexp "[[:xdigit:]]\\{6\\}") (with-match #'identity))
  "Find references to colors in buffer that match `regexp' and transform
them with `with-match'."
  (interactive)
  (let ((acc '()))
    (save-excursion
      (with-current-buffer
          (or buffer (current-buffer))
        (save-restriction
          (widen)
          (goto-char 1)
          (while (search-forward-regexp regexp nil t 1)
            ;; still not dynamic (only hex)
            (let* ((color (match-string 0))
                   (-fdsa- (pp color))
                   (result (funcall with-match color))
                   (result (eldc-color-to-string result)))
              ;; (setf (car acc) (caar acc))
              (push (list color result) acc)
              (replace-match result t nil color))))))
    (nreverse acc)))

(provide 'eldc-color)

;;; eldc-color.el
