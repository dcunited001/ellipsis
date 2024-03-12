;; BSD 2-Clause License

;; Copyright (c) 2020, Artyom Bologov <aartaka@protonmail.com>
;; All rights reserved.

;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions are met:

;; 1. Redistributions of source code must retain the above copyright notice, this
;;    list of conditions and the following disclaimer.

;; 2. Redistributions in binary form must reproduce the above copyright notice,
;;    this list of conditions and the following disclaimer in the documentation
;;    and/or other materials provided with the distribution.

;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
;; AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
;; IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
;; DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
;; FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
;; DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
;; SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
;; CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
;; OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
;; OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

(in-package #:nyxt-user)

;; from https://github.com/aartaka/nyxt-config/blob/master/status.lisp

(define-configuration :status-buffer
                      "Display modes as short glyphs."
                      ((glyph-mode-presentation-p t)))

(define-configuration :force-https-mode ((glyph "ϕ")))
(define-configuration :user-script-mode ((glyph "u")))
(define-configuration :blocker-mode ((glyph "β")))
(define-configuration :proxy-mode ((glyph "π")))
(define-configuration :reduce-tracking-mode ((glyph "τ")))
(define-configuration :certificate-exception-mode ((glyph "χ")))
(define-configuration :style-mode ((glyph "ϕ")))
(define-configuration :cruise-control-mode ((glyph "σ")))

(define-configuration status-buffer
                      "Hide most of the status elements but URL and modes."
                      ((style (str:concat
                               %slot-value%
                               (theme:themed-css (theme *browser*)
	                                               `("#controls,#tabs"
	                                                 :display none !important))))))

(defmethod format-status-load-status ((status status-buffer))
           "A fancier load status."
           (spinneret:with-html-string
            (:span (if (and (current-buffer)
                            (web-buffer-p (current-buffer)))
                       (case (slot-value (current-buffer) 'nyxt::status)
                         (:unloaded "∅")
                         (:loading "∞")
                         (:finished ""))
                     ""))))
