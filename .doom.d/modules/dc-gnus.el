;;; gnus.el -*- lexical-binding: t; -*-
;;
;; Copyright © 2025 David Conner
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

;;; Gnus

;;;; Startup
(setq gnus-read-newsrc-file nil
      gnus-save-newsrc-file t
      gnus-use-dribble-file t
      gnus-always-read-dribble-file t)

;;;; Async
(setq gnus-asynchronous t
      gnus-use-article-prefetch 20)

;;;; Agent

;;;; Article
(setq gnus-article-over-scroll nil
      gnus-article-show-cursor t
      gnus-html-frame-width 80
      gnus-html-image-automatic-caching t
      ;; gnus-inhibit-images t
      gnus-max-image-proportion 0.7
      ;; gnus-treat-display-smileys nil
      ;; gnus-article-mode-line-format "%G %S %m"

      ;; all images in headers are outright annoying---disabled!
      gnus-article-x-face-too-ugly ".*")

;;;; Group

(setq gnus-group-line-format "%M%B%p│%-42,42c│%3T│%4I│%7y│%L%S %d\n"
      gnus-group-uncollapsed-levels 3
      gnus-level-subscribed 6
      gnus-level-default-subscribed 4)

;;;;; Topics

;; "%i└─♦ %(%{%n%}%) %gg:%a (%G:%A) ]%v\n"
;; "%i╚═♦ %(%{%n%}%) »───« %a in %g »───« %G in %A »───♦  %v\n"

;; needs too many hacks, may not work
;;
;; dc/gnus-topic-indent-max (* 5 gnus-topic-indent-level)
;;
;; (concat "%i└─•─"
;;         "%4,4~(form (make-string dc/gnus-topic-indent-max (string-to-char \"─\")))@"
;;         "« %(%{%n%}%) »───« %G in %A »───♦  %v\n")
(setq gnus-topic-indent-level 2
      gnus-topic-line-format
      (concat "%i└─•────« %(%{%n%}%) »───« %G%v »───♦ %-56=%7A \n"))

;;;; Server

;; the %h formatter needs type conversion to set the length
(setq gnus-server-line-format "%-8,8s │ %-8,8a │ %-14,14c | %-16,16w %h\n")
;; (dc/gnus-format-alist-translate gnus-server-line-format-alist)

;;;; Summary

(setq gnus-summary-line-format "%U%R%z %d %B%-3L %[%f%] %s\n"
      gnus-sum-thread-tree-root            "☼──► "
      gnus-sum-thread-tree-false-root      "○┬─► "
      gnus-sum-thread-tree-vertical        "│"
      gnus-sum-thread-tree-leaf-with-other "├•─► "
      gnus-sum-thread-tree-single-leaf     "└•─► "
      gnus-sum-thread-tree-indent          " "
      gnus-sum-thread-tree-single-indent   "■    "
      gnus-summary-newsgroup-prefix        "⇒ "
      gnus-summary-to-prefix               "→ ")

;;;; Package

(use-package gnus
  :defer t
  :custom
  (gnus-select-method (nntp "news.gmane.io"))
  ;; (gnus-select-method '(nnimap "imap.gmail.com"))
  (gnus-large-newsgroup 4000)
  ;; (gnus-secondary-select-methods '((nntp "news.gmane.io")))
  ;; TODO: add (nnimap "imap.gmail.com")
  ;; TODO: add nnrss/nnatom?
  (gnus-message-archive-group "\"[Gmail]/Sent Mail\"")
  :hook
  ((gnus-group-mode-hook gnus-summary-update-hook gnus-browse-mode-hook) . (hl-line-mode)))

;;;; Interface

;; TODO Hydras for Gnus & Sending Messages

;;;;; Gnus Visual

;;;;; Window Layout

;; definitely, definitely use the trees
(setq gnus-use-trees t
      gnus-generate-tree-function 'gnus-generate-horizontal-tree)

;;;;; Gravatars
;; retro, identicon, robohash, wavatar, monsterid
;; set to "404" to disable
(setq gravatar-default-image "robohash")

(use-package gnus-gravatar
  :defer t
  :custom
  (gnus-treat-from-gravatar 'head)
  (gnus-treat-mail-gravatar 'head))

;;;;; Notifications

;;;; Daemons

;;;;; Support

(defun dc/gnus-demon-scan-mail-unless-closed (servers)
  (save-window-excursion
    (let (server
          (nnmail-fetched-sources (list t))
          (servers (seq-intersection
                    gnus-opened-servers servers
                    (lambda (a b) (eq (car a) (car b))))))
      (while (setq server (car (pop servers)))
        (and (gnus-check-backend-function 'request-scan (car server))
             (eq 'nnimap (car server))
             ;; exits early if the server is closed
             (not (eq 'closed (gnus-server-status server)))
             (message "Reading mail from %s" (pp-to-string server))
             (or (gnus-server-opened server)
                 (gnus-open-server server))
             (gnus-request-scan nil server))))))

;;;;; Handlers

(defun dc/gnus-demon-setup ()
  ;; close connections after emacs is idle for 30 minutes
  (gnus-demon-add-handler
   'gnus-demon-close-connections nil 30)

  ;; assuming the imap server is specified by gnus-select-method at
  ;; gnus-startup.
  (let ((nnimap-methods (list (list gnus-select-method))))

    ;; this still may not work
    (gnus-demon-add-handler
     (apply-partially #'dc/gnus-demon-scan-mail-unless-closed
                      nnimap-methods) 10 nil))

  (gnus-demon-init))

;;; Mail

(setq gnus-thread-hide-subtree t)

;; TODO configure mail/message settings for Gnus after comparing to Doom Settings


(provide 'dc-gnus)
