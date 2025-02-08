;;; ../.dotfiles/.doom.d/scratch/dcscr-activities.el -*- lexical-binding: t; -*-

;; this doesn't seem to take affect
(defun dc/winconf-changed ()
  (lambda () (message "window configuration changed")))
(remove-hook 'window-configuration-change-hook #'dc/winconf-changed)

(defun dc/activities-in-tabs (&optional tabs)
  (let ((tabs (or tabs (funcall tab-bar-tabs-function))))
    (flatten-list (mapcar (lambda (tab) (alist-get 'activity tab)) tabs))))

(defun dc/tab-names (&optional tabs)
  (let ((tabs (or tabs (funcall tab-bar-tabs-function))))
    (mapcar (lambda (tab) (alist-get 'name tab)) tabs)))

;; (dc/activities--names-in-tabs)

;; (defun dc/tab-bar-titles (&optional tabs)
;;   (unless tabs (setq tabs (funcall tab-bar-tabs-function))))

(setq frame-title-format '("DOOM :: " (:eval (string-join (dc/tab-names) " // "))))
