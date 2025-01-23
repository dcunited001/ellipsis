;;; dc-util.el --- Utilities Shared Across Emacsen -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025 David Conner
;;
;; Author: David Conner <aionfork@gmail.com>
;; Maintainer: David Conner <aionfork@gmail.com>
;; Created: January 19, 2025
;; Modified: January 19, 2025
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex text tools unix vc wp
;; Homepage: https://github.com/dcunited001/dc-util
;; Package-Requires: ((emacs "29.1") (org "9.7"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  This functionality is intended to defined some general support
;;  utilities. It is in the process of being migrated from the 'dc-support
;;  feature in dcunited001/.emacs.guix.
;;
;;  This feature must not invoke use-package or straight, but it does expect
;;  packages to be defined. Where dependencies exist, they should be stubbed
;;  out with `declare-function' and similar.
;;
;;  It probably doesn't require org 9.7, but whatever.
;;
;;; Code:

;;* System
;;** Identification

(defvar dw/is-guix-system (and (eq system-type 'gnu/linux)
                               (with-temp-buffer
                                 (insert-file-contents "/etc/os-release")
                                 (search-forward "ID=guix" nil t))
                               t))

;;* Emacs

;;** System Identification

;;** Templates

(declare-function org-file-contents "org")
(defun dc/read-org-template-from-file (file)
  (if (file-exists-p file) (org-file-contents file)
    (error "* Template file %s not found" file)))

;;** Interface

;;*** Learn a man one fish...

(defun dc/forcing-function (msg)
  "You so stupid! Nothing! Nothing in the box."
  (interactive)
  (user-error msg))

;;*** Toggle
;;**** Variables

;; NOTE: if parsing the body to extract bindings is necessary,
;; use macroexp-parse-body
(defmacro dc/toggleable-boolean (name &optional keybind)
  "Define an interactive defun to toggle the variable NAME
along with KEYBIND, if present. (keybind is probably broken)."
  (declare (indent defun))
  (let* ((symname (symbol-name (or (intern-soft name) (defvar name))))
         (toggle-name (format "dc/toggle-%s" symname))
         (toggle-docstring (format "Toggle variable: %s" symname))
         (toggle-sym (or (intern-soft toggle-name)
                         (intern toggle-name))))
    `(progn
       (defun ,toggle-sym ()
         (list ,toggle-docstring)
         (interactive)
         (setq-default ,name (not ,name)))
       ;; ,(if keybind `(map! ,keybind #',toggle-sym))
       )))

;;**** Eval Truncation

(defvar eval-expression-print-level-default eval-expression-print-level
  "The default `eval-expression-print-level' to restore.")
(defvar eval-expression-print-length-default eval-expression-print-length
  "The default `eval-expression-print-length' to restore.")
(defvar eval-level-toggle-truncation 'off
  "Whether `eval-level-toggle-truncation' has been enabled.")
(defvar eval-length-toggle-truncation 'off
  "Whether `eval-length-toggle-truncation' has been enabled.")

(defun dc/eval-length-toggle-truncation ()
  (interactive)
  ;; monoid
  (if (not (boundp 'eval-length-toggle-truncation))
      (setq-local eval-length-toggle-truncation 'off
                  eval-expression-print-length-default eval-expression-print-length
                  print-length-default print-length))

  (cond ((eq eval-length-toggle-truncation 'on)
         (setq-local eval-length-toggle-truncation 'off
                     eval-expression-print-length eval-expression-print-length-default
                     print-length print-length-default))
        ((eq eval-length-toggle-truncation 'off)
         (setq-local eval-length-toggle-truncation 'on
                     eval-expression-print-length nil
                     print-length nil))))

(defun dc/eval-level-toggle-truncation ()
  (interactive)
  ;; monoid
  (if (not (boundp 'eval-level-toggle-truncation))
      (setq-local eval-level-toggle-truncation 'off
                  eval-expression-print-level-default eval-expression-print-level
                  print-level-default print-level))

  (cond ((eq eval-level-toggle-truncation 'on)
         (setq-local eval-level-toggle-truncation 'off
                     eval-expression-print-level eval-expression-print-level-default
                     print-level print-level-default))
        ((eq eval-level-toggle-truncation 'off)
         (setq-local eval-level-toggle-truncation 'on
                     eval-expression-print-level nil
                     print-level nil))))

;;* Emacs Lisp
;;** ELD

;; NOTE: projectile already has unserialize/deserialize to/from ELD
;;
;; (defalias 'dc/eld-serialize 'projectile-serialize)
;; (defalias 'dc/eld-unserialize 'projectile-unserialize)

;; this lacks error-handling (and it quotes all the top-level forms)
;; (eq (dc/eld-unserialize f)
;;     (caadr (dc/read-lisp-into-list)))
(defun dc/read-lisp-into-list (file)
  "Read lisp from `file' into a list without evaluating it."
  ;; TODO: ensure dc/read-lisp-into-list doesn't eval anything
  (with-temp-buffer
    (save-excursion
      (insert "'(\n")
      (insert-file-contents file)
      (goto-char (point-max))
      (insert "\n)\n"))
    ;; (pp (current-buffer))
    (read (current-buffer))))


;;** Pretty Print

(defalias 'ppe #'pp-eval-last-sexp)
(defalias 'ppel #'pp-emacs-lisp-code)
(defalias 'ppme #'pp-macroexpand-last-sexp)

(provide 'dc-util)
;;; dc-util.el ends here
