;;; help-dwim.el --- show help information

;; Copyright 2007 Ye Wenbin
;;
;; Author: wenbinye@gmail.com
;; Version: $Id: help-dwim.el,v 0.0 2007/08/22 06:35:00 ywb Exp $
;; Modified-by: Štěpán Němec <stepnem@gmail.com>
;; Time-stamp: "2010-03-05 12:13:39 CET stepnem"
;; Keywords: help, convenience
;; 
;; This file is part of PDE (Perl Development Environment).
;; But it is useful in general.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:
;;  This package provides a single command `help-dwim' to search and
;;  display multiple kinds of documentation for symbols.
;;
;;  Woman Note: The woman-topic-all-completions is create using
;;  woman-file-name which will prompt for the file name. So you may
;;  have to M-x woman before active woman.

;;; Dependencies:
;;  no extra libraries are required

;;; Installation:
;;  Put this file into your load-path and the following into your
;;  `user-init-file':
;;  (require 'help-dwim)
;;   

;;; Code:

(eval-when-compile
  (require 'cl))

(defgroup help-dwim nil
  "Show help information from different sources."
  :group 'help
  :group 'pde)

;; FIXME why vectors?
(defvar help-dwim-type-alist
  '((elisp-function . [function-called-at-point obarray fboundp describe-function])
    (elisp-variable . [variable-at-point obarray boundp describe-variable]))
  "*List of types for `help-dwim'.")

(defvar help-dwim-autoloads nil
  "Autoload code for `help-dwim-load-extra'")

;;; Helper functions
(defsubst help-dwim-type (type)
  (car type))

(defsubst help-dwim-object (type)
  (aref (cdr type) 0))

(defsubst help-dwim-obarray (type)
  (and (boundp (aref (cdr type) 1))
       (symbol-value (aref (cdr type) 1))))

(defsubst help-dwim-predicate (type)
  (aref (cdr type) 2))

(defsubst help-dwim-handler (type)
  (aref (cdr type) 3))

(defun help-dwim-load-extra ()
  (interactive)
  (dolist (extra help-dwim-autoloads)
    (when (and (memq (car extra) help-dwim-active-types)
               (null (help-dwim-obarray (assoc (car extra) help-dwim-type-alist))))
      (eval (cdr extra)))))

(defun help-dwim-active-types ()
  "All activated types. Remove those not register yet."
  (delq nil (mapcar (lambda (type)
                      (assoc type help-dwim-type-alist))
                    help-dwim-active-types)))

(defcustom help-dwim-active-types '(elisp-function elisp-variable)
  "*Activated types.
The order of this list is important for the default behavior of
`help-dwim'."
  :type '(set :convert-widget
              (lambda (wid)
                (setq help-dwim-active-types
                      (help-dwim-active-types))
                (widget-put wid :args
                            (mapcar (lambda (type)
                                      (list 'const (car type)))
                                    help-dwim-type-alist))
                wid))
  :set (lambda (symbol value)
         (set symbol value)
         (help-dwim-load-extra)
         value)
  :group 'help-dwim)

(defvar help-dwim-last-item nil
  "FIXME")

(defvar help-dwim-obarray nil
  "FIXME")

(defun help-dwim-guess-types (name)
  "Guess types of NAME."
  (let (types sym predicate)
    (mapc
     (lambda (type)
       (setq help-dwim-obarray (help-dwim-obarray type))
       (when (and help-dwim-obarray
                  (setq sym (intern-soft name help-dwim-obarray))
                  (if (setq predicate (help-dwim-predicate type))
                      (funcall predicate sym)
                    t))
         (push (help-dwim-type type) types)))
     (help-dwim-active-types))
    (nreverse types)))

(defun help-dwim-things-ap ()
  "Find symbol under point that is interned in obarrays of active types."
  (let (things thing object sym predicate)
    (dolist (type (help-dwim-active-types) things)
      (save-excursion
        (setq object (help-dwim-object type))
        (if (symbolp object)
            (when (setq thing (funcall object))
              (if (stringp thing)
                  (setq thing (intern-soft thing (help-dwim-obarray type))))
              (and thing (symbolp thing)
                   (push (cons (help-dwim-type type) thing) things)))
          (skip-chars-forward object)
          (setq thing (buffer-substring
                       (progn (skip-chars-backward object) (point))
                       (progn (skip-chars-forward object) (point))))
          (and (setq help-dwim-obarray (help-dwim-obarray type))
               (setq sym (intern-soft thing help-dwim-obarray))
               (if (setq predicate (help-dwim-predicate type))
                   (funcall predicate sym)
                 t)
               (push (cons (help-dwim-type type) sym) things)))))))

;;;###autoload 
(defun help-dwim (name &optional type)
  "Show help for NAME.
TYPE is one of `help-dwim-active-types'."
  (interactive 
   (let ((things (help-dwim-things-ap))
         ;; for speedup completion functions, Is it really help?
         (collections (mapcar (lambda (type)
                               (cons (help-dwim-obarray type)
                                     (help-dwim-predicate type)))
                             (help-dwim-active-types)))
         types name)
     (setq name
           (completing-read
            (if things
                (format "Describe (default %S): " (cdar things))
              "Describe: ")
            (lambda (str pred flag)
              (let ((types collections) complete)
                (cond ((eq flag 'lambda) ; for test-completion
                       (while (and (not complete) types)
                         (setq complete (test-completion str (caar types)
                                                         (cdar types))
                               types (cdr types)))
                       complete)
                      ((null flag)      ; for try-completion
                       (while (and (not complete) types)
                         (setq complete (try-completion str (caar types)
                                                        (cdar types)))
                         (unless (or (eq complete 't)
                                     (= (length complete) (length str)))
                           (setq complete nil))
                         (setq types (cdr types)))
                       complete)
                      (t                ; for all-completions
                       (apply 'append
                              (mapcar 
                               (lambda (type)
                                 (all-completions str (car type) (cdr type)))
                               collections))))))
            nil t nil nil
            (if things (symbol-name (cdar things)))))
     (setq types (help-dwim-guess-types name))
     (if (= (length types) 1)
         (list name (car types))
       (list name (intern (completing-read
                           (format "Type of description (default %S): "
                                   (car types))
                           (mapcar 'list types)
                           nil t nil nil (symbol-name (car types))))))))
  (setq type (assoc type help-dwim-type-alist))
  (funcall (help-dwim-handler type)
           (intern-soft name (help-dwim-obarray type))))

;;;###autoload 
(defun help-dwim-activate-type (type &optional append)
  "Activate TYPE for current buffer.
If APPEND is non-nil, that means TYPE is an additional help command.
Use `help-dwim-customize-types' to activate or deactivate types globally."
  (interactive
   (list (intern (completing-read "Activate type: "
                                  help-dwim-type-alist nil t))
         current-prefix-arg))
  (help-dwim-deactivate-type type)
  (add-to-list 'help-dwim-active-types type append)
  (help-dwim-load-extra))

(defun help-dwim-deactivate-type (type)
  "Deactivate TYPE for current buffer.
If APPEND is non-nil, that means TYPE is an additional help command.
Use `help-dwim-customize-types' to activate or deactivate types globally."
  (interactive
   (list (intern (completing-read "Deactivate type: "
                                  (mapcar 'list help-dwim-active-types)
                                  nil t))))
  (make-local-variable 'help-dwim-active-types)
  (setq help-dwim-active-types (remove type help-dwim-active-types)))

(defun help-dwim-customize-types ()
  (interactive)
  (customize-variable 'help-dwim-active-types))

(defun help-dwim-register (type activate &optional body)
  "Register a new type of help.
TYPE is an element of `help-dwim-type-alist'.
If ACTIVATE is non-nil, the type will be added to
`help-dwim-active-types', and BODY will be evaluated immediately. BODY
is the form to evaluate when the type is activated. If the type is
registered without being activated, BODY will be added to
`help-dwim-autoloads'. When you use `help-dwim-activate-type' or
`help-dwim-customize-types' to add the type, BODY will be evaluated
at that time."
  (add-to-list 'help-dwim-type-alist type t)
  (if (or activate (memq type help-dwim-active-types))
      (progn
        (add-to-list 'help-dwim-active-types (car type) t)
        (and body (eval (cons 'progn body)))) ; FIXME hm...
    (when body
      (add-to-list 'help-dwim-autoloads (cons (car type) (cons 'progn body))))))

;;; An example to show how to add a new type to help-dwim
(defvar help-dwim-woman-obarray nil
  "Items in `woman-topic-all-completions'")

(defun help-dwim-build-woman-obarray (&optional re-cache)
  "Build help-dwim-obarray from woman-topic-all-completions.
With a prefix argument, force the caches of woman to be updated."
  (interactive "P")
  (require 'woman)
  (woman-file-name "" re-cache) ; rebuild the caches if needed
  (setq help-dwim-woman-obarray (make-vector 1519 0))
  (mapc (lambda (elt)
          (intern (car elt) help-dwim-woman-obarray))
        woman-topic-all-completions))

(defun help-dwim-woman (symbol)
  (woman (symbol-name symbol)))

(help-dwim-register
 '(woman . ["-+.:[_a-zA-Z0-9" help-dwim-woman-obarray nil help-dwim-woman])
 nil
 '((help-dwim-build-woman-obarray)))

(provide 'help-dwim)
;;; help-dwim.el ends here
