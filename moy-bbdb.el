;;; moy-bbdb.el --- add recipients of outgoing mails to BBDB

;; Copyright (C) 2002  Free Software Foundation, Inc.

;; Author: Matthieu Moy <Matthieu.Moy@imag.fr>
;; Modified-by: Štěpán Němec <stepnem@gmail.com>
;; Keywords: mail
;; Version: 1.3M
;; The latest version should allways be availlable from
;; http://www-verimag.imag.fr/~moy/emacs/moy-bbdb.el

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; (Note: Most of this code seems actually copied and modified from BBDB core,
;; cf. for example `bbdb{,/send}-auto-notes-hook'. --sn)

;; This file has been written and tested for use with Gnus. However, I didn't
;; use specific features from Gnus, and this should be able to run without any
;; modification for any other Emacs mailer. It is known to run on Gnus 5.8,
;; Oort Gnus. I have been told that it didn't work for VM. If you use it with
;; VM, then, please, inform me.
;;
;; Usage : put the following code in your .emacs (.gnus.el or .gnus if
;; you use Gnus)
;;
;; (autoload 'bbdb/send-hook "moy-bbdb"
;;   "Function to be added to `message-send-hook' to notice records when sending messages" t)
;;
;; (add-hook 'message-send-hook 'bbdb/send-hook) ; If you use Gnus
;;
;; (add-hook 'mail-send-hook 'bbdb/send-hook) ; For other mailers
;;                                            ; (VM, Rmail)
;;
;; Then, each time a message is sent, the user is asked if the addresses of
;; recipients should be added to the database. The addresses matching
;; `bbdb-user-mail-names' are never added. The variables
;; `bbdb/news-auto-create-p' and `bbdb-ignore-some-messages-alist' are taken
;; in account the same way as for adding senders when recieving messages.
;;
;; Cf. the bbdb-noticing-records Customize group for more information.

;;; Code:

(defcustom bbdb/send-prompt-for-create-p t
  "If non-nil, prompt before automatically creating new BBDB records for people you send mail to.

If this is a function name or lambda, then it is called with no
arguments to decide whether an entry should be automatically
created.

This variable is similar  to `bbdb/prompt-for-create-p', but acts when
sending mail instead of when recieving."
  :group 'bbdb-noticing-records
  :type '(choice (const :tag "Prompt before creating a record" t)
                 (const :tag "Do not prompt" nil)
                 (function :tag "Prompt with function" bbdb/send-)))

(defcustom bbdb/send-auto-create-p t
  "If non-nil, automatically create new BBDB records for people you send mail to.
If this is a function name or lambda, then it is called with no
arguments to decide whether an entry should be automatically
created. You can use this to, for example, create or not create
messages which have a particular subject.

This variable is similar to `bbdb/{news|mail}-auto-create-p', but
acts when sending mail instead of when recieving."
  :group 'bbdb-noticing-records
  :type '(choice (const :tag "Automatically create" t)
                 (const :tag "Do not automatically create" nil)
                 (function :tag "Create with function" bbdb/send-)))

(defvar bbdb/send--scraped-headers '("to" "cc" "bcc")
  "When sending a message, add addresses found in these header fields to BBDB.")

(defcustom bbdb/send-ignore-most-messages-alist nil
  "Alist describing which messages to automatically create BBDB records for when sending messages.

This the equivalent to `bbdb-ignore-most-messages-alist', but
only used when sending messages.

See also `bbdb/send-ignore-some-messages-alist', which has the
opposite effect."
  :group 'bbdb-noticing-records
  :type '(repeat (cons (string :tag "Header name")
                       (regexp :tag "Regex to match on header value"))))


(defcustom bbdb/send-ignore-some-messages-alist nil
  "Alist describing which messages *not* to automatically create BBDB records for when sending messages.

This is equivalent to `bbdb-ignore-some-messages-alist', but only
used when sending messages.

See also `bbdb/send-ignore-most-messages-alist', which has the
opposite effect."
  :group 'bbdb-noticing-records
  :type '(repeat (cons
                  (string :tag "Header name")
                  (regexp :tag "Regex to match on header value"))))


;;;###autoload
(defun bbdb/send-ignore-most-messages-hook (&optional invert-sense)
  "For use as the value of `bbdb/send-auto-create-p'.
This will automatically create BBDB entries for messages which
match the `bbdb/send-ignore-most-messages-alist' (which see) and
*no* others."
  ;; don't need to optimize this to check the cache, because if
  ;; bbdb/*-update-record uses the cache, this won't be called.
  (let ((rest (if invert-sense bbdb/send-ignore-some-messages-alist
                bbdb/send-ignore-most-messages-alist))
        (case-fold-search t)
        (marker (bbdb-header-start))
        done field regexp fieldval)
    (with-current-buffer (marker-buffer marker)
      (save-restriction
        (widen)
        (while (and rest (not done))
          (goto-char marker)
          (setq field (car (car rest))
                regexp (cdr (car rest))
                fieldval (bbdb-extract-field-value field))
          (and fieldval (string-match regexp fieldval) (setq done t))
          (setq rest (cdr rest)))))
    (if invert-sense (not done) done)))

;;;###autoload
(defun bbdb/send-ignore-some-messages-hook ()
  "For use as a `bbdb/send-auto-create-hook'.
This will automatically create BBDB entries for messages which do
*not* match the `bbdb/send-ignore-some-messages-alist' (which
see)."
  (bbdb/send-ignore-most-messages-hook t))

(defcustom bbdb/send-auto-notes-alist nil
  "Alist which lets you have certain pieces of text
automatically added to the BBDB record representing the recipient
of the current message based on the subject or other header
fields. This only works if `bbdb/send-notice-hook' contains
`bbdb/send-auto-notes-hook'. The format of this alist is the same
as `bbdb-auto-notes-alist'. The only difference between the two
variables is that bbdb/send-auto-notes-alist acts when *sending*
mail only.

Example value: '((\"Subject\"
                  (\".*Test.*\" . \"I sent him a test\")
                  (\".*\" last-sent 0 t))))"
  :group 'bbdb-noticing-records
  :type
  '(repeat (bbdb-alist-with-header
            (string :tag "Header name")
            (repeat (cons
                     (regexp :tag "Regexp to match on header value")
                     (string :tag "String for notes if regexp matches"))))))

(defcustom bbdb/send-auto-notes-ignore nil
  "Alist of headers and regexps to ignore.
Used by `bbdb/send-auto-notes-hook'. The format and behaviour are
the same as `bbdb-auto-notes-ignore'."
  :group 'bbdb-noticing-records
  :type '(repeat (cons (string :tag "Header name")
                       (regexp :tag "Regexp to match on header value"))))

(defcustom bbdb/send-auto-notes-ignore-all nil
  "Alist of headers and regexps which cause the entire message to be ignored.
Used by `bbdb/send-auto-notes-hook'. The format and behaviour are
the same as `bbdb-auto-notes-ignore-all'."
  :group 'bbdb-noticing-records
  :type '(repeat (cons (string :tag "Header name")
                       (regexp :tag "Regexp to match on header value"))))

;;;###autoload
(defun bbdb/send-auto-notes-hook (record)
  "For use as a `bbdb/send-notice-hook'.
This might automatically add some text to the notes field of the
BBDB record corresponding to the current record based on the
header of the current message. See the documentation for the
variables `bbdb/send-auto-notes-alist' and
`bbdb/send-auto-notes-ignore'."
  ;; This could stand to be faster...
  ;; could optimize this to check the cache, and noop if this record is
  ;; cached for any other message, but that's probably not the right thing.
  (unless bbdb-readonly-p
    (let ((rest bbdb/send-auto-notes-alist)
          ignore
          (ignore-all bbdb/send-auto-notes-ignore-all)
          (case-fold-search t)
          (marker (bbdb-header-start))
          field pairs fieldval          ; do all bindings here for speed
          regexp string notes-field-name notes
          replace-p replace-or-add-msg)
      (with-current-buffer (marker-buffer marker)
        (save-restriction
          (widen)
          (goto-char marker)
          (while (and ignore-all (not ignore))
            (goto-char marker)
            (setq field (car (car ignore-all))
                  regexp (cdr (car ignore-all))
                  fieldval (bbdb-extract-field-value field))
            (if (and fieldval
                     (string-match regexp fieldval))
                (setq ignore t)
              (setq ignore-all (cdr ignore-all))))

          (unless ignore                ; ignore-all matched
            (while rest ; while their still are clauses in the auto-notes alist
              (goto-char marker)
              (setq field (car (car rest)) ; name of header, e.g., "Subject"
                    pairs (cdr (car rest)) ; (REGEXP . STRING) or
                                        ; (REGEXP FIELD-NAME STRING) or
                                        ; (REGEXP FIELD-NAME STRING REPLACE-P)
                    fieldval (bbdb-extract-field-value field)) ; e.g., Subject line
              (when fieldval
                (while pairs
                  (setq regexp (car (car pairs))
                        string (cdr (car pairs)))
                  (if (consp string)   ; not just the (REGEXP . STRING) format
                      (setq notes-field-name (car string)
                            replace-p (nth 2 string) ; perhaps nil
                            string (nth 1 string))
                    ;; else it's simple (REGEXP . STRING)
                    (setq notes-field-name 'notes
                          replace-p nil))
                  (setq notes (bbdb-record-getprop record notes-field-name))
                  (let ((did-match
                         (and (string-match regexp fieldval)
                              ;; make sure it is not to be ignored
                              (let ((re (cdr (assoc field
                                                    bbdb/send-auto-notes-ignore))))
                                (if re
                                    (not (string-match re fieldval))
                                  t)))))
                    ;; An integer as STRING is an index into match-data:
                    ;; A function as STRING calls the function on fieldval:
                    (if did-match
                        (setq string
                              (cond ((integerp string) ; backward compat
                                     (substring fieldval
                                                (match-beginning string)
                                                (match-end string)))
                                    ((stringp string)
                                     (bbdb-auto-expand-newtext fieldval string))
                                    (t
                                     (goto-char marker)
                                     (let ((s (funcall string fieldval)))
                                       (or (stringp s)
                                           (null s)
                                           (error "%s returned %s: not a string"
                                                  string s))
                                       s)))))
                    ;; need expanded version of STRING here:
                    (if (and did-match
                             string     ; A function as STRING may return nil
                             (not (and notes
                                       ;; check that STRING is not already
                                       ;; present in the NOTES field
                                       (string-match
                                        (regexp-quote string)
                                        notes))))
                        (if replace-p
                            ;; replace old contents of field with STRING
                            (progn
                              (if (eq notes-field-name 'notes)
                                  (message "Replacing with note \"%s\"" string)
                                (message "Replacing field \"%s\" with \"%s\""
                                         notes-field-name string))
                              (bbdb-record-putprop record notes-field-name
                                                   string)
                              (bbdb-maybe-update-display record))
                          ;; add STRING to old contents, don't replace
                          (if (eq notes-field-name 'notes)
                              (message "Adding note \"%s\"" string)
                            (message "Adding \"%s\" to field \"%s\""
                                     string notes-field-name))
                          (bbdb-annotate-notes record string notes-field-name))))
                  (setq pairs (cdr pairs))))
              (setq rest (cdr rest)))))))))

(defcustom bbdb/send-notice-hook nil
  "Hook run when a BBDB record is \"noticed\" while sending a message.
This value overrides the value of `bbdb-notice-hook' while
calling the function `bbdb/send-hook'."
  :group 'bbdb-hooks
  :type 'hook)

(defun bbdb/send-hook-fetch-fields (fields)
  (when fields
    (let ((field-content (mail-fetch-field (car fields))))
      (append (when field-content
                (mapcar (lambda (elem)
                          (concat "\"" (car elem) "\" <" (cadr elem) ">"))
                        (bbdb-extract-address-components field-content)))
              (bbdb/send-hook-fetch-fields (cdr fields))))))

(defun bbdb/send-hook-annotate-message (rcp)
  (unless (string-match (or bbdb-user-mail-names
                            "$^") ;; Regexp matching nothing (?)
                        rcp)
    (bbdb-annotate-message-sender
     rcp t
     (bbdb-invoke-hook-for-value bbdb/send-auto-create-p)
     (bbdb-invoke-hook-for-value bbdb/send-prompt-for-create-p))))

;;;###autoload
(defun bbdb/send-hook ()
  "Auto-create BBDB records when sending messages.
Useful as a value of `mail-send-hook' or `message-send-hook'.

This function parses the relevant message headers and inserts the
addresses of the recipients one by one into BBDB if they do not
exist already."
  (interactive)
  (let ((bbdb-notice-hook bbdb/send-notice-hook))
    (save-restriction
      (widen)
      (narrow-to-region (point-min)
                        (progn (goto-char (point-min))
                               (search-forward mail-header-separator)
                               (beginning-of-line)
                               (point)))
      (let ((recipients
             (bbdb/send-hook-fetch-fields bbdb/send--scraped-headers)))
        (widen)
        (when recipients
          (let ((added-records
                 (delq nil
                       (mapcar 'bbdb/send-hook-annotate-message recipients))))
            (if added-records (bbdb-display-records added-records)
              (message "No records added"))))))))

(provide 'moy-bbdb)
;;; moy-bbdb.el ends here
