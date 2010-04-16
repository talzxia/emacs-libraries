;;; yaoddmuse-extension.el --- Some enhanced functions for yaoddmuse.el

;; Filename: yaoddmuse-extension.el
;; Description: Some enhanced functions for yaoddmuse.el
;; Author: Andy Stewart <lazycat.manatee@gmail.com>
;; Maintainer: Andy Stewart <lazycat.manatee@gmail.com>
;; Copyright (C) 2009, Andy Stewart, all rights reserved.
;; Created: 2009-01-09 22:27:36
;; Version: 0.2.3
;; Modified-by: Štěpán Němec <stepnem@gmail.com>
;; Last-Updated: 2010-02-24 23:13
;;           By: Štěpán Němec
;; URL: FIXME
;; Original-URL: http://www.emacswiki.org/emacs/download/yaoddmuse-extension.el
;; Keywords: oddmuse, yaoddmuse, wiki
;; Compatibility: GNU Emacs 22-23
;;
;; Features that might be required by this library:
;;
;; `yaoddmuse' `w3m' `growl'
;;

;;; This file is NOT part of GNU Emacs

;;; License
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; Some extra functions for `yaoddmuse.el':
;;
;; 1. Function `yaoddmuse-browse-page-in-w3m' can be used as the value of
;;    `yaoddmuse-browse-function' to replace the default browse function
;;    (`browse-url'). By default, `browse-url' just opens a new buffer.
;;    `yaoddmuse-browse-page-in-w3m' will search for an existing buffer
;;    containing the page, and update that instead of duplicating the page in
;;    w3m. It's very useful when you edit a lot of pages in a single session.
;;
;;    (setq yaoddmuse-browse-function 'yaoddmuse-browse-page-in-w3m)
;;
;; 2. `yaoddmuse-w3m-edit-emacswiki-page' can be used to edit the EmacsWiki
;;    page displayed in the current w3m buffer without entering the page name.
;;
;; 3. Commands `yaoddmuse-yasnippet-insert-file' and
;;    `yaoddmuse-yasnippet-insert-directory' can be used to insert YASnippet
;;    templates as EmacsWiki code snippets. Handy for sharing with others. :)
;;
;; 4. Function `yaoddmuse-notify-popup-window' uses the "notify-send" program
;;    to display Yaoddmuse notification window.
;;
;;    (setq yaoddmuse-notify-function 'yaoddmuse-notify-popup-window)
;;


;;; Installation:
;;
;; Put yaoddmuse-extension.el in your `load-path'.
;;
;; And the following to your `user-init-file':
;;
;; (require 'yaoddmuse-extension)

;;; Change Log:
;; 2010/02/24
;;  * Štěpán Němec:
;;      * Cleanup.
;;
;; 2009/03/11
;;  * Andy Stewart:
;;      * Fix bug of `yaoddmuse-w3m-edit-emacswiki-page'.
;;      * Fix doc.
;;
;; 2009/03/11
;;   * rubikitch
;;      * Add new function `yaoddmuse-notify-by-growl'
;;
;; 2009/02/22
;;   * Andy Stewart:
;;      * Add new function `yaoddmuse-notify-popup-window'
;;      * Fix doc.
;;
;; 2009/02/13
;;   * Andy Stewart:
;;      * Add new command `yaoddmuse-yasnippet-insert-file'
;;        and `yaoddmuse-yasnippet-insert-directory'.
;;        Fast insert yasnippet for sharing. :)
;;
;; 2009/01/09
;;      * First released.

;;; Code:

(require 'yaoddmuse)
(require 'w3m)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Customize ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defcustom yaoddmuse-notify-cmd "notify-send"
  "Notification command to use."
  :type 'string
  :group 'yaoddmuse)

(defcustom yaoddmuse-notify-icon "~/MyEmacs/Image/Irc.png"
  "Image file to use as the notification icon."
  :type 'string
  :group 'yaoddmuse)

(defcustom yaoddmuse-notify-timeout 5000
  "Notification timeout in milliseconds."
  :type 'number
  :group 'yaoddmuse)

(defcustom yaoddmuse-notify-urgency "low"
  "Notification urgency level (low, normal, critical)."
  :type 'string
  :group 'yaoddmuse)

(defcustom yaoddmuse-notify-category "im.received"
  "Notification category."
  :type 'string
  :group 'yaoddmuse)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Interactive Functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun yaoddmuse-yasnippet-insert-file (file)
  "Insert a YASnippet template FILE."
  (interactive "fFile: ")
  (insert
   (with-temp-buffer
     (insert-file-contents file)
     (string-insert-rectangle (point-min) (point-max) "    ")
     (goto-char (point-max))
     (newline)
     (insert "}}}\n\n")
     (goto-char (point-min))
     (insert "{{{")
     (newline)
     (goto-char (point-min))
     (open-line 1)
     (insert (format "* %s" (file-name-nondirectory file)))
     (buffer-string))))

(defun yaoddmuse-yasnippet-insert-directory (dir)
  "Insert YASnippet template files in DIR."
  (interactive "DDirectory: ")
  (dolist (file (directory-files dir t))
    (unless (file-directory-p file)
      (unless (string-match "^\\.?#" (file-name-nondirectory file))
        (yaoddmuse-yasnippet-insert-file file)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Utility Functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun yaoddmuse-browse-page-in-w3m (url)
  "This function is browse URL in w3m."
  (let ((current-window (selected-window)))
    (unless (eq major-mode 'w3m-mode)
      (other-window 1))
    (catch 'find-match
      (dolist (buffer (buffer-list))
        (set-buffer buffer)
        (and (eq major-mode 'w3m-mode)
             (or (string-equal w3m-current-url url)
                 (yaoddmuse-match-emacswiki-page w3m-current-url url))
             (with-current-buffer buffer
               (switch-to-buffer buffer)
               (w3m-reload-this-page)
               (throw 'find-match "Find same page in w3m-mode."))))
      (w3m-goto-url-new-session url t))
    (select-window current-window)))

(defun yaoddmuse-match-emacswiki-page (url match-url)
  "Return t if URL matches an EmacsWiki URL corresponding to MATCH-URL."
  (when (string-match "^http://www.emacswiki.org" url)
    (let ((page-name
           (replace-regexp-in-string
            (format
             "^%s/" (cadr (assoc "EmacsWiki" yaoddmuse-wikis))) "" match-url)))
      ;; FIXME
      (or (string-equal
           (format "http://www.emacswiki.org/emacs/%s" page-name) url)
          (string-equal
           (format "http://www.emacswiki.org/emacs-en/%s" page-name) url)
          (string-equal
           (format "http://www.emacswiki.org/cgi-bin/emacs/%s" page-name) url)))))

(defun yaoddmuse-w3m-edit-emacswiki-page ()
  "Edit the current EmacsWiki wiki page."
  (interactive)
  (yaoddmuse-edit "EmacsWiki" (replace-regexp-in-string
                               "\\(.*id=\\).*$" ""
                               (replace-regexp-in-string
                                "http://.*/\\([^/]+\\?\\)?" ""
                                w3m-current-url)
                               nil nil 1)))

(defun yaoddmuse-notify-popup-window (&optional msg)
  "Pop up a notification window with MSG using \"notify-send\"."
  (flet ((message (&rest args)))
    (shell-command (concat yaoddmuse-notify-cmd
                           " -i " yaoddmuse-notify-icon
                           " -t " (int-to-string
                                   yaoddmuse-notify-timeout)
                           " -u " yaoddmuse-notify-urgency
                           " -c " yaoddmuse-notify-category
                           " -- "
                           " \"Yaoddmuse-Notify\""
                           " \"" (or msg "") "\""))))

;; FIXME dunno...
(defun yaoddmuse-notify-by-growl (msg)
  "Use program `growl' notify yaoddmuse-message MSG."
  (if (require 'growl nil t)
      (with-no-warnings
        (growl "Yaoddmuse" msg))))

(provide 'yaoddmuse-extension)

;;; yaoddmuse-extension.el ends here

;;; LocalWords:  yaoddmuse oddmuse yasnippet fFile DDirectory cmd im msg
;;; LocalWords:  PageName
