;;; w3m-extension.el --- emacs-w3m extensions

;; Copyright (C) 2008, 2009, Andy Stewart, all rights reserved.

;; Author: Andy Stewart <lazycat.manatee@gmail.com>
;; Maintainer: Štěpán Němec <stepnem@gmail.com>
;; Created: 2008-06-07 22:06:58
;; Version: 0.3
;; Keywords: emacs-w3m, w3m, Google, search

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:
(require 'w3m)


(defun w3m-toggle-with-other-buffer ()
  "Switch to a w3m buffer or return to the previous buffer."
  (interactive)
  (let ((blist (buffer-list))
        (test (if (derived-mode-p 'w3m-mode)
                  (lambda () (not (derived-mode-p 'w3m-mode)))
                (lambda () (derived-mode-p 'w3m-mode)))))
    (while blist
      (if (with-current-buffer (car blist)
            (and (not (minibufferp)) (funcall test)))
          (progn
            (switch-to-buffer (car blist))
            (setq blist nil))
        (setq blist (cdr blist))))))

(defun w3m-emacswiki-view-diff ()
  "View a diff of the current EmacsWiki page."
  (interactive)
  (w3m-view-regexp-url
   "^\\(Last edited\\|Edited\\) [0-9]\\{4\\}\\(-[0-9]\\{2\\}\\)\\{2\\}\
 [0-9]\\{2\\}:[0-9]\\{2\\} UTC by .*(diff)$"
   "different"))

(defun w3m-emacswiki-view-other-version ()
  "View other versions of the current EmacsWiki page."
  (interactive)
  (w3m-view-regexp-url
   "^Edit this page View other revisions"
   "other version"))

(defun w3m-view-regexp-url (regexp echo-string)
  "View regexp link in emacswiki.org"
  (let ((remember-pos (point)))
    (w3m-redisplay-this-page)
    (goto-char (point-min))
    (if (search-forward-regexp regexp nil t)
        (progn
          (backward-char)
          (w3m-view-this-url t) ; t -> force reload
          (message (format "Read %s with current wiki page." echo-string)))
      (goto-char remember-pos)
      (message (format "Don't %s in current wiki page." echo-string)))))

;;;###autoload
(defun w3m-emacswiki-recent-changes ()
  "View recent EmacsWiki changes using w3m."
  (interactive)
  (w3m-goto-url-new-session
   "http://www.emacswiki.org/emacs/?action=rc;days=7;all=1;showedit=1"))

(provide 'w3m-extension)
;;; w3m-extension.el ends here
