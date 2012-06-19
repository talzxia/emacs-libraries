;;; rfc.el --- download and view RFC documents

;; Maintainer: Katsuya Iida (katsuya_iida@hotmail.com)
;; Keywords: rfc view
;; Modified-by: Štěpán Němec <stepnem@gmail.com>
;; Time-stamp: "2012-06-20 00:33:14 CEST stepnem"

;; This is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This software is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with This software; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; You can use this package to download and view the RFC documents. The entry
;; points are `rfc-index' and `rfc-goto-number'.
;;
;; The variable `rfc-article-alist' specifies the directories or URLs where
;; rfc-index.txt and rfc????.txt files are to be found, or a zip file which
;; contains them.
;;
;; All visible identifiers in this package are prefixed with `rfc-'.
;; `rfc-index-' and `rfc-article-' are used for commands pertaining to
;; `rfc-index-mode' and `rfc-article-mode', respectively.

;;; Code:

(require 'dotelib)

(defface rfc-node
  '((t (:bold t :foreground "blue")))
  "Face for RFC index node."
  :group 'rfc)

(defface rfc-xref
  '((t (:bold t)))
  "Face for RFC index node."
  :group 'rfc)

(defface rfc-header
  '((t (:bold t :italic t)))
  "Face for RFC header."
  :group 'rfc)

(defface rfc-subject
  '((t (:bold t)))
  "Face for RFC subject."
  :group 'rfc)

(defcustom rfc-index-mode-hook nil
  "Normal hook run when starting RFC index mode."
  :type 'hook
  :group 'rfc)

(defcustom rfc-insert-content-url-hook '(rfc-url-save)
  "Hook run after downloading an RFC file."
  :type 'hook
  :group 'rfc)

(defcustom rfc-article-mode-hook nil
  "Normal hook run when starting RFC index mode."
  :type 'hook
  :group 'rfc)

;; "ftp://ftp.rfc-editor.org/in-notes/rfc-index.txt"
(defcustom rfc-index-url "http://www.ietf.org/download/rfc-index.txt"
  "URL for index file."
  :type 'string
  :group 'rfc)

;; FIXME weird things happen when the directory does not exist
(defcustom rfc-url-save-directory "~/.emacs.d/rfc"
  "Directory where files retrieved from URL is saved."
  :type 'directory
  :group 'rfc)

(defcustom rfc-archive-alist (list rfc-url-save-directory
                                   "http://www.rfc-editor.org/rfc/")
  "A list of places from where RFC files are retrieved."
  :type '(repeat string)
  :group 'rfc)

(defcustom rfc-fontify t
  "Non-nil enables highlighting and fonts in rfc mode."
  :type 'boolean
  :group 'rfc)

(defcustom rfc-unzip-command "unzip"
  "UnZip command filename."
  :type 'file
  :group 'rfc)


(defconst rfc-start-tag-index "^[ \t]*\n\\([0-9][0-9][0-9][0-9]\\) ")

;;; helper functions
(defun rfc-nearest-rfc-number ()
  "Return the RFC number appearing nearest in the buffer.
If there is no such a node, it returns nil."
  (save-excursion
    (backward-word 2)
    (and (re-search-forward "RFC[- ]?\\([0-9][0-9][0-9][0-9]\\)"
                            (min (point-max) (+ (point) 15)) t)
         (buffer-substring (match-beginning 1) (match-end 1)))))

(defun rfc-index-current-number ()
  (save-excursion
    (end-of-line)
    (re-search-backward rfc-start-tag-index nil t)
    (buffer-substring (match-beginning 1) (match-end 1))))

(defun rfc-index-start-of-current ()
  (save-excursion
    (end-of-line)
    (re-search-backward rfc-start-tag-index nil t)))

(defun rfc-index-end-of-current ()
  (save-excursion
    (end-of-line)
    (or (re-search-forward rfc-start-tag-index nil t) (point-max))))

(defun rfc-index-related-number-of-nearest-node (tag)
  (save-excursion
    (re-search-backward "^[ \t]*$" nil t)
    (forward-line)
    (and
     (re-search-forward
      (concat tag "[ \t]+by[ \t]+RFC\\([0-9][0-9][0-9][0-9]\\)")
      (rfc-index-end-of-current) t)
     (string-to-int (buffer-substring (match-beginning 1) (match-end 1))))))

;;; RFC index mode
;; FIXME
(defvar rfc-index-mode-map nil
  "Keymap for RFC index mode.")

(unless rfc-index-mode-map
  (setq rfc-index-mode-map (make-sparse-keymap))
  (suppress-keymap rfc-index-mode-map)
  (let ((map rfc-index-mode-map))
    (define-key map "\C-m" 'rfc-index-goto-nearest)
    (define-key map "g" 'rfc-goto-number)
    (define-key map "\C-j" 'rfc-index-follow-nearest)
    (define-key map "f" 'rfc-index-follow-number)
    (define-key map "o" 'rfc-index-follow-obsoleted)
    (define-key map "O" 'rfc-index-follow-obsoletes)
    ;; FIXME switch updates & updated for consistency
    (define-key map "u" 'rfc-index-follow-updates)
    (define-key map "U" 'rfc-index-follow-updated)
    (define-key map [mouse-2] 'rfc-index-mouse-2)
    ;; FIXME next-line previous-line
    (define-key map "n" 'scroll-up)
    (define-key map "p" 'scroll-down)
    (define-key map " " 'scroll-up)
    (define-key map "\C-?" 'scroll-down)
    (define-key map [backspace] 'scroll-down)
    (define-key map "s" 'isearch-forward)
    (define-key map "r" 'isearch-backward)
    (define-key map "q" 'rfc-index-kill-buffer)))

(defun rfc-index-mode ()
  (setq major-mode 'rfc-index-mode)
  (setq mode-name "RFC Index")
  (setq buffer-read-only t)
  (use-local-map rfc-index-mode-map)
  (run-hooks 'rfc-index-mode-hook))

;;;###autoload
(defun rfc-index (&optional arg)
  "Display the RFC index.
With a prefix argument, download a fresh index file first, even
if already present locally."
  (interactive "P")
  (when arg
    (condition-case nil
        (progn
          (delete-file (concat rfc-url-save-directory "/rfc-index.txt"))
          (kill-buffer "*RFC index*"))
      (error nil)))
  (switch-to-buffer "*RFC index*")
  (when (< (buffer-size) 10) ;; Buffer is empty
    (rfc-insert-contents "rfc-index.txt")
    (when rfc-fontify
      (rfc-index-fontify-buffer))
    (set-buffer-modified-p nil)
    (goto-char (point-min)))
  (rfc-index-mode))

(defun rfc-index-follow-number (number)
  (interactive "nFollow RFC number: ")
  (switch-to-buffer "*RFC index*")
  (goto-char (point-min))
  (re-search-forward (concat "^" (format "%04d" number)))
  (beginning-of-line))

(defun rfc-index-fontify-buffer ()
  "Fontify the current buffer in article mode."
  (save-excursion
    (let ((buffer-read-only nil))
      (goto-char (point-min))
      (while (re-search-forward "^\\([0-9][0-9][0-9][0-9]\\) " nil t)
        (put-text-property (match-beginning 1) (match-end 1) 'face 'rfc-node)
        (put-text-property (match-beginning 1) (match-end 1)
                           'mouse-face 'highlight))
      (goto-char (point-min))
      (while (re-search-forward "\\(RFC[0-9][0-9][0-9][0-9]\\)" nil t)
        (put-text-property (match-beginning 1) (match-end 1) 'face 'rfc-xref)
        (put-text-property (match-beginning 1) (match-end 1)
                           'mouse-face 'highlight)))))

;; The user commands of RFC index mode

(defun rfc-index-mouse-2 (click)
  (interactive "e")
  (if (featurep 'xemacs)
      nil ; (event-closest-point click)
    (goto-char (car (cdr (event-start click)))))
  (rfc-index-goto-nearest))

(defun rfc-index-goto-nearest ()
  "Go to the article referenced most closely."
  (interactive)
  (let ((number (or (rfc-nearest-rfc-number) (rfc-index-current-number))))
    (rfc-goto-number number)
    (add-to-history 'rfc-number-history number)))

(defun rfc-index-follow-updates () ; might not be used.
  "Go to the index of the node updated by the current node."
  (interactive)
  (let ((number (rfc-index-related-number-of-nearest-node "Updates")))
    (or number (error "This RFC updates nothing"))
    (rfc-index-follow-number number)))

(defun rfc-index-follow-updated () ; might not be used.
  "Go to the index of the node updating the current node."
  (interactive)
  (let ((number (rfc-index-related-number-of-nearest-node "Updates")))
    (or number (error "This RFC isn't updated"))
    (rfc-index-follow-number number)))

(defun rfc-index-follow-obsoletes ()
  "Go to the index of the node obsoleted by the current node."
  (interactive)
  (let ((number (rfc-index-related-number-of-nearest-node "Obsoletes")))
    (or number (error "This RFC obsoletes nothing"))
    (rfc-index-follow-number number)))

(defun rfc-index-follow-obsoleted ()
  "Go to the index of the node obsoleting the current node."
  (interactive)
  (let ((number (rfc-index-related-number-of-nearest-node "Obsoleted")))
    (or number (error "This RFC isn't obsoleted"))
    (rfc-index-follow-number number)))

(defun rfc-index-kill-buffer ()
  (interactive)
  (kill-buffer (current-buffer)))

;;; RFC Article mode
(defvar rfc-article-mode-map nil
  "Keymap for RFC Article mode.")

(unless rfc-article-mode-map
  (setq rfc-article-mode-map (make-sparse-keymap))
  (suppress-keymap rfc-article-mode-map)
  (let ((map rfc-article-mode-map))
    (define-key map "\C-m" 'rfc-article-goto-nearest)
    (define-key map "d" 'rfc-index)
    (define-key map "g" 'rfc-goto-number)
    (define-key map "P" 'rfc-article-goto-page)
    (define-key map [mouse-2] 'rfc-article-mouse-2)
    (define-key map "n" 'rfc-article-next-page)
    (define-key map "p" 'rfc-article-previous-page)
    (define-key map " " 'rfc-article-next-page)
    (define-key map [backspace] 'rfc-article-previous-page)
    (define-key map "\C-?" 'rfc-article-previous-page)
    (define-key map "s" 'isearch-forward)
    (define-key map "r" 'isearch-backward)
    (define-key map "q" 'rfc-article-kill-buffer)))

(defun rfc-article-mode ()
  (setq major-mode 'rfc-article-mode)
  (setq mode-name "RFC Article")
  (setq buffer-read-only t)
  (use-local-map rfc-article-mode-map)
  (run-hooks 'rfc-article-mode-hook))

(defvar rfc-number-history nil)
;;;###autoload
(defun rfc-goto-number (number)
  "Display RFC number NUMBER."
  (interactive (list (.read-string-with-default
                      "Go to RFC number" 'rfc-number-history
                      (.match-nearest-point "[0-9]+" "w" t))))
  (switch-to-buffer (concat "*RFC" number "*"))
  (rfc-insert-contents (concat "rfc" number ".txt"))
  (rename-buffer (concat "*RFC" number "*"))
  (when rfc-fontify
    (rfc-article-fontify-buffer))
  (set-buffer-modified-p nil)
  (rfc-article-beginning-of-article)
  (rfc-article-mode))

;; FIXME reimplement using font-lock-keywords
(defun rfc-article-fontify-buffer ()
  "Fontify the current buffer in article mode."
  (save-excursion
    (let ((buffer-read-only nil))
      (goto-char (point-min))
      (while (re-search-forward ".\\(RFC[- ]?[0-9][0-9][0-9][0-9]\\)" nil t)
        (put-text-property (match-beginning 1) (match-end 1) 'face 'rfc-node)
        (put-text-property (match-beginning 1) (match-end 1)
                           'mouse-face 'highlight))
      ;; (goto-char (point-min))
      ;; (while (re-search-forward "^\\([^ \t].*$\\)" nil t)
      ;; (put-text-property (match-beginning 1) (match-end 1) 'face 'rfc-subject))
      (goto-char (point-min))
      (while (re-search-forward "^\\(RFC[- ][0-9][0-9][0-9][0-9].*$\\)" nil t)
        (put-text-property (match-beginning 1) (match-end 1)
                           'face 'rfc-header))
      (goto-char (point-min))
      (while (re-search-forward "^\\([^ \t].*\\[Page [0-9]+\\]$\\)" nil t)
        (put-text-property (match-beginning 1) (match-end 1)
                           'face 'rfc-header)))))

(defun rfc-article-mouse-2 (click)
  (interactive "e")
  (if (featurep 'xemacs)
      nil
    (goto-char (car (cdr (event-start click)))))
  (rfc-article-goto-nearest))


(defun rfc-article-goto-nearest ()
  "Go to the article referenced most closely."
  (interactive)
  (rfc-index-goto-nearest))

(defun rfc-article-beginning-of-article ()
  (interactive)
  (goto-char (point-min))
  (forward-line 3)
  (recenter 0))

(defun rfc-article-goto-page (number)
  "Go to the NUMBERth page of the current RFC document."
  (interactive "nGo to page number: ")
  (cond
   ((= number 1)
    (rfc-article-beginning-of-article))
   ((> number 1)
    (goto-char (point-min))
    (search-forward (concat "[Page " (number-to-string (1- number)) "]"))
    (forward-line 2)
    (beginning-of-line)
    (recenter 0))
   (t
    (error "Page number should be positive"))))

(defun rfc-article-next-page ()
  (interactive)
  (if (search-forward "\014" nil t)
      (progn
        (if (> (forward-line 1) 0)
            (beep))
        (beginning-of-line)
        (recenter 0))
    (beep)))

(defun rfc-article-previous-page ()
  (interactive)
  (if (search-backward "\014" nil t)
      nil
    (beep)
    (rfc-article-beginning-of-article))
  (if (search-backward "\014" nil t)
      (progn
        (forward-line 1)
        (beginning-of-line)
        (recenter 0))
    (rfc-article-beginning-of-article)))

(defun rfc-article-kill-buffer ()
  (interactive)
  (kill-buffer (current-buffer)))

;;; Functions to fetch RFC files
(defun rfc-insert-contents (filename &optional place)
  "Retrieve FILENAME from RFC archive and insert it into the current buffer.
Optional argument PLACE can be one of the symbols `local' or
`network' and restricts the retrieval to the corresponding method
only."
  (unless rfc-archive-alist
    (error "`rfc-archive-alist' not set"))
  (erase-buffer)
  (let ((inhibit-read-only t)
        (archive-alist rfc-archive-alist)
        (continue t))
    (while (and archive-alist continue)
      (let ((archive (car archive-alist)))
        (if (cond
             ((and (string-match "\\.zip$" archive)
                   (not (eq place 'network)))
              (rfc-insert-contents-zip archive filename))
             ((and (string-match "^\\(ftp\\|http\\)://" archive)
                   (not (eq place 'local)))
              (rfc-insert-contents-url archive filename))
             ((not (eq place 'network))
              (rfc-insert-contents-file archive filename)))
            (setq continue nil)
          (setq archive-alist (cdr archive-alist)))))
    (when continue
      (set-buffer-modified-p nil)
      (kill-buffer nil)
      (error "Not found"))))

(defun rfc-insert-contents-zip (archive filename)
    (shell-command (concat rfc-unzip-command
                           " -p \"" archive "\" " filename)
                   (current-buffer))
    (goto-char (point-min))
    (> (buffer-size) 100))              ;hm...

(defun rfc-insert-contents-file (archive filename)
  (condition-case nil
      ;; FIXME use expand-file-name?
      (insert-file-contents (concat archive "/" filename))
    (error nil)))

(defun rfc-insert-contents-url (archive filename)
  (let ((url
         (if (and (equal filename "rfc-index.txt")
                  rfc-index-url)
             rfc-index-url
           (concat archive filename))))
    (url-insert-file-contents url)
    (if (string-match "<\\(title\\|TITLE\\)>"
                      (buffer-substring (point-min)
                                        (min 500 (point-max))))
        (progn
          (erase-buffer) nil)
      (run-hook-with-args 'rfc-insert-content-url-hook filename)
      t)))

(defun rfc-url-save (filename)
  ;; FIXME
  (write-file (concat rfc-url-save-directory "/" filename))
  (set-visited-file-name nil))

(provide 'rfc)
;;; rfc.el ends here
