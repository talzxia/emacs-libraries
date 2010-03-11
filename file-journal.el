;;; file-journal.el --- revisit files by date or visit count

;; Copyright (C) 2008  Tamas Patrovics
;;                     Jonathan Arkell (current mainteiner)

;; Modified-by: Štěpán Němec <stepnem@gmail.com>
;; Time-stamp: "2010-03-11 12:05:39 CET stepnem"
;; URL: http://github.com/stepnem/emacs-libraries/blob/master/file-journal.el
;; Original-URL: http://www.emacswiki.org/emacs/download/file-journal.el

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
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:
(defgroup file-journal nil
  "File-journal keeps a list of all the files you have visited, like
a persistent most-recently viewed list.  You can control the number
of days to keep the journal for.  You can also set some file patterns
to exclude from tracking.

Work with files as usual and use `M-x fj-show' to revisit them later
by date.")

;;; Change Log:
;; (see also repository history at the URL above)
;;
;; v 0.5 ?
;; IMPORTANT: `fj-journal' renamed to `fj--journal', so if you don't
;;         want to lose your journal data, evaluate the following:
;;         (setq fj--journal fj-journal)
;;         Also, the default value of fj-journal-file changed, so if
;;         you haven't customized it, you might have to load it manually
;;         and possibly to adjust the value.
;;
;;       - other similar renames: `fj-update-fj-buffer', `fj-file-in-excluded',
;;         `fj-record-file'
;;
;;       Other changes:
;;       - `fj--attach-with-anything' -> `fj--add-anything-source'
;;       - "*file-journal*" -> "*File-Journal*"
;;       - use `find-file-hook' instead of advice to track opened files
;;       - make the journal buffer read-only
;;       - comment out the unimplemented `fj-visit-files' definition
;;       - improve the integration with `anything'
;;       - use "~/.emacs.d/.file-journal" as the default value of
;;         `fj-journal-file' to not pollute users' home directory
;;       - set `fj-exclude-files' default to "TAGS$"
;;       - add some library headers
;;       - general cleanup (code, formatting, whitespace, docstrings, comments)
;;
;; v 0.4 - Small optimization to make the exclusion not suck
;;       - Integration with anything
;;
;; v 0.3 - Added a timer that saves the journal once an hour.
;;       - Added a code to refresh the *file-journal* buffer
;;         when a new file is visited.
;;
;; v 0.2 - Changes by Jonathan Arkell
;;       - Minor fixes to allow for vars to be customizeable
;;       - Added an exclusion list
;;
;; v 0.1 - First release by Tamas

;; TODO:
;; - turn the functionality into a proper minor mode(?)
;; - Hook into, or replace ECB's previous files list.
;; - Open more files at once


;; Tested on Emacs 22 and 23.

;;; Code:

(eval-when-compile
  (require 'cl))

(defcustom fj-journal-size 5
  "Number of past days to keep in the journal."
  :type 'integer
  :group 'file-journal)

(defcustom fj-hitlist-size 30
  "Number of most often visited files to keep in the hitlist."
  :type 'integer
  :group 'file-journal)

(defcustom fj-journal-file "~/.emacs.d/.file-journal"
  "File where journal info is stored."
  :type 'file
  :group 'file-journal)

(defcustom fj-exclude-files '("TAGS$")
  "List of regexps specifying which files to exclude from journal.
E.g. using \".*\.muse$\" prevents any Muse files from being stored."
  :type '(repeat regexp)
  :group 'file-journal)

(defcustom fj-save-timer-interval 3600
  "Journal auto-save interval (seconds)."
  :type 'integer
  :group 'file-journal)

(defcustom fj-integrate-with-anything nil
  "If non-nil, add file journal to `anything-sources' automatically."
  :type 'boolean
  :group 'file-journal)

(defface fj-header-face
  '((t (:inherit highlight)))
  "Face for date headers."
  :group 'file-journal)

(defvar fj--journal nil
  "List of (DATE . FILES) pairs describing which files were visited when.")

(defvar fj--hitlist nil
  "List of (FILENAME . COUNT) pairs recording number of visits for each file.")

(defvar fj--current-view-mode 'journal
  "Current view mode in the journal buffer.
One of the symbols `journal' or `hitlist'.")

(define-derived-mode fj-mode fundamental-mode "File Journal"
  (setq buffer-read-only t))

(suppress-keymap fj-mode-map)
(define-key fj-mode-map (kbd "<return>") 'fj-visit-file)
(define-key fj-mode-map "h" 'fj-usage)
(define-key fj-mode-map "k" 'fj-kill)
(define-key fj-mode-map "q" 'fj-quit)
(define-key fj-mode-map "v" 'fj-switch-view)


(defun fj-show ()
  "Show the journal and allow user to select a file."
  (interactive)
  (switch-to-buffer "*File-Journal*")
  (fj-mode)
  (fj--update-fj-buffer)
  (fj-usage))

(defun fj--update-fj-buffer ()
  "Update contents of the journal buffer."
  (if (eq fj--current-view-mode 'journal)
      (fj--display-journal)
    (fj--display-hitlist)))

(defun fj--display-journal ()
  "Insert formatted contents of the journal into the current buffer."
  (setq fj--current-view-mode 'journal)
  (let (buffer-read-only)
    (erase-buffer)
    (dolist (entry fj--journal)
      (unless (bobp)
        (insert "\n"))
      (let ((start (point)))
        (insert (car entry) "\n")
        (put-text-property start (point) 'face 'fj-header-face))
      (dolist (file (cdr entry))
        (insert " " file "\n"))))
  (goto-char (point-min))
  (set-buffer-modified-p nil))

(defun fj--display-hitlist ()
  "Isert formatted contents of the hitlist into the current buffer."
  (setq fj--current-view-mode 'hitlist)
  (let (buffer-read-only)
    (erase-buffer)
    (insert "Files ordered by number of times visited:\n")
    (let ((start (point)))
      (dolist (entry fj--hitlist)
        (insert (int-to-string (cdr entry)) " " (car entry) "\n"))
      (if (fboundp 'align-cols)
          (align-cols start (point) 2)
        (align-regexp start (point) " " 0 1))
      (goto-char start)))
  (set-buffer-modified-p nil))

(defun fj-switch-view (&optional mode)
  "Switch view mode of the journal buffer.
With MODE being one of the symbols `journal' or `hitlist', switch
to that mode unconditionally."
  (interactive (list nil))
  (cond ((eq mode 'journal) (fj--display-journal))
        ((eq mode 'hitlist) (fj--display-hitlist))
        (t (if (eq fj--current-view-mode 'journal)
               (fj--display-hitlist)
             (fj--display-journal)))))

(defun fj-visit-file ()
  "Visit file under the cursor."
  (interactive)
  (if (eq fj--current-view-mode 'journal)
      (if (save-excursion
            (beginning-of-line)
            (looking-at " "))
          (find-file (buffer-substring (1+ (line-beginning-position))
                                       (line-end-position)))
        (error "No file on this line."))
    ;; 'hitlist
    (let ((pos (save-excursion
                 (beginning-of-line)
                 (re-search-forward "^[0-9]+\\s-+" (line-end-position) t))))
      (if pos
          (find-file (buffer-substring pos (line-end-position)))
        (error "No file on this line.")))))


;;; FIXME better mark files as in Dired?
;; (defun fj-visit-files ()
;;   "Visit all the files in the region."
;;   (interactive)
;;   ;region-beginning
;;   ;region-end
;;   )

;;; Well, I know it might seem silly...
(defun fj-quit ()
  "Bury the file journal buffer."
  (interactive)
  (bury-buffer))

(defun fj-kill ()
  "Kill the file journal buffer."
  (interactive)
  (kill-buffer nil)) ; argument required in Emacs < 23

(defun fj-usage ()
  "Display short usage message in the echo area."
  (interactive)
  (let (message-log-max)
    (message
     (substitute-command-keys
      (concat "\\[fj-visit-file] (visit file), "
              "\\[fj-switch-view] (switch view), "
              "\\[fj-quit]/\\[fj-kill] (bury/kill this buffer), "
              "\\[fj-usage] (this help message)")))))

(defun fj--file-in-excluded (file)
  "Test to see if FILE matches the exclusion regex."
  (catch 'excluded
    (dolist (pattern fj-exclude-files)
      (when (string-match pattern file)
        (throw 'excluded t)))))

(defun fj--record-file ()
  "Record the file in the journal."
  (when (and buffer-file-name (not (fj--file-in-excluded buffer-file-name)))
    (let* ((date (format-time-string "%Y-%m-%d"))
           (j-entry (assoc-default date fj--journal))
           (h-entry (assoc buffer-file-name fj--hitlist)))
      (if j-entry
          (setq j-entry (remove buffer-file-name j-entry))
        (push (list date) fj--journal)
        (if (> (length fj--journal) fj-journal-size)
            (setq fj--journal (nbutlast fj--journal (- (length fj--journal)
                                                       fj-journal-size)))))
      (push buffer-file-name j-entry)
      (setcdr (assoc date fj--journal) j-entry)

      (if h-entry
          (incf (cdr h-entry))
        (push (cons buffer-file-name 1) fj--hitlist)
        (if (> (length fj--hitlist) fj-hitlist-size)
            (setq fj--hitlist (nbutlast fj--hitlist (- (length fj--hitlist)
                                                       fj-hitlist-size)))))
      (setq fj--hitlist (sort fj--hitlist (lambda (x y) (> (cdr x) (cdr y)))))

      (when (get-buffer "*File-Journal*")
        (with-current-buffer "*File-Journal*"
          (fj--update-fj-buffer))))))


(add-hook 'find-file-hook 'fj--record-file)


(defun fj-save-journal ()
  "Save the file journal to disk."
  (interactive)
  (with-temp-buffer
    (insert
     ";; -*- mode: emacs-lisp -*-\n"
     ";; Journal entries for visited files\n")
    (prin1 `(setq fj--journal ',fj--journal) (current-buffer))
    (insert "\n\n")
    (prin1 `(setq fj--hitlist ',fj--hitlist) (current-buffer))
    (write-region (point-min) (point-max) fj-journal-file nil
                  (unless (interactive-p) 'quiet))))

(defvar fj--save-journal-timer
  (run-with-timer
   fj-save-timer-interval fj-save-timer-interval 'fj-save-journal))

(add-hook 'kill-emacs-hook 'fj-save-journal)

(when (file-readable-p fj-journal-file)
  (load-file fj-journal-file))

;; integration with anything
(defun fj--anything-candidates ()
  "Return a list of candidates for anything."
  (reduce 'append (mapcar 'cdr fj--journal)))

(defvar fj--anything-source
  '((name . "File Journal")
    (candidates . fj--anything-candidates)
    (volatile)     ; is it really needed here?
    (type . file))
  "Anything source provided by `file-journal'.
List of recently used files.")

(defun fj--add-anything-source ()
  (when (featurep 'anything)
    (add-to-list 'anything-sources fj--anything-source t)))

(when fj-integrate-with-anything
  (add-hook 'emacs-startup-hook 'fj--add-anything-source))

(provide 'file-journal)
;;; file-journal.el ends here
