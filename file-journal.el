;;; file-journal.el --- revisit files by date or visit count

;; Copyright (C) 2008  Tamas Patrovics
;;                     Jonathan Arkell (current mainteiner)

;; Modified-by: Štěpán Němec <stepnem@gmail.com>
;; Time-stamp: "2010-03-17 13:51:50 CET stepnem"
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

;; File-journal keeps a list of all the files you have visited, like
;; a persistent most-recently viewed list.  You can control the number
;; of days to keep the journal for.  You can also set some file patterns
;; to exclude from tracking.
;;
;; Toggle the mode with `M-x file-journal-mode' and use `M-x fj-show' to
;; revisit files by date or number of times visited.

;; You can also integrate file-journal with the anything.el package, e.g.:
;;
;; (defun anything-c-file-journal-candidates ()
;;   "Return a list of candidates for anything."
;;   (reduce 'append (mapcar 'cdr fj--journal)))

;; (defvar anything-c-source-file-journal
;;   '((name . "File Journal")
;;     (candidates . anything-c-file-journal-candidates)
;;     (type . file))
;;   "Anything source provided by `file-journal'.
;; List of recently used files.")

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
;;       - new commands/functionality:
;;         - toggle view mode (journal <-> hitlist)
;;         - mark/unmark files and open them in Dired
;;         - convenient movement bindings
;;         - exit the journal buffer
;;         - display usage information
;;       - "*file-journal*" -> "*File-Journal*"
;;       - use `find-file-hook' instead of advice to track opened files
;;       - make the journal buffer read-only
;;       - comment out the unimplemented `fj-visit-files' definition
;;       - mention the integration with `anything' just as a tip in commentary
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

;;; TODO:
;; - do we need something like `file-journal-mode-hook'?
;; - rename `fj-' -> `file-journal-'
;; ? Hook into, or replace ECB's previous files list.

;; Tested on Emacs 23.

;;; Code:

(eval-when-compile
  (require 'cl))

(defgroup file-journal nil
  "Global minor mode enabling tracking and revisiting recently
  opened files based on date or number of times visited."
  :group 'files)

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
  "List of regexps specifying which files to exclude from journal."
  :type '(repeat regexp)
  :group 'file-journal)

(defcustom fj-save-timer-interval 3600
  "Journal auto-save interval (seconds)."
  :type 'integer
  :group 'file-journal)

(defcustom fj-lighter nil
  "Mode-line string indicating that file journal is turned on.
It should start with a space, e.g. \" fj\".
Defaults to nil (no indicator)."
  :type 'string
  :group 'file-journal)

(defcustom fj-integrate-with-anything nil
  "If non-nil, add file journal to `anything-sources' automatically."
  :type 'boolean
  :group 'file-journal)

(defface fj-header-face
  '((t (:inherit highlight)))
  "Face for date headers."
  :group 'file-journal)

(defface fj-marked-face
  '((t (:inherit dired-marked)))
  "Face for marked files."
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
(define-key fj-mode-map " " 'scroll-up)
(define-key fj-mode-map "?" 'fj-usage)
(define-key fj-mode-map "d" 'fj-do-dired)
(define-key fj-mode-map "h" 'describe-mode)
(define-key fj-mode-map "k" 'fj-kill)
(define-key fj-mode-map "m" 'fj-mark-file)
(define-key fj-mode-map "n" 'next-line)
(define-key fj-mode-map "p" 'previous-line)
(define-key fj-mode-map "q" 'fj-quit)
(define-key fj-mode-map "u" 'fj-unmark-file)
(define-key fj-mode-map "v" 'fj-switch-view)
(define-key fj-mode-map (kbd "DEL") 'scroll-down)


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
        (insert " " file "\n")
        (and (member file fj--marked-files)
             (save-excursion
               (forward-line -1)
               (put-text-property (line-beginning-position)
                                  (line-end-position)
                                  'face 'fj-marked-face))))))
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
        (insert (int-to-string (cdr entry)) " " (car entry) "\n")
        (and (member (car entry) fj--marked-files)
             (save-excursion
               (forward-line -1)
               (put-text-property
                (line-beginning-position)
                (line-end-position) 'face 'fj-marked-face))))
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

(defun fj--get-file-under-cursor ()
  "Return name of the file under cursor, or signal an error."
  (if (eq fj--current-view-mode 'journal)
      (if (save-excursion
            (beginning-of-line)
            (looking-at " "))
          (buffer-substring (1+ (line-beginning-position))
                            (line-end-position))
        (error "No file on this line."))
    ;; 'hitlist
    (let ((pos (save-excursion
                 (beginning-of-line)
                 (re-search-forward "^[0-9]+\\s-+" (line-end-position) t))))
      (if pos
          (buffer-substring pos (line-end-position))
        (error "No file on this line.")))))

(defun fj-visit-file ()
  "Visit the file under cursor."
  (interactive)
  (find-file (fj--get-file-under-cursor)))

(defvar fj--marked-files nil
  "List of currently marked files in the file-journal buffer.")

(defun fj-mark-file (&optional arg)
  "Mark the file under cursor.
With a prefix argument, mark that many files."
  (interactive "p")
  (dotimes (_ arg)
    (add-to-list 'fj--marked-files (fj--get-file-under-cursor))
    (let (buffer-read-only)
      (put-text-property
       (line-beginning-position) (line-end-position) 'face 'fj-marked-face))
    (next-line)))

(defun fj-unmark-file (&optional arg)
  "Unmark the file under cursor.
With a prefix argument, unmark that many files."
  (interactive "p")
  (dotimes (_ arg)
    (setq fj--marked-files
          (delete (fj--get-file-under-cursor) fj--marked-files))
    (let (buffer-read-only)
      (remove-text-properties
       (line-beginning-position) (line-end-position) '(face nil)))
    (next-line)))

(defun fj-do-dired ()
  "Open a Dired buffer on the marked files."
  (interactive)
  (dired (cons "File journal -- marked files" fj--marked-files)))

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
      (concat "\\[fj-switch-view] (switch view), "
              "\\[fj-quit]/\\[fj-kill] (bury/kill this buffer), "
              "\\[fj-usage] (this message), "
              "\\[describe-mode] (describe mode)")))))

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

(defconst fj--used-hooks
  '((kill-emacs-hook fj-save-journal)
    (find-file-hook fj--record-file))
  "Hooks used by file-journal.")

(defvar fj--save-journal-timer nil
  "Timer used by file-journal to periodically save the file lists.")

(defun fj--enabled-p ()
  "Return non-nil if file journal is currently enabled."
  ;; (list (length find-file-hook) (length kill-emacs-hook)) ;-P
  (memq 'fj--record-file find-file-hook))

;;;###autoload
(define-minor-mode file-journal-mode
  "Toggle file-journal mode.
With prefix argument ARG, turn on if positive, otherwise off.
Return non-nil if the mode is enabled.

When file-journal mode is enabled, it maintains a list of
recently visited files ordered by date and visit count.

Use \\[fj-show] to display the list."
  :global t
  :group 'file-journal
  :lighter fj-lighter

  (unless (and file-journal-mode (fj--enabled-p))
    (if file-journal-mode
        (progn
          (when (file-readable-p fj-journal-file)
            (load-file fj-journal-file))
          (setq fj--save-journal-timer
            (run-with-timer
             fj-save-timer-interval fj-save-timer-interval 'fj-save-journal)))
      (fj-save-journal)
      (cancel-timer fj--save-journal-timer))
    (let ((func (if file-journal-mode 'add-hook 'remove-hook)))
      (dolist (hook fj--used-hooks)
        (apply func hook)))
    (when (called-interactively-p 'interactive)
      (message "File journal %sabled" (if file-journal-mode "en" "dis")))
    file-journal-mode))

;; I wonder if this is really needed -- see
;; (info "(elisp) Unloading")
(defun file-journal-unload-function ()
  "Unload file-journal."
  (file-journal-mode -1)
  nil)

(provide 'file-journal)
;;; file-journal.el ends here
