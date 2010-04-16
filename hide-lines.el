;;; hide-lines.el --- hide lines based on a regexp
;;
;; Author: Mark Hulme-Jones <ture at plig cucumber dot net>
;; Modified-by: Štěpán Němec <stepnem@gmail.com>
;; Time-stamp: "2010-04-16 23:55:29 CEST stepnem"
;; URL: http://www.emacswiki.org/emacs/HideLines
;;
;;; History
;;
;; 16/04/2010
;; - include the empty-string tip from Emacs Wiki
;; - add autoload cookie
;; - renames:
;;   `add-invisible-overlay' -> `hide-lines--add-overlay'
;;   `hide-matching-lines' -> `hide-lines-hide-matching'
;;   `hide-non-matching-lines' -> `hide-lines-hide-non-matching'
;;   `invisible-areas-list' -> `hide-lines--overlays'
;;   `show-all-invisible' -> `hide-lines-show-all'
;; - minor code and language fixes
;;
;; 24/03/2004
;; - Incorporate fix for infinite loop bug from David Hansen
;;
;;; Commentary
;;
;; The simplest way to make `hide-lines' work is to add the following
;; lines to your `user-init-file':
;;
;; (autoload 'hide-lines "hide-lines" "Hide lines based on a regexp." t)
;; (global-set-key "\C-ch" 'hide-lines)
;;
;; Now, when you type C-c h, you will be prompted for a regexp
;; (regular expression). All lines in the current buffer matching this
;; regexp will be hidden.
;;
;; Alternatively, you can type C-u C-c h (i.e. provide a prefix
;; argument to the `hide-lines' command) to hide all lines that *do
;; not* match the specified regexp.
;;
;; If you want to make all of the hidden areas re-appear again, use
;; the `hide-lines-show-all' command or just hit RET at the
;; `hide-lines' command prompt .

(defvar hide-lines--overlays ()
 "List of invisible overlays used by the `hide-lines' package.")

(add-to-invisibility-spec 'hl)

;;;###autoload
(defun hide-lines (&optional arg)
  "Hide lines matching a regexp specified interactively.
With a prefix arg, hide lines that do NOT match the specified regexp."
  (interactive "p")
  (if (> arg 1)
      (call-interactively 'hide-lines-hide-non-matching)
      (call-interactively 'hide-lines-hide-matching)))

(defun hide-lines--add-overlay (start end)
  "Add an overlay from START to END in the current buffer.
Push the overlay onto the `hide-lines--overlays' list."
  (let ((overlay (make-overlay start end)))
    (setq hide-lines--overlays (cons overlay hide-lines--overlays))
    (overlay-put overlay 'invisible 'hl)))

(defun hide-lines-hide-non-matching (regexp)
  "Hide lines that don't match the specified REGEXP string."
  (interactive "MHide lines not matching regexp: ")
  (set (make-local-variable 'line-move-ignore-invisible) t)
  (save-excursion
    (let ((start-position (goto-char (point-min)))
          (pos (re-search-forward regexp nil t)))
      (while pos
        (beginning-of-line)
        (hide-lines--add-overlay start-position (point))
        (forward-line 1)
        (setq start-position (point))
        (if (eq (point) (point-max))
            (setq pos nil)
          (setq pos (re-search-forward regexp nil t))))
      (hide-lines--add-overlay start-position (point-max)))))

(defun hide-lines-hide-matching  (regexp)
  "Hide lines matching the specified REGEXP string."
  (interactive "MHide lines matching regexp: ")
  (if (equal regexp "")
      (hide-lines-show-all)
    (set (make-local-variable 'line-move-ignore-invisible) t)
    (save-excursion
      (goto-char (point-min))
      (let ((pos (re-search-forward regexp nil t))
            start-position)
        (while pos
          (beginning-of-line)
          (setq start-position (point))
          (end-of-line)
          (hide-lines--add-overlay start-position (+ 1 (point)))
          (forward-line 1)
          (if (eq (point) (point-max))
              (setq pos nil)
            (setq pos (re-search-forward regexp nil t))))))))

(defun hide-lines-show-all ()
  "Show all areas hidden by the `hide-lines' command."
  (interactive)
  (mapc 'delete-overlay hide-lines--overlays)
  (setq hide-lines--overlays nil))

(provide 'hide-lines)
