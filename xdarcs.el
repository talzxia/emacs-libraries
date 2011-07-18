;;; xdarcs.el --- Emacs integration for Darcs

;; Copyright (C) 2007 James Wright

;; Author: James Wright <james@chumsley.org>
;; Modified-by: Štěpán Němec <stepnem@gmail.com>
;; Time-stamp: <2011-07-19 01:01:42 CEST stepnem>
;; Created: 12 May 2007

;; This file is NOT part of GNU Emacs.

;; xdarcs.el is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; xdarcs.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with xdarcs.el.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This is a set of commands for integrating Darcs with Emacs (either of GNU
;; Emacs or XEmacs ought to work). It was inspired by John Wiegley and
;; Christian Neukirchen's darcsum.el.
;;
;; To get started, visit a file that is in a Darcs repository. Make some
;; changes, and then type `M-x darcs-whatsnew'. Select the hunks that you want
;; to include (space toggles inclusion), and hit `C-c C-c' to record them.

;;; Code:

(require 'xml)
(require 'timezone)
(require 'diff-mode)

(eval-and-compile
  (unless (boundp 'running-xemacs) (defconst running-xemacs (featurep 'xemacs))))

(defvar darcs-patch-responses nil
  "Patch responses for the currently-running interactive Darcs process.")
(make-variable-buffer-local 'darcs-patch-responses)

(defvar *darcs-narrow-target* nil
  "If `darcs-whatsnew' was called with TARGET-LOCATION-ONLY, contains the target that was displayed.")

(defmacro darcs-do-command-async (root-dir-options-list &rest body)
  "Run darcs asynchronously according to ROOT-DIR-OPTIONS-LIST.
Output will be sent to the current buffer. When the process
terminates, the body of the macro will be executed in the current
buffer."
  (let ((root-dir (car root-dir-options-list))
        (options (cdr root-dir-options-list)))
    `(darcs-do-command-async-fn ,root-dir (lambda () ,@body) ,@options)))

(defface darcs-blame-author-face
  '((t :inherit font-lock-function-name-face))
  "Face used to highlight the author column of blame output"
  :group 'darcs)

(defface darcs-blame-date-face
  '((t :inherit font-lock-variable-name-face))
  "Face used to highlight the date column of blame output"
  :group 'darcs)

(defface darcs-patch-name-face
  '((t :inherit font-lock-constant-face))
  "Face used to highlight patch names"
  :group 'darcs)

(defface darcs-tag-name-face
  '((((class color) (background dark))
     (:foreground "red" :bold t))
    (((class color) (background light))
     (:foreground "red" :bold t))
    (t (:bold t)))
  "Face used to highlight tag names"
  :group 'darcs)

(defface darcs-file-link-face
  '((((class color) (background dark))
     (:foreground "yellow" :bold t))
    (((class color) (background light))
     (:foreground "black" :bold t))
    (t (:bold t)))
  "Face used to highlight filename links"
  :group 'darcs)

(defface darcs-line-added-face
  '((t :inherit diff-added))
  "Face used for lines added"
  :group 'darcs)

(defface darcs-line-removed-face
  '((t :inherit diff-removed))
  "Face used for lines removed"
  :group 'darcs)

(defface darcs-header-line-face
  '((t :inherit diff-file-header))
  "Face used for header lines (eg atomic patch description)"
  :group 'darcs)

(defface darcs-excluded-patch-face
  '((((class color) (background dark))
     (:foreground "gray50"))
    (((class color) (background light))
     (:foreground "gray50"))
    (t (:bold t)))
  "Face used for patches that have been excluded"
  :group 'darcs)

(defface darcs-excluded-header-line-face
  '((((class color) (background dark))
     (:background "gray90" :strikethru t))
    (((class color) (background light))
     (:background "gray90" :strikethru t))
    (t (:bold t)))
  "Face used for header lines of excluded patches"
  :group 'darcs)

(defface darcs-excluded-patch-name-face
  '((((class color) (background dark))
     (:strikethru t))
    (((class color) (background light))
     (:strikethru t))
    (t (:bold t)))
  "Face used for header lines of excluded patches"
  :group 'darcs)


(defcustom darcs-debug nil
  "When non-nil, the *darcs output* buffer is never deleted."
  :type 'boolean
  :group 'darcs)

(defvar darcs-prefix-map
  (let ((map (make-sparse-keymap)))
    (define-key map [?a] 'darcs-add)
    (define-key map [?b] 'darcs-blame)
    (define-key map [?c] 'darcs-changes)
    (define-key map [?=] 'darcs-diff)
    (define-key map [?d] 'darcs-describe-patch)
    (define-key map [?-] 'darcs-ediff)
    (define-key map [?f] 'darcs-filelog)
    (define-key map [?h] 'darcs-filelog)
    (define-key map [?G] 'darcs-pull)
    (define-key map [?l] 'darcs-pull)
    (define-key map [?S] 'darcs-push)
    (define-key map [?u] 'darcs-push)
    (define-key map [?i] 'darcs-init)
    (define-key map [?r] 'darcs-record)
    (define-key map [(control ?r)] 'darcs-revert)
    (define-key map [?m] 'darcs-query-manifest)
    (define-key map [?q] 'darcs-query-manifest)
    (define-key map [?w] 'darcs-whatsnew)
    (define-key map [?x] 'darcs-remove)
    map)
  "The base keymap containing darcs commands.")

(defvar darcs-base-map
  (let ((map (make-sparse-keymap 'darcs-base-map)))
    (if running-xemacs (define-key map 'button2 'darcs-mouse-follow-link)
      (define-key map [mouse-2] 'darcs-mouse-follow-link))
    map)
  "Base keymap for darcs buffers.  For many this will be sufficient.")

(defvar darcs-link-map
  (let ((map (make-sparse-keymap)))
    (suppress-keymap map)
    (define-key map [?q] 'darcs-quit-current)
    (define-key map [?\r] 'darcs-follow-link)
    (if running-xemacs (define-key map 'button2 'darcs-mouse-follow-link)
      (define-key map [mouse-2] 'darcs-mouse-follow-link))
    map)
  "Keymap for darcs links.")

(defvar darcs-patch-display-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map darcs-link-map)
    (define-key map [?\ ] 'darcs-toggle-patch-included)
    (define-key map [?\r] 'darcs-toggle-patch-expanded)
    (define-key map [tab] 'darcs-toggle-patch-expanded)
    (define-key map [(control return)] 'darcs-find-patch-in-other-window)
    (define-key map [?n] 'darcs-next-patch)
    (define-key map [?p] 'darcs-prev-patch)
    (define-key map [?y] 'darcs-include-patch)
    (define-key map [?x] 'darcs-exclude-patch)
    (define-key map [?s] 'darcs-exclude-all-in-current-file)
    (define-key map [?f] 'darcs-include-all-in-current-file)
    (define-key map [?a] 'darcs-expand-all-patches)
    (define-key map [?z] 'darcs-collapse-all-patches)
    (define-key map [?Y] 'darcs-include-all-patches)
    (define-key map [?X] 'darcs-exclude-all-patches)
    (define-key map [?j] 'darcs-next-named-patch)
    (define-key map [?k] 'darcs-prev-named-patch)
    (define-key map [?N] 'darcs-next-named-patch) ;??? Should we keep N and P?
    (define-key map [?P] 'darcs-prev-named-patch)
    (define-key map [?A] 'darcs-expand-only-named-patches)
    map)
  "Keymap for displaying lists of atomic patches.")

(defvar darcs-record-map
  (let ((map (make-sparse-keymap 'darcs-record-map)))
    (set-keymap-parent map darcs-base-map)
    (define-key map [(control ?c) (control ?c)] 'darcs-commit-record)
    (define-key map [(control ?x) ?#] 'darcs-commit-record)
    map)
  "Keymap for `darcs-record-mode'.")

(defvar darcs-whatsnew-map
  (let ((map (make-sparse-keymap 'darcs-whatsnew-map)))
    (set-keymap-parent map darcs-base-map)
    (define-key map [(control ?c) (control ?c)] 'darcs-record-from-whatsnew)
    (define-key map [(control ?c) (control ?r)] 'darcs-commit-revert)
    (define-key map [(control ?x) ?#] 'darcs-record-record-from-whatsnew)
    map)
  "Keymap for `darcs-whatsnew-mode'.")

(defvar darcs-revert-map
  (let ((map (make-sparse-keymap 'darcs-revert-map)))
    (set-keymap-parent map darcs-base-map)
    (define-key map [(control ?c) (control ?r)] 'darcs-commit-revert)
    map)
  "Keymap for `darcs-revert-mode'.")

(defvar darcs-pull-map
  (let ((map (make-sparse-keymap 'darcs-pull-map)))
    (set-keymap-parent map darcs-base-map)
    (define-key map [(control ?c) (control ?c)] 'darcs-commit-pull)
    (define-key map [(control ?x) ?#] 'darcs-commit-pull)
    map)
  "Keymap for `darcs-pull-mode'.")

(defvar darcs-push-map
  (let ((map (make-sparse-keymap 'darcs-push-map)))
    (set-keymap-parent map darcs-base-map)
    (define-key map [(control ?c) (control ?c)] 'darcs-commit-push)
    (define-key map [(control ?x) ?#] 'darcs-commit-push)
    map)
  "Keymap for `darcs-push-mode'.")

(defun darcs-make-link-overlay (start end action)
  "Make a button overlay from START to END with ACTION."
  (let ((ov (make-overlay start end)))
    (overlay-put ov 'mouse-face 'highlight)
    (overlay-put ov 'read-only t)
    (overlay-put ov 'darcs-select-action action)
    (xdarcs-set-overlay-keymap ov darcs-link-map)
    ov))

(defvar darcs--orig-wincfg nil "Window configuration before invoking xdarcs.")
(defun darcs-quit-current ()
  "Hide the current buffer."
  (interactive)
  (set-window-configuration darcs--orig-wincfg)
  (setq darcs--orig-wincfg nil))

(defun darcs-follow-link ()
  "In the other window, perform the `darcs-select-action' property of the overlay nearest point."
  (interactive)
  (let* ((ov (xdarcs-overlay-at (point) 'darcs-select-action))
         (action (overlay-get (or ov (error "No link on current line"))
                              'darcs-select-action)))
    (apply (car action) (cdr action))))

(defun darcs-mouse-follow-link (evt)
  "Function to translate mouse clicks to character events."
  (interactive "e")
  (let ((win (event-window evt)) (pnt (event-point evt)))
    (select-window win)
    (goto-char pnt)
    (darcs-follow-link)))

(defvar darcs-editable-patch-name-overlay nil
  "Overlay we use to highlight the patch name in a darcs record buffer.")
(make-variable-buffer-local 'darcs-editable-patch-name-overlay)

(defun darcs-pre-idle-hook ()
  "Display tool-tips on active overlays when point is over them, and maintain font-locking."
  (when darcs-editable-patch-name-overlay
    (save-excursion
      (goto-char (point-min))
      (move-overlay darcs-editable-patch-name-overlay
                    (point-at-bol) (point-at-eol))))
  (let ((ov (xdarcs-overlay-at (point) 'darcs-tool-tip)))
    (when ov (message "%s" (overlay-get ov 'darcs-tool-tip)))))

(add-hook 'post-command-hook 'darcs-pre-idle-hook)

(defvar darcs-exclude-enabled-function (lambda (ov) t)
  "Function called to determine whether `darcs-include-patch' and `darcs-exclude-patch' functions should be permitted on a given overlay.")
(make-variable-buffer-local 'darcs-exclude-enabled-function)

(defun darcs-nearest-patch ()
  "Return the patch nearest to point."
  (or (xdarcs-overlay-at (point) 'darcs-patch-ov)
      (progn (beginning-of-line-text) (xdarcs-overlay-at (point) 'darcs-patch-ov))
      (darcs-move-to-patch -1)
      (error "no patch around point")))

(defun darcs-toggle-patch-included ()
  "If a patch is included, then exclude it; else re-include it."
  (interactive)
  (let ((ov (darcs-nearest-patch)))
    (unless (funcall darcs-exclude-enabled-function ov)
      (error "`darcs-exclude-patch' not enabled for this patch"))
    (if (overlay-get ov 'patch-excluded)
        (darcs-include-patch t)
      (darcs-exclude-patch t))))

(defun darcs-exclude-patch (&optional recursive-p)
  "Exclude the current patch and skip to the next patch."
  (interactive)
  (let ((ov (darcs-nearest-patch)))
    (unless (or recursive-p
                (funcall darcs-exclude-enabled-function ov))
      (error "`darcs-exclude-patch' is not enabled for this patch"))
    (let ((desc-ov (overlay-get ov 'darcs-patch-ov)))
      (overlay-put ov 'patch-excluded t)
      (xdarcs-set-overlay-face ov (if (darcs-named-patch-p ov)
                                      'darcs-excluded-patch-name-face
                                    'darcs-excluded-header-line-face))
      (xdarcs-set-overlay-face desc-ov 'darcs-excluded-patch-face)
      (xdarcs-set-overlay-priority desc-ov 10)

      (when (darcs-named-patch-p ov)
        (save-restriction
          (save-excursion
            (narrow-to-region (overlay-start desc-ov) (overlay-end desc-ov))
            (goto-char (overlay-start ov))
            (when (darcs-move-to-patch 1)
              (darcs-on-all-patches
               (lambda (ov)
                 (unless (darcs-named-patch-p ov)
                   (darcs-exclude-patch t))))))))

      (darcs-collapse-patch)
      (unless recursive-p
        (if (darcs-named-patch-p ov)
            (darcs-next-named-patch)
          (darcs-next-patch))))))

(defun darcs-include-patch (&optional recursive-p)
  "Include the current patch and skip to the next patch."
  (interactive)
  (let* ((ov (darcs-nearest-patch))
         (desc-ov (overlay-get ov 'darcs-patch-ov)))
    (unless (or recursive-p
                (funcall darcs-exclude-enabled-function ov))
      (error "`darcs-include-patch' not enabled for this patch"))
    (overlay-put ov 'patch-excluded nil)
    (xdarcs-set-overlay-face ov (if (darcs-named-patch-p ov)
                                    'darcs-patch-name-face
                                  'darcs-header-line-face))
    (xdarcs-set-overlay-face (overlay-get ov 'darcs-patch-ov) nil)
    (darcs-expand-patch)
    (when (darcs-named-patch-p ov)
      (save-restriction
        (save-excursion
          (narrow-to-region (overlay-start desc-ov) (overlay-end desc-ov))
          (goto-char (overlay-start ov))
          (when (darcs-move-to-patch 1)
            (darcs-on-all-patches
             (lambda (ov)
               (unless (darcs-named-patch-p ov)
                 (darcs-include-patch t)))))
          (goto-char (point-min))
          (darcs-collapse-all-atomic-patches))))
    (unless recursive-p
      (if (darcs-named-patch-p ov)
          (darcs-next-named-patch)
        (darcs-next-patch)))))

(defun darcs-patch-collapsed-p ()
  "Return non-nil if patch at point is collapsed."
  (let* ((ov (darcs-nearest-patch)))
    (= ?\^M (char-after (or (overlay-get ov 'darcs-collapse-point)
                            (overlay-end ov))))))

(defun darcs-toggle-patch-expanded ()
  "Expands or collapses the current patch."
  (interactive)
  (save-excursion
    (if (darcs-patch-collapsed-p)
        (darcs-expand-patch)
      (darcs-collapse-patch))))

(defun darcs-flag-patch (flag-char)
  "Set all newlines to ^M or vice versa.
\(if FLAG-CHAR is ?\n, set all to ?\n). Applies to the
description region of the current patch."
  (let* ((inhibit-read-only t)
         (ov (darcs-nearest-patch))
         (desc-ov (overlay-get ov 'darcs-patch-ov))
         (collapse-point (overlay-get ov 'darcs-collapse-point)))
    ;; A little bit of hackery here. We assume that the collapse-point
    ;; precedes a space; we convert that space to a ^M to hide the rest of the
    ;; line. When expanding, we convert it back to a space. If collapse-point
    ;; ever precedes a non-space we're screwed, so include an explicit check.
    (when collapse-point
      (save-excursion
        (goto-char collapse-point)
        (delete-char 1)
        (if (= flag-char ?\n) (insert " ")
          (unless (looking-at " ")
            (error "Assertion failed: (looking-at \" \")"))
          (insert "\r"))))
    (subst-char-in-region (or collapse-point (overlay-end ov))
                          (overlay-end desc-ov)
                          (if (= flag-char ?\n) ?\^M ?\n) flag-char)))

(defun darcs-expand-patch ()
  "Expand the current patch."
  (interactive)
  (let* ((ov (darcs-nearest-patch))
         (desc-ov (overlay-get ov 'darcs-patch-ov)))

    (darcs-flag-patch ?\n)

    ;; More special-case hackery.  If we expand a named patch, collapse all its children afterward.
    (when (darcs-named-patch-p ov)
      (save-excursion
        (save-restriction
          (narrow-to-region (overlay-start desc-ov) (overlay-end desc-ov))
          (darcs-collapse-all-atomic-patches))))))

(defun darcs-collapse-patch ()
  "Hide the current patch."
  (interactive)
  (darcs-flag-patch ?\^M))

(defun darcs-find-patch-in-other-window ()
  "Open the file associated with the nearest patch in the other window and move point to the associated line, if any."
  (interactive)
  (let ((root-dir (darcs-root-directory default-directory))
        (ov (darcs-nearest-patch)))
    (when ov
      (let ((file (darcs-associated-file root-dir (xdarcs-overlay-string ov)))
            (line (darcs-associated-line root-dir (xdarcs-overlay-string ov))))
        (unless file
          (error "No file associated with change '%s'" (xdarcs-overlay-string ov)))
        (find-file-other-window file)
        (when line (goto-line line))))))

(defun darcs-move-to-patch (delta)
  "Move to the next (DELTA = 1) or previous (DELTA = -1) patch.
Skips over intermediate patches when (> (abs DELTA) 1)"
  (interactive)
  (when (zerop delta)
    (error "DELTA must not be 0"))
  (let ((orig-point (point))
        (ov nil))
    (goto-char (point-at-bol))
    (while (and (null ov)
                (zerop (forward-line delta))
                (/= (point) (point-max)))
      (beginning-of-line-text)
      (setq ov (xdarcs-overlay-at (point) 'darcs-patch-ov)))
    (if (and ov (/= (point) orig-point))
        ov
      (goto-char orig-point)
      nil)))

;; (defun darcs-maybe-recenter ()
;;  "Recenter if necessary to bring the current patch into full view"
;;  (let* ((ov (darcs-nearest-patch))
;;         (desc-ov (overlay-get ov 'darcs-patch-ov))
;;         (ws (line-number-at-pos (window-start)))
;;         (we (line-number-at-pos (window-end)))
;;         (l (line-number-at-pos))
;;         (oe (overlay-end desc-ov)))
;;    (when (> oe we)
;;      (let ((top (- (- l ws) (- oe we))))
;;      (message (format "Recentering at %d or %d" top 5))
;;      (recenter (max top 5))))))


(defun darcs-maybe-recenter (&optional median-height)
  "Recenter if we are more than MEDIAN-HEIGHT lines from the top of the buffer."
  (setq median-height (or median-height (/ (window-body-height) 4)))
  (let ((median-line (+ (line-number-at-pos (window-start))
                        median-height)))
    (when (> (line-number-at-pos) median-line)
      (recenter median-height))))

(defun darcs-next-patch ()
  "Move point to the beginning of the next patch heading."
  (interactive)
  (if (darcs-move-to-patch 1)
      (darcs-maybe-recenter)
    (message "No more patches")))

(defun darcs-prev-patch ()
  "Move point to the beginning of the previous patch heading."
  (interactive)
  (if (darcs-move-to-patch -1)
      (darcs-maybe-recenter)
    (message "No more patches")))

(defun darcs-named-patch-p (ov)
  "Return non-NIL if OV is an overlay representing a named patch."
  ;; only named patches have a collapse-point
  (overlay-get ov 'darcs-collapse-point))

(defun darcs-next-named-patch ()
  "Move point to the beginning of the next named patch."
  (interactive)
  (let ((orig-point (point))
        (ov (darcs-move-to-patch 1)))
    (while (and ov
                (not (darcs-named-patch-p ov)))
      (setq ov (darcs-move-to-patch 1)))
    (if ov
        (darcs-maybe-recenter)
      (goto-char orig-point)
      (message "No more named patches"))))

(defun darcs-prev-named-patch ()
  "Move point to the beginning of the next named patch."
  (interactive)
  (let ((orig-point (point))
        (ov (darcs-move-to-patch -1)))
    (while (and ov
                (not (darcs-named-patch-p ov)))
      (setq ov (darcs-move-to-patch -1)))
    (if ov
        (darcs-maybe-recenter)
      (goto-char orig-point)
      (message "No more named patches"))))

(defun darcs-on-all-patches (thunk)
  "Evaluate THUNK with point set to the beginning of each patch in the current buffer."
  (save-excursion
    (goto-char (point-min))
    (let ((ov (or (xdarcs-overlay-at (point) 'darcs-patch-ov)
                  (darcs-move-to-patch 1))))
      (while ov
        (funcall thunk ov)
        (setq ov (darcs-move-to-patch 1))))))

(defun darcs-collapse-all-patches ()
  "Collapse all patches in the current buffer."
  (interactive)
  (darcs-on-all-patches (lambda (ov) (darcs-flag-patch ?\^M))))

(defun darcs-expand-all-patches ()
  "Expand all patches in the current buffer."
  (interactive)
  (darcs-on-all-patches (lambda (ov) (darcs-flag-patch ?\n))))

(defun darcs-include-all-patches ()
  "Include all patches in the current buffer."
  (interactive)
  (darcs-on-all-patches (lambda(ov) (darcs-include-patch t))))

(defun darcs-exclude-all-patches ()
  "Exclude all patches in the current buffer."
  (interactive)
  (darcs-on-all-patches (lambda(ov) (darcs-exclude-patch t))))

(defun darcs-collect-patch-responses ()
  "Return a list of cells of the form (PATCH-DESC . PLIST).
PATCH-DESC is a string describing the patch (eg. \"hunk
./notes/darcs-mode 35\") and PLIST contains two properties:
:INCLUDED = non-NIL for included patches
:EXPANDED = non-NIL for expanded patches"
  (let ((responses nil))
    (darcs-on-all-patches (lambda (ov)
                            (push (list (xdarcs-overlay-string ov)
                                        :named (darcs-named-patch-p ov)
                                        :included (not (overlay-get ov 'patch-excluded))
                                        :expanded (not (darcs-patch-collapsed-p)))
                                  responses)))
    responses))

(defun darcs-apply-patch-responses (patch-responses)
  "Ensure every patch in the current buffer is excluded if not in PATCH-RESPONSES."
  ;; ??? make number of patches etc. match??
  (darcs-on-all-patches (lambda (ov)
                          (let ((cell (assoc (xdarcs-overlay-string ov) patch-responses)))
                            (when cell
                              (if (xdarcs-plist-get-with-default (cdr cell) :included t)
                                  (darcs-include-patch t)
                                (darcs-exclude-patch t))
                              (if (xdarcs-plist-get-with-default (cdr cell) :expanded t)
                                  (darcs-expand-patch)
                                (darcs-collapse-patch)))))))

(defun darcs-on-all-henceforth-patches-in-current-file (thunk)
  "Apply THUNK with point on the current patch, and on each subsequent patch of the same file.
On completion, point will be either on the last patch, or on the
first subsequent patch associated with a different file."
  (let* ((ov (darcs-nearest-patch))
         (file (when (and ov (not (darcs-named-patch-p ov)))
                 (darcs-associated-file default-directory
                                        (xdarcs-overlay-string ov)))))
    (while (and ov file
                (not (darcs-named-patch-p ov))
                (string= file (darcs-associated-file default-directory
                                                     (xdarcs-overlay-string ov))))
      (funcall thunk)
      (setq ov (darcs-move-to-patch 1)))))

(defun darcs-include-all-in-current-file ()
  "Include current patch and all following patches in the same file."
  (interactive)
  (unless (funcall darcs-exclude-enabled-function (darcs-nearest-patch))
    (error "`darcs-include-patch' not enabled for this patch"))
  (darcs-on-all-henceforth-patches-in-current-file
   (lambda ()
     (darcs-include-patch t))))

(defun darcs-exclude-all-in-current-file ()
  "Exclude current patch and all following patches in the same file."
  (interactive)
  (unless (funcall darcs-exclude-enabled-function (darcs-nearest-patch))
    (error "`darcs-exclude-patch' not enabled for this patch"))
  (darcs-on-all-henceforth-patches-in-current-file
   (lambda ()
     (darcs-exclude-patch t))))

(defun darcs-collapse-all-atomic-patches ()
  "Exclude all atomic (ie, unnamed) patches while leaving named patches unchanged."
  (darcs-on-all-patches
   (lambda (ov)
     (unless (darcs-named-patch-p ov)
       (darcs-flag-patch ?\^M)))))

(defun darcs-expand-only-named-patches ()
  "Expand all named patches but collapse all others."
  (interactive)
  (darcs-on-all-patches
   (lambda (ov)
     (if (darcs-named-patch-p ov)
         (darcs-expand-patch)
       (darcs-collapse-patch)))))

;;;;; XML format
;;;
;;; The XML produced by 'darcs annotate' appears to have the following features:
;;; a single tag of the form
;;;
;;;    <modified><modified_how></<modified_how><patch></patch></modified>
;;;
;;; describing the most-recent patch to be applied to the file, followed by several of
;;;
;;;    <normal_line><added_by><patch></patch></added_by> ...text... </normal_line>
;;;
;;; for lines that are part of the file due to previous (ie, not the most-recent) patches, plus
;;; several of
;;;
;;;    <added_line> ...text... </added_line>
;;;    <removed_line> ...text... </removed_line>
;;;
;;; for lines that were added or removed by the most-recent patch.

;;;;; code

(defun darcs-blame (file)
  "Evaluate the darcs annotate command on FILE and outputs it with author and date annotations."
  (interactive (list (or (buffer-file-name) default-directory)))
  (unless (darcs-file-registered-p file)
    (if (darcs-root-directory file)
        (error (format "%s is not part of darcs repo at %s" file (darcs-root-directory file)))
      (error (format "No darcs repo at or around %s" (file-name-directory file)))))
  (let* ((root-dir (darcs-root-directory file))
         (data (with-temp-buffer
                 (darcs-do-command root-dir
                                   "annotate"
                                   (darcs-canonical-name file)
                                   "--xml")
                 (xml-parse-region (point-min) (point-max))))
         (inhibit-read-only t))
    (switch-to-buffer (darcs-format-buffername 'blame (file-name-nondirectory file)))
    (erase-buffer)
    (darcs-set-mode-for-file file)
    (let ((modified-tag (car (xdarcs-xml-get-children* (car data) 'modified))))
      (dolist (child (xml-node-children (car data)))
        (when (and (listp child)
                   (or (eq 'normal_line (xml-node-name child))
                       (eq 'added_line (xml-node-name child))))
          (let* ((chg-spec (or (car (xdarcs-xml-get-children* child 'added_by))
                               modified-tag))
                 (patch-tag (car (xdarcs-xml-get-children* chg-spec 'patch)))
                 (local-date (xml-get-attribute patch-tag 'local_date))
                 (author (xml-substitute-special
                          (xml-get-attribute patch-tag 'author)))
                 (patch-name (darcs-xml-node-text
                              (car (xdarcs-xml-get-children* patch-tag 'name))))
                 (hash (xml-get-attribute patch-tag 'hash))
                 (line (darcs-xml-node-text child)))
            (when (> (length line) 0)
              (let (e pa1 pa2 pd1 pd2 pn1 pn2 author-ov date-ov name-ov all-ov)

                (setq pd1 (point))
                (insert (substring (darcs-cook-date local-date) 0 11))
                (setq pd2 (point))

                (insert " ")
                (setq pa1 (point))
                (insert (format "%-7.7s" author))
                (setq pa2 (point))

                (insert " ")
                (setq pn1 (point))
                (insert (format "%-15.15s" patch-name))
                (setq pn2 (point))

                (insert ": ")
                (setq e (point))

                (insert (darcs-trim-newlines line) "\n")

                (setq author-ov (make-overlay pa1 pa2))
                (setq date-ov   (make-overlay pd1 pd2))
                (setq name-ov   (make-overlay pn1 pn2))
                (setq all-ov    (darcs-make-link-overlay
                                 pd1 e (list 'darcs-describe-patch root-dir patch-name hash)))

                (overlay-put all-ov 'darcs-tool-tip
                             (format "%s   [%s  %s]"
                                     patch-name
                                     (darcs-cook-date local-date)
                                     author))

                (xdarcs-set-overlay-priority date-ov   5)
                (xdarcs-set-overlay-priority author-ov 5)
                (xdarcs-set-overlay-priority name-ov   5)

                (xdarcs-set-overlay-face author-ov 'darcs-blame-author-face)
                (xdarcs-set-overlay-face date-ov   'darcs-blame-date-face)
                (xdarcs-set-overlay-face name-ov   'darcs-patch-name-face))))))
      (goto-char (point-min)))))

(defun darcs-add (filename)
  "Add FILENAME to the nearest darcs repository."
  (interactive (list (or (buffer-file-name) default-directory)))
  (let ((root-dir (darcs-root-directory filename))
        (canonical-name (darcs-canonical-name filename)))
    (unless root-dir
      (error (format "No darcs repo at or around %s" (file-name-directory filename))))
    (with-temp-buffer
      (unless (zerop (darcs-do-command root-dir "add" canonical-name))
        (error (xdarcs-buffer-string-single-line)))
      (message "Added %s to darcs repo %s" canonical-name root-dir)
      (darcs-refresh-query-manifest))))

(defun darcs-remove (filename)
  "Remove FILENAME from the nearest darcs repository."
  (interactive (list (or (buffer-file-name) default-directory)))
  (let ((root-dir (darcs-root-directory filename))
        (canonical-name (darcs-canonical-name filename)))
    (unless root-dir
      (error (format "No darcs repo at or around %s" (file-name-directory filename))))
    (with-temp-buffer
      (unless (zerop (darcs-do-command root-dir "remove" canonical-name))
        (error (xdarcs-buffer-string-single-line)))
      (message "Removed %s from darcs repo %s" canonical-name root-dir)
      (darcs-refresh-query-manifest))))

(defun darcs-query-manifest (file-or-dir &optional recursive-p)
  "Show the files managed in the repo at or around FILE-OR-DIR."
  (interactive (list (or (buffer-file-name) default-directory)))
  (let ((root-dir (darcs-root-directory file-or-dir))
        (inhibit-read-only t))
    (unless root-dir
      (error (format "No darcs repo at or around %s" (file-name-directory file-or-dir))))
    (save-excursion
      (darcs-set-buffer 'query-manifest root-dir recursive-p)
      (erase-buffer)
      (dolist (file (darcs-manifest file-or-dir))
        (let (p1 p2 ov)
          (setq p1 (point))
          (insert (format "%s" file))
          (setq p2 (point))
          (insert "\n")
          (setq ov (darcs-make-link-overlay p1 p2 (list 'find-file-other-window (expand-file-name (concat root-dir file)))))
          (xdarcs-set-overlay-face ov 'bold)))
      (when (= (point-min) (point-max))
        (insert "No files managed in this repo"))
      (unless recursive-p
        (goto-char (point-min))))))

(defun darcs-refresh-query-manifest ()
  "Refresh the appropriate query-manifest window if it exists (based on the current buffer's default directory)."
  (let ((root-dir (darcs-root-directory default-directory)))
    (save-excursion
      (when (get-buffer (darcs-format-buffername 'query-manifest root-dir))
        (darcs-query-manifest root-dir t)))))

(defvar darcs-patch-headers-re
  (regexp-opt
   '("hunk" "replace" "binary" "addfile" "adddir" "rmfile" "rmdir" "move"
     "changepref" "merger" "regrem" "conflict" "tcilfnoc"))
  "All the different kinds of atomic patches that can be part of a patch.")

(defun darcs-describe-patch (file-or-dir patch-name &optional patch-hash)
  "Describe a particular patch."
  (interactive (list (or (buffer-file-name) default-directory)
                     (read-string "Patch name/regexp: ")))
  (let ((root-dir (darcs-root-directory file-or-dir))
        (inhibit-read-only t))
    (unless root-dir
      (error (format "No darcs repo at or around %s" (file-name-directory file-or-dir))))
    (darcs-set-buffer 'describe (or patch-name patch-hash))
    (erase-buffer)
    (setq darcs-exclude-enabled-function (lambda (ov) nil))
    (darcs-do-command root-dir
                      "annotate"
                      (if patch-hash (format "--match=hash %s" patch-hash)
                        (format "--patch=%s" patch-name))
                      "-u")
    (goto-char (point-min))
    (save-excursion (darcs-markup-patch-descriptions))))

(defun darcs-whatsnew (location &optional recursive-p target-location-only)
  "Show all unrecorded changes in the specified repo.
If RECURSIVE-P is non-NIL, updates an existing buffer without
necessarily displaying it. If TARGET-LOCATION-ONLY is non-NIL,
only shows differences for LOCATION."
  (interactive (list (or (buffer-file-name) default-directory)))
  (let ((root-dir (darcs-root-directory location))
        (inhibit-read-only t))
    (unless root-dir
      (error (format "No darcs repo at or around %s" (file-name-directory location))))
    (darcs-set-buffer 'whatsnew root-dir recursive-p)
    (erase-buffer)
    (if target-location-only
        (set (make-local-variable '*darcs-narrow-target*) (darcs-canonical-name location))
      (set (make-local-variable '*darcs-narrow-target*) nil))
    (save-excursion
      (unless (zerop (darcs-do-command root-dir "whatsnew" "-u" *darcs-narrow-target*))
        (xdarcs-set-overlay-keymap (make-overlay (point-min) (point-max)) darcs-patch-display-map)
        (toggle-read-only 1)
        (unless recursive-p
          (message (xdarcs-buffer-string-single-line))))
      (goto-char (point-min))
      (darcs-markup-patch-descriptions))
    (or (progn (beginning-of-line-text) (xdarcs-overlay-at (point) 'darcs-patch-ov))
        (darcs-move-to-patch -1)
        (darcs-move-to-patch 1))))

(defun darcs-refresh-whatsnew ()
  "Refresh the whatsnew window without necessarily displaying it."
  (let ((root-dir (darcs-root-directory default-directory))
        (patch-responses nil))
    ;; TODO since we destroy and then recreate the entire contents of the
    ;; window, `save-excursion' doesn't seem to work the way we intended. We
    ;; might want to behave differently depending on whether the list of
    ;; changes is "congruent" (ie, is this basically the same set of patches?)
    (save-excursion
      (when (get-buffer (darcs-format-buffername 'whatsnew root-dir))
        (set-buffer (darcs-format-buffername 'whatsnew root-dir))
        (setq patch-responses (darcs-collect-patch-responses))
        (darcs-whatsnew (or *darcs-narrow-target* root-dir) t *darcs-narrow-target*)
        (darcs-apply-patch-responses patch-responses)))))

(defun darcs-record-from-whatsnew ()
  "Invoke `darcs-record' with patch inclusion/exclusion pre-populated based on the setup of the whatsnew window."
  (interactive)
  (let ((patch-responses (darcs-collect-patch-responses))
        (narrow-target *darcs-narrow-target*))
    (darcs-record default-directory t nil nil *darcs-narrow-target*)
    (darcs-apply-patch-responses patch-responses)
    (set (make-local-variable '*darcs-narrow-target*) narrow-target)))

(defun darcs-diff (location)
  "Show all unrecorded differences at LOCATION."
  (interactive (list (buffer-file-name)))
  (darcs-whatsnew location nil t))

(defun darcs-ediff (location)
  "Show the unrecorded differences at LOCATION in an ediff session."
  (interactive (list (or (buffer-file-name) default-directory)))
  (let ((root-dir (darcs-root-directory location)))
    (unless root-dir
      (error (format "No darcs repo at or around %s" (file-name-directory location))))
    (darcs-do-interactive-command root-dir nil
                                  "diff"
                                  "--diff-command=ediff %1 %2"
                                  (darcs-canonical-name location))))

(defvar darcs-record-buffer-instructions "***END OF DESCRIPTION***
Place the long patch description above the ***END OF DESCRIPTION*** marker.
The first line of this file will be the patch name.
The patch will contain all the 'included' changes below.

Type x on a change to exclude it from this patch.
Type y on a change to include it.

Type C-c C-c to submit the patch and exit the record buffer.
Type C-x k to abandon this record buffer.


This patch contains the following changes:
"
  "Instructions for using the record buffer.")

(defvar darcs-placeholder-patch-name "<enter patch name>"
  "Placeholder patch name for when the user hasn't specified one.")

(defun darcs-record (repo-dir &optional same-window patch-name patch-description target-location)
  "Display a buffer for describing a patch and choosing what changes will be included in it.
If SAME-WINDOW is nil (the usual case), displays in the 'other'
window; otherwise displays in the current window. If PATCH-NAME
and PATCH-DESCRIPTION are provided, they will be inserted."
  (interactive (list (or (buffer-file-name) default-directory)))
  (let ((root-dir (darcs-root-directory repo-dir))
        (inhibit-read-only t)
        (starting-point nil))
    (unless root-dir
      (error (format "No darcs repo at or around %s" (file-name-directory repo-dir))))
    (save-some-buffers)
    (setq patch-name (or patch-name
                         (read-string "What is the patch name? ")))
    (darcs-set-buffer 'record root-dir (when same-window 'same-window))
    (erase-buffer)
    (insert (if (zerop (length patch-name)) darcs-placeholder-patch-name patch-name))
    (setq darcs-editable-patch-name-overlay (make-overlay (point-at-bol) (point-at-eol)))
    (xdarcs-set-overlay-face darcs-editable-patch-name-overlay 'darcs-patch-name-face)
    (insert "\n")
    (if patch-description
        (insert patch-description)
      (insert "\n")
      (setq starting-point (if (zerop (length patch-name)) (point-min) (point)))
      (insert "\n\n"))
    (save-excursion
      (insert darcs-record-buffer-instructions)
      (unless (zerop (darcs-do-command root-dir "whatsnew" "-u" target-location))
        (xdarcs-set-overlay-keymap (make-overlay (point-min) (point-max)) darcs-patch-display-map)
        (toggle-read-only 1)
        (error (xdarcs-buffer-string-single-line))))
    (darcs-markup-patch-descriptions)
    (goto-char (or starting-point (point-min)))))

(defun darcs-refresh-record ()
  "Refresh the record window without necessarily displaying it."
  (interactive)
  (let ((root-dir (darcs-root-directory default-directory))
        (patch-responses nil)
        (patch-name (save-excursion
                      (goto-char (point-min))
                      (buffer-substring (point) (point-at-eol))))
        (patch-description (save-excursion
                             (goto-char (point-min))
                             (forward-line)
                             (let ((s (point)))
                               (re-search-forward (regexp-quote darcs-record-buffer-instructions))
                               (buffer-substring s (match-beginning 0))))))
    (save-excursion
      (when (get-buffer (darcs-format-buffername 'record root-dir))
        (set-buffer (darcs-format-buffername 'record root-dir))
        (setq patch-responses (darcs-collect-patch-responses))
        (darcs-record root-dir t patch-name patch-description *darcs-narrow-target*)
        (darcs-apply-patch-responses patch-responses)))))

(defvar darcs-comment-filename nil
  "Name of the tempfile that contains the comment for the most-recently commited record.")

(defun darcs-commit-record ()
  "Commit the patch described by the current buffer."
  (interactive)
  (let ((patch-name (save-excursion
                      (goto-char (point-min))
                      (buffer-substring (point-at-bol) (point-at-eol)))))
    (when (string= patch-name darcs-placeholder-patch-name)
      (goto-char (point-min))
      (error "Please enter a name for this patch")))
  (let ((root-dir default-directory)
        (patch-responses (darcs-collect-patch-responses))
        (logfile-end (save-excursion
                       (goto-char (point-min))
                       (re-search-forward (regexp-quote darcs-record-buffer-instructions))
                       (goto-char (match-beginning 0))
                       ;; skip all but one trailing newline
                       (while (save-excursion
                                (forward-line -2)
                                (looking-at "\n\n"))
                         (forward-line -1))
                       (point)))
        (comment-filename (make-temp-name "darcs-record-")))

    (set (make-local-variable 'darcs-comment-filename)
         (expand-file-name (concat root-dir comment-filename)))
    (write-region (point-min) logfile-end darcs-comment-filename)
    (add-hook 'kill-buffer-hook 'darcs-delete-comment-filename nil t)
    (darcs-do-interactive-command root-dir patch-responses
                                  "record" *darcs-narrow-target* (format "--logfile=%s" comment-filename))))

(defun darcs-revert (repo-dir &optional same-window)
  (interactive (list (or (buffer-file-name) default-directory)))
  (let* ((root-dir (darcs-root-directory repo-dir))
         (inhibit-read-only t))
    (unless root-dir
      (error (format "No darcs repo at or around %s" (file-name-directory repo-dir))))
    (save-some-buffers)
    (darcs-set-buffer 'revert root-dir (when same-window 'same-window))
    (setq default-directory root-dir)
    (erase-buffer)
    (insert "Select which patches to revert.\nType C-c C-r to revert the included patches.\nType C-x k to abandon revert.\n")
    (save-excursion
      (unless (zerop (darcs-do-command root-dir "whatsnew" "-u"))
        (xdarcs-set-overlay-keymap (make-overlay (point-min) (point-max)) darcs-patch-display-map)
        (toggle-read-only 1)
        (error (xdarcs-buffer-string-single-line))))
    (darcs-markup-patch-descriptions)
    (darcs-exclude-all-patches)
    (goto-char (point-min))
    (darcs-next-patch)))

(defun darcs-commit-revert ()
  "Revert the patches included in the current buffer."
  (interactive)
  (let ((root-dir default-directory)
        (patch-responses (darcs-collect-patch-responses)))
    (when (yes-or-no-p "Do you really want to revert these changes? ")
      (darcs-do-interactive-command root-dir patch-responses
                                    "revert" *darcs-narrow-target*))))

(defun darcs-refresh-responded ()
  "Call `revert-buffer' on each buffer visiting a \"responded to\" file.
\"Responded to\" includes all files referenced in
`darcs-patch-responses')."
  (let* ((root-dir (darcs-root-directory default-directory))
         (files (mapcar (lambda (cell)
                          (when (and (xdarcs-plist-get-with-default (cdr cell) :included t)
                                     (not (xdarcs-plist-get-with-default (cdr cell) :named nil)))
                            (darcs-associated-file root-dir (car cell))))
                        darcs-patch-responses)))
    (dolist (buffer (buffer-list))
      (when (and (buffer-file-name buffer)
                 (member (expand-file-name (buffer-file-name buffer)) files))
        (with-current-buffer buffer
          ;; Only confirm if the buffer is modified; otherwise silently revert
          (if (buffer-modified-p)
              (revert-buffer t)
            (revert-buffer t t)))))))

(defun darcs-changes (repo-dir &optional number-of-changes)
  "Show all the changes in the entire REPO-DIR.
Shows the last NUMBER-OF-CHANGES changes if specified; otherwise
shows a complete list."
  (interactive (list (or (buffer-file-name) default-directory)
                     current-prefix-arg))
  (let ((inhibit-read-only t)
        (root-dir (darcs-root-directory repo-dir)))
    (darcs-set-buffer 'changes root-dir)
    (erase-buffer)
    (if number-of-changes
        (darcs-do-command root-dir "changes" "--verbose"
                          (format "--last=%d"
                                  (prefix-numeric-value number-of-changes)))
      (darcs-do-command root-dir "changes" "--verbose"))
    (goto-char (point-min))
    (setq darcs-exclude-enabled-function (lambda (ov) nil))
    (save-excursion
      (darcs-markup-patch-descriptions 4)
      (darcs-collapse-all-atomic-patches))))

(defun darcs-filelog (file &optional number-of-changes)
  "Show all the changes that apply to FILE.
Shows the last NUMBER-OF-CHANGES changes if specified; otherwise
shows a complete list."
  (interactive (list (or (buffer-file-name)
                         (error "Current buffer is not associated with a file"))
                     (if (listp current-prefix-arg)
                         (car current-prefix-arg)
                       current-prefix-arg)))
  (darcs-set-buffer 'filelog file)
  (let ((inhibit-read-only t)
        (canon-file (darcs-canonical-name file)))
    (erase-buffer)
    (if number-of-changes
        (darcs-do-command (darcs-root-directory file)
                          "changes" canon-file "--verbose"
                          (format "--last=%d" number-of-changes))
      (darcs-do-command (darcs-root-directory file)
                        "changes" canon-file "--verbose"))
    (goto-char (point-min))
    (setq darcs-exclude-enabled-function (lambda (ov) nil))
    (save-excursion
      (darcs-markup-patch-descriptions 4)
      (darcs-collapse-all-atomic-patches))))

(defun darcs-pull (repo-dir &optional no-prompt)
  "Interface to the darcs pull command.
A list of possible patches will be displayed for
inclusion/exclusion. With a prefix argument, all patches will be
pulled without prompting."
  (interactive (list (or (buffer-file-name) default-directory)
                     current-prefix-arg))
  (let ((root-dir (or (darcs-root-directory repo-dir)
                      (error (format "No darcs repo at or around %s" (file-name-directory repo-dir)))))
        (inhibit-read-only t))
    (save-some-buffers)
    (darcs-set-buffer 'pull root-dir)
    (erase-buffer)
    (setq darcs-exclude-enabled-function 'darcs-named-patch-p)
    (darcs-do-command-async (root-dir "pull" "--verbose" "--dry-run")
                            (let ((inhibit-read-only t)
                                  (atomic-indentation 4)
                                  (forced-indentation nil))
                              (goto-char (point-min))
                              (cond
                               ((re-search-forward "No remote changes to pull in!" nil t)
                                (goto-char (match-beginning 0))
                                (delete-region (point-min) (point))
                                (save-excursion
                                  (darcs-markup-patch-descriptions)))
                               ((re-search-forward "Would pull the following changes:" nil t)
                                (delete-region (point-min) (point))
                                ;; Detect pre-2.0 darcs format, where atomic patches start from column 1
                                (save-excursion
                                  (goto-char (point-min))
                                  (when (or (re-search-forward "^\\+" nil t) (re-search-forward "^-" nil t))
                                    (setq atomic-indentation nil)
                                    (setq forced-indentation "    ")))
                                (insert "Select which patches to pull.\nType C-c C-c to pull the included patches.\nType C-x k to abandon pull.\n")
                                (save-excursion
                                  (darcs-markup-patch-descriptions atomic-indentation forced-indentation)
                                  (darcs-collapse-all-atomic-patches))
                                (darcs-move-to-patch 1)))))))

(defun darcs-commit-pull ()
  "Pull the patches included in the current buffer."
  (interactive)
  (let ((root-dir default-directory)
        (patch-responses (darcs-collect-patch-responses)))
    (darcs-do-interactive-command root-dir patch-responses "pull")))

(defun darcs-push (repo-dir &optional no-prompt)
  "Interface to the darcs push command.
A list of possible patches will be displayed for
inclusion/exclusion. With a prefix argument, all patches will be
pushed without prompting."
  (interactive (list (or (buffer-file-name) default-directory)
                     current-prefix-arg))
  (let ((root-dir (or (darcs-root-directory repo-dir)
                      (error (format "No darcs repo at or around %s" (file-name-directory repo-dir)))))
        (inhibit-read-only t))
    (save-some-buffers)
    (darcs-set-buffer 'push root-dir)
    (erase-buffer)
    (setq darcs-exclude-enabled-function 'darcs-named-patch-p)
    (darcs-do-command-async (root-dir "push" "--verbose" "--dry-run")
                            (let ((inhibit-read-only t)
                                  (atomic-indentation 4)
                                  (forced-indentation nil))
                              (goto-char (point-min))
                              (cond
                               ((re-search-forward "\nNo recorded local changes to push!" nil t)
                                (goto-char (match-beginning 0))
                                (delete-region (point-min) (point))
                                (save-excursion
                                  (darcs-markup-patch-descriptions)))
                               ((re-search-forward "\nWould push the following changes:" nil t)
                                (delete-region (point-min) (point))
                                ;; Detect pre-2.0 darcs format, where atomic patches start from column 1
                                (save-excursion
                                  (goto-char (point-min))
                                  (when (or (re-search-forward "^\\+" nil t) (re-search-forward "^-" nil t))
                                    (setq atomic-indentation nil)
                                    (setq forced-indentation "    ")))
                                (insert "Select which patches to push.\nType C-c C-c to push the included patches.\nType C-x k to abandon push.\n")
                                (save-excursion
                                  (darcs-markup-patch-descriptions atomic-indentation forced-indentation)
                                  (darcs-collapse-all-atomic-patches))
                                (darcs-move-to-patch 1)))))))

(defun darcs-commit-push ()
  "Push the patches included in the current buffer."
  (interactive)
  (let ((root-dir default-directory)
        (patch-responses (darcs-collect-patch-responses)))
    (darcs-do-interactive-command root-dir patch-responses "push")))

(defun darcs-init (root-dir)
  "Initialize a repository at ROOT-DIR."
  (interactive (list (read-directory-name "Repository directory: " default-directory)))
  (let ((default-directory root-dir))
    (with-temp-buffer
      (unless (zerop (darcs-do-command default-directory "init"))
        (error (xdarcs-buffer-string-single-line)))
      (message "Created darcs repo %s" root-dir))))

(defun darcs-do-command (root-dir &rest options)
  "Run darcs in ROOT-DIR, passing it OPTIONS."
  (let ((default-directory root-dir)
        (cmd-line "darcs"))
    (setq options (remove nil options))
    (dolist (opt options)
      (setq cmd-line (concat cmd-line " " opt)))
    (message cmd-line)
    (prog1
        (apply 'call-process "darcs" nil (current-buffer) t options)
      (message ""))))

(defun darcs--kill-current-buffer-process ()
  "Kill the process associated with the current buffer.
This is intended to be added to `kill-buffer-hook'"
  (let ((proc (get-buffer-process (current-buffer))))
    (when proc
      (kill-process proc))))

(defun darcs-delete-comment-filename ()
  "Delete the comment filename tempfile.  This is intended to be added to `kill-buffer-hook'."
  (when darcs-comment-filename
    (delete-file darcs-comment-filename)
    (setq darcs-comment-filename nil)))

;; Really we need a general mechanism for setting process-buffer-local variables, as in darcsum
(defvar darcs-process-scan-pos (point-min)
  "The point that `darcs-process-filter' should start scanning from.")
(make-variable-buffer-local 'darcs-process-scan-pos)

(defun darcs-do-interactive-command (root-dir patch-responses &rest options)
  (let ((default-directory root-dir)
        (cmd-line "darcs")
        (process nil)
        (process-environment (append '("DARCS_DONT_ESCAPE_TRAILING_SPACES=1"
                                       "DARCS_DONT_COLOR=1")
                                     process-environment)))
    (setq options (remove nil options))
    (dolist (opt options)
      (setq cmd-line (concat cmd-line " " opt)))
    (message "%s" cmd-line)

    (when (and (get-buffer "*darcs output*")
               (get-buffer-process "*darcs output*")
               (eq 'run (process-status (get-buffer-process "*darcs output*")))
               (yes-or-no-p "A darcs process is already running; kill it?"))
      (kill-process (get-buffer-process "*darcs output*"))
      (kill-buffer "*darcs output*"))
    (setq process (apply 'start-process cmd-line "*darcs output*" "darcs" options))
    (with-current-buffer (process-buffer process)
      (erase-buffer)
      (setq darcs-patch-responses patch-responses)
      (setq darcs-process-scan-pos (point-min))
      (setq default-directory root-dir)
      (add-hook 'kill-buffer-hook 'darcs--kill-current-buffer-process nil t))
    (set-process-sentinel process 'darcs-process-sentinel)
    (set-process-filter process 'darcs-process-filter)))

(defun darcs-process-sentinel (proc string)
  (when (and (string-match "^exited abnormally" string)
             (process-buffer proc))
    (message "%s: %s" (process-name proc) string))
  (when (and (not (eq 'run (process-status proc)))
             (buffer-live-p (process-buffer proc))
             (not darcs-debug))
    (kill-buffer (process-buffer proc))))

(defvar darcs-process-filter-mark-overlay nil
  "An overlay that highlights the currently unconsumed output in the darcs output buffer.")

;; The starting point for this function was `darcsum-process-filter' in darcsum.el.
(defun darcs-process-filter (proc string)
  (when (buffer-live-p (process-buffer proc))
    (with-current-buffer (process-buffer proc)
      (unless darcs-process-filter-mark-overlay
        (setq darcs-process-filter-mark-overlay (make-overlay (process-mark proc) (point-max)))
        (xdarcs-set-overlay-face darcs-process-filter-mark-overlay 'highlight))
      (goto-char (process-mark proc))
      (insert string)
      (set-marker (process-mark proc) (point))
      (let ((prev-scan-pos nil))
        (flet ((send-input (input &optional insert-pos)
                           "Send input to the process and also insert that input into the buffer"
                           (when insert-pos (goto-char insert-pos))
                           (insert input)
                           (insert "\n")
                           (when (< (process-mark proc) (point))
                             (set-marker (process-mark proc) (point)))
                           (process-send-string proc input)))
          (while (and (buffer-live-p (process-buffer proc))
                      (< darcs-process-scan-pos (point-max))
                      (not (eql prev-scan-pos darcs-process-scan-pos)))
            (setq prev-scan-pos darcs-process-scan-pos)
            (goto-char darcs-process-scan-pos)
            (move-overlay darcs-process-filter-mark-overlay
                          (point) (point-max))
            (cond
             ((looking-at "[\r\n ]*Finished recording patch")
              (darcs-kill-if-exists 'record default-directory)
              (darcs-refresh-whatsnew)
              (message "Changes recorded."))
             ((looking-at "[\r\n ]*Finished applying...")
              (darcs-kill-if-exists 'push default-directory)
              (message "Finished pushing and applying."))
             ((looking-at "[\r\n ]*Ok, if you don't want to record anything")
              (message "No changes recorded."))
             ((looking-at "[\r\n ]*[wW]arning:[^\n]*")
              (let ((s (match-beginning 0))
                    (e (match-end 0)))
                (message "%s" (buffer-substring s e))
                (setq darcs-process-scan-pos e)))
             ;;TODO - support for automated patch-sending
                                        ;              ((looking-at "[\r\n ]*What is the target email address")
                                        ;               (send-input darcsum-process-arg (point-max))
                                        ;               (setq darcs-process-scan-pos (point-max)))
                                        ;              ((looking-at "[\r\n ]*Successfully sent patch bundle")
                                        ;               (message "Changes sent to `%s'." darcsum-process-arg))
             ((looking-at "[\r\n ]*You don't want to send any patches")
              (message "No changes sent."))
             ((looking-at ".*\nHit return to move on...")
              (send-input "\r\n"))
             ((looking-at "[\r\n ]*Do you really want to .+\\? ")
              (send-input "y\n" (point-max))
              (setq darcs-process-scan-pos (point-max)))
             ((looking-at "[\r\n ]*\\([^\n]+\\)'s password:")
              (process-send-string proc (read-passwd (format "Password for %s: " (match-string 1))))
              (send-input "\n" (point-max))
              (setq darcs-process-scan-pos (point-max)))
             ((looking-at "[\r\n ]*Finished reverting.")
              (darcs-refresh-whatsnew)
              (darcs-refresh-query-manifest)
              (darcs-refresh-responded)
              (darcs-kill-if-exists 'revert default-directory)
              (message "Changes reverted."))
             ((looking-at "[\r\n ]*If you don't want to revert")
              (message "No changes reverted."))
             ((looking-at "[\r\n ]*Finished pulling and applying.")
              (darcs-refresh-whatsnew)
              (darcs-refresh-query-manifest)
              (darcs-refresh-responded)
              (darcs-kill-if-exists 'pull default-directory)
              (message "Finished pulling and applying"))
             ((looking-at "[\r\n ]*You don't want to pull any patches, and that's fine with me!")
              (message "No patches pulled"))
             ((looking-at "[\r\n ]*\\(Waiting for lock.*\\)\n+")
              (setq darcs-process-scan-pos (point-max))
              (message (match-string 1)))
             ((looking-at "[\r\n ]*\\(Couldn't get lock.*\\)[\r\n ]*")
              (message (match-string 1)))
             ((looking-at "[\r\n ]*\\(Pulling from\\|Pushing to\\) \"?\\([^\n\"]+\\)\"?\\.\\.\\.\n")
              (let ((verb (match-string 1))
                    (repo (match-string 2))
                    (end (match-end 0)))
                (message "%s %s..." verb repo)
                (setq darcs-process-scan-pos end)))
             ((looking-at "[\r\n ]*\\(No \\(remote\\|recorded local\\) changes to \\(pull in\\|push\\)!\\)")
              (message (match-string 1)))
             ((looking-at "[\r\n ]*We have conflicts in the following files:\n")
              (let ((s (match-beginning 0))
                    (e (match-end 0))
                    (conflict-text))
                (goto-char e)
                (while (looking-at "\\./\\([^\n]+\\)\n")
                  (forward-line))
                (setq conflict-text (buffer-substring s (point)))
                (save-selected-window
                  (switch-to-buffer-other-window (darcs-format-buffername 'conflicts default-directory))
                  (goto-char (point-max))
                  (insert conflict-text))
                (setq darcs-process-scan-pos (point))))
             ((looking-at "[\r\n ]*Darcs needs to know what name")
              (let* ((default-mail (concat user-full-name
                                           " <" user-mail-address ">"))
                     (enable-recursive-minibuffers t)
                     (mail-address (read-string
                                    (format
                                     "What is your email address? (default %s) "
                                     default-mail)
                                    nil nil default-mail)))
                (send-input mail-address)
                (send-input "\n"))
              (re-search-forward "What is your email address\\?.*")
              (setq darcs-process-scan-pos (point)))

             ((looking-at (format "[\r\n ]*\\(\\(%s\\)[^\n]+\\)\n" darcs-patch-headers-re))
              (let ((change-desc (match-string 1)))
                (save-excursion
                  (goto-char (match-end 0))
                  (while (looking-at "^[+-].*")
                    (forward-line))
                  (when (looking-at
                         "^Shall I \\(record\\|send\\|revert\\) this \\(patch\\|change\\)\\?\\( ([0-9]+/[0-9]+)\\)?.+[]:] ")
                    (let ((response-cell (assoc change-desc darcs-patch-responses)))
                      (if response-cell
                          (let ((end (match-end 0))
                                (response (if (xdarcs-plist-get-with-default (cdr response-cell) :included t)
                                              "y" "n")))
                            (send-input response end)
                            (setq darcs-process-scan-pos (point))
                            (when (match-string 3)
                              (message (format "%s%s %s%s"
                                               (if (string= response "y") (match-string 1) " skipp")
                                               "ing" (match-string 2) (match-string 3)))))
                        (send-input "q")
                        (message (format "Unrecognized change description '%s'" change-desc))))))))
             ((looking-at "[\r\n ]*\\(Sun\\|Mon\\|Tue\\|Wed\\|Thu\\|Fri\\|Sat\\)[^\n]+\n[ \t]+\\(?:\\*\\|tag\\(?:ged\\)?\\) \\([^\r\n]+\\)[\r\n]")
              (let ((change-desc (match-string 2))
                    (end (match-end 0)))
                (save-excursion
                  (goto-char end)
                  (while (looking-at "  ")
                    (forward-line))    ; Skip over the long description
                  (when (looking-at "Shall I \\(push\\|pull\\) this patch\\?.*[]:] ")
                    (let ((response-cell (assoc change-desc darcs-patch-responses)))
                      (if response-cell
                          (let ((end (match-end 0))
                                (response (if (xdarcs-plist-get-with-default (cdr response-cell) :included t)
                                              "y" "n")))
                            (send-input response end)
                            (setq darcs-process-scan-pos (point))
                            (message (format "%s%s patch: %s"
                                             (if (string= response "y") (concat "  " (match-string 1)) " skipp")
                                             "ing" change-desc)))
                        (send-input "q" (point-max))
                        (message (format "Unrecognized change description '%s'" change-desc))))))))
             ;; Darcs 2 support
             ((looking-at "[\r\n ]*\\(Identifying repository .*\\)\n")
              (message (match-string 1))
              (forward-line)
              (setq darcs-process-scan-pos (point)))
             ((looking-at "[\r\n ]*\\(Reading .* of repository .*\\)\n")
              (message (match-string 1))
              (forward-line)
              (setq darcs-process-scan-pos (point)))
             ((looking-at "[\r\n ]*\\(\\(Reading\\|Writing\\|Synchronizing\\|Cleaning\\|Optimizing\\) .* [0-9]+/[0-9]+.*\\)\n")
              (message (match-string 1))
              (forward-line)
              (setq darcs-process-scan-pos (point)))
             ((looking-at "[\r\n ]*\\(Skipped \\(push\\|pull\\|record\\|revert\\) of [0-9]+ patch\\(es\\)?.\\)\n")
              (message (match-string 1))
              (forward-line)
              (setq darcs-process-scan-pos (point)))
             ((looking-at "[\r\n ]*\\(Recording\\|Reverting\\) changes in \".*\"\\(\\.\\.\\|:\\)[\r\n ]*")
              (forward-line)
              (setq darcs-process-scan-pos (point))))))))))

(defvar darcs-async-continuation nil
  "Code to execute once the command started by `darcs-do-command-async' has terminated.")

(defun darcs-do-command-async-fn (root-dir continuation &rest options)
  "Run darcs asynchronously in ROOT-DIR, passing it OPTIONS.
When the process terminates, call CONTINUATION.

  It's generally much more convenient to use the
  `darcs-do-command-async' macro than to use this function
  directly."
  (let ((default-directory root-dir)
        (cmd-line "darcs")
        (process nil)
        (process-environment (append '("DARCS_DONT_ESCAPE_TRAILING_SPACES=1"
                                       "DARCS_DONT_COLOR=1")
                                     process-environment)))
    (dolist (opt options)
      (setq cmd-line (concat cmd-line " " opt)))
    (message "%s" cmd-line)

    (setq process (apply 'start-process cmd-line (current-buffer) "darcs" options))
    (add-hook 'kill-buffer-hook 'darcs--kill-current-buffer-process nil t)
    (set (make-local-variable 'darcs-async-continuation) continuation)
    (set-process-sentinel process 'darcs-async-sentinel)
    (set-process-filter process 'darcs-async-filter)))

(defun darcs-async-sentinel (proc string)
  "Resume `darcs-async-continuation' when PROC exits."
  (when (process-buffer proc)
    (with-current-buffer (process-buffer proc)
      (when darcs-async-continuation
        (funcall darcs-async-continuation)
        (setq darcs-async-continuation nil)))))

(defun darcs-async-filter (proc string)
  "Provide status updates as the asynchronous update proceeds."
  (with-current-buffer (process-buffer proc)
    (insert string)))

(defun darcs-markup-patch-descriptions (&optional allow-leading-whitespace force-leading-whitespace)
  "Starting from point and moving down the rest of the buffer, apply formatting to each patch description.
If ALLOW-LEADING-WHITESPACE is a number, leading whitespace
precisely of the specified length will be permitted. (This makes
it possible to mark up patch descriptions that have been
indented, as in the `darcs-changes' list). Contrariwise, if
FORCE-LEADING-WHITESPACE is specified, it will be inserted in
front of (TODO in place of?) the leading whitespace. This allows
us to make the display consistent even for commands where darcs
itself does not display consistently."
  (setq allow-leading-whitespace (or allow-leading-whitespace 0)) ; defaults to 0
  (let ((lines-left 0)
        (prev-header-ov nil)
        (prev-patch-name-ov nil)
        (orig-pos (point))
        (display-ov nil)
        (header-re-w/whitespace (format "\\([ \t]*\\)\\(%s\\)" darcs-patch-headers-re)))
    (while (zerop lines-left)
      (cond
       ((looking-at "\\([MTWFS].*[0-9][0-9][0-9][0-9]\\)[ \t]+\\([^\r\n]*\\)[\r\n]")
        (let* ((local-date (buffer-substring (match-beginning 1) (match-end 1)))
               (author (buffer-substring (match-beginning 2) (match-end 2)))
               (arpa-date (darcs-cook-date local-date))
               (pre-author-point nil)
               (patch-name (save-excursion
                             (forward-line 1)
                             (and (looking-at "[ \t]+\\(\\*\\|tag\\(?:ged\\)?\\) \\([^\r\n]+\\)[\r\n]")
                                  (buffer-substring (match-beginning 2) (match-end 2)))))
               (patch-type (and patch-name (buffer-substring (match-beginning 1) (match-end 1)))))
          (when patch-name
            (delete-region (point-at-bol) (point-at-eol))
            (insert patch-name)
            (let ((patch-name-ov (darcs-make-link-overlay (point-at-bol) (point-at-eol)
                                                          '(darcs-toggle-patch-expanded))))
              (xdarcs-set-overlay-face patch-name-ov (if (string= patch-type "*")
                                                         'darcs-patch-name-face
                                                       'darcs-tag-name-face))
              (xdarcs-set-overlay-keymap patch-name-ov darcs-patch-display-map)
              (when prev-header-ov
                (overlay-put prev-header-ov 'darcs-patch-ov
                             (make-overlay (overlay-start prev-header-ov)
                                           (save-excursion
                                             (goto-char (overlay-start patch-name-ov))
                                             (goto-char (point-at-bol))
                                             (forward-char -2)
                                             (point)))))
              (when prev-patch-name-ov
                (overlay-put prev-patch-name-ov 'darcs-patch-ov
                             (make-overlay (overlay-start prev-patch-name-ov)
                                           (save-excursion
                                             (goto-char (overlay-start patch-name-ov))
                                             (goto-char (point-at-bol))
                                             (forward-char -2)
                                             (point)))))
              (setq prev-patch-name-ov patch-name-ov)
              (setq prev-header-ov patch-name-ov)

              (goto-char (point-at-bol))
              (forward-line 1)
              (delete-region (point-at-bol) (point-at-eol))
              (insert arpa-date)
              (xdarcs-set-overlay-face (make-overlay (point-at-bol) (point)) 'darcs-blame-date-face)
              (overlay-put patch-name-ov 'darcs-collapse-point (point))
              (insert "  ")
              (setq pre-author-point (point))
              (insert author)
              (xdarcs-set-overlay-face (make-overlay pre-author-point (point)) 'darcs-blame-author-face)))))
       ((and (looking-at "\\([ \t]*\\)\\[")
             (or (null prev-header-ov) (not (eq prev-header-ov prev-patch-name-ov)))
             (eql allow-leading-whitespace (length (match-string 1))))
        ;; get rid of leading '[' in patch name
        (delete-char 1)
        ;; Insert forced indentation if requested
        (when force-leading-whitespace (insert force-leading-whitespace))
        ;; highlight patch name..
        (xdarcs-set-overlay-face (make-overlay (point-at-bol) (point-at-eol))
                                 'darcs-patch-name-face)
        ;; ...and also the author line that we know will follow
        (forward-line 1)
        (xdarcs-set-overlay-face (make-overlay (point-at-bol) (point-at-eol))
                                 'darcs-blame-author-face)
        ;; trim the author line to get rid of the date (and possibly description-ending delimiters)
        ;; Re-insert the date at the beginning in a nicer format
        (re-search-forward "\\*\\*\\([0-9]+\\)[^*\r\n]+[\r\n$]")
        (when (match-beginning 0)
          (let* ((date-str (buffer-substring (match-beginning 1) (match-end 1)))
                 (year (car (read-from-string date-str 0 4)))
                 (month (car (read-from-string date-str 4 6)))
                 (day (car (read-from-string date-str 6 8)))
                 (hour (car (read-from-string date-str 8 10)))
                 (minute (car (read-from-string date-str 10 12)))
                 (second (car (read-from-string date-str 12))))
            (goto-char (match-beginning 0))
            (delete-region (point) (point-at-eol))
            (goto-char (point-at-bol))
            (save-excursion
              (insert (timezone-make-date-arpa-standard
                       (timezone-make-arpa-date year month day
                                                (timezone-make-time-string hour minute second))))
              (xdarcs-set-overlay-face (make-overlay (point-at-bol) (point))
                                       'darcs-blame-date-face)
              (insert "  ")))))
       ((and (looking-at "\\([ \t]*\\)[]>] {")
             (or (null prev-header-ov) (not (eq prev-header-ov prev-patch-name-ov)))
             (eql allow-leading-whitespace (length (match-string 1))))
        (delete-region (point-at-bol) (point-at-eol)))
       ((looking-at "Making no changes:  this is a dry run.")
        (delete-region (point-at-bol) (point-at-eol)))
       ((and (looking-at "\\([ \t]*\\)[]{}<>] *[\r\n]")
             (eql allow-leading-whitespace (length (match-string 1))))
        (delete-region (point-at-bol) (point-at-eol)))
       ((and (looking-at "\\([ \t]*\\)\\+")
             (or (null prev-header-ov) (not (eq prev-header-ov prev-patch-name-ov)))
             (eql allow-leading-whitespace (length (match-string 1))))
        (when force-leading-whitespace (insert force-leading-whitespace))
        (xdarcs-set-overlay-face (make-overlay (point-at-bol) (point-at-eol))
                                 'darcs-line-added-face))
       ((and (looking-at "\\([ \t]*\\)\\-")
             (or (null prev-header-ov) (not (eq prev-header-ov prev-patch-name-ov)))
             (eql allow-leading-whitespace (length (match-string 1))))
        (when force-leading-whitespace (insert force-leading-whitespace))
        (xdarcs-set-overlay-face (make-overlay (point-at-bol) (point-at-eol))
                                 'darcs-line-removed-face))
       ((and (looking-at header-re-w/whitespace)
             (eql allow-leading-whitespace (length (match-string 1))))
        (when force-leading-whitespace (insert force-leading-whitespace))
        (let* ((bot (save-excursion (beginning-of-line-text) (point)))
               (ov (darcs-make-link-overlay bot (point-at-eol)
                                            '(darcs-toggle-patch-expanded))))
          (when prev-header-ov
            (overlay-put prev-header-ov 'darcs-patch-ov
                         (make-overlay (overlay-start prev-header-ov)
                                       (save-excursion
                                         (goto-char (overlay-start ov))
                                         (goto-char (point-at-bol))
                                         (forward-char -1)
                                         (point)))))
          (xdarcs-set-overlay-face ov 'darcs-header-line-face)
          (xdarcs-set-overlay-keymap ov darcs-patch-display-map)
          (setq prev-header-ov ov))))
      (setq lines-left (forward-line 1)))
    (when prev-header-ov
      (overlay-put prev-header-ov 'darcs-patch-ov
                   (make-overlay (overlay-start prev-header-ov)
                                 (point-max))))
    (when prev-patch-name-ov
      (overlay-put prev-patch-name-ov 'darcs-patch-ov
                   (make-overlay (overlay-start prev-patch-name-ov)
                                 (point-max))))
    (setq display-ov (make-overlay orig-pos (point-max)))
    (xdarcs-set-overlay-keymap display-ov darcs-patch-display-map)
    (overlay-put display-ov 'read-only t)
    (overlay-put display-ov 'start-open t)
    (overlay-put display-ov 'end-open nil)
    (setq selective-display t)))

(defconst xdarcs-time-zone-translations '(("Pacific Standard Time" . "PST")
                                          ("Pacific Daylight Time" . "PDT")
                                          ("Eastern Daylight Time" . "EDT")
                                          ("Eastern Standard Time" . "EST")
                                          ("Atlantic Daylight Time" . "ADT")
                                          ("Atlantic Standard Time" . "AST")))
(defun darcs-cook-date (local-date)
  (let ((cells xdarcs-time-zone-translations)
        (arpa-date nil))
    ;; Try to avoid confusing `timezone-make-date-arpa-standard' by translating long-form timezone
    ;; names into their short forms before calling it.
    (while cells
      (setq local-date (replace-regexp-in-string (caar cells) (cdar cells) local-date))
      (setq cells (cdr cells)))

    ;; After we do the translation, just return the local time if we get garbage results; otherwise,
    ;; return the much-shorter ARPA date.
    (setq arpa-date (timezone-make-date-arpa-standard local-date))
    (if (string= arpa-date "31 Dec 1999 16:00:00 -0800")
        local-date
      arpa-date)))

(defun darcs-canonical-name (file)
  "Return a relative path for FILE from its repository root directory, starting from '.'."
  (let ((root (darcs-root-directory file))
        (abs-path (expand-file-name file)))
    (when (string= (substring abs-path 0 (length root))
                   root)
      (concat "./" (substring abs-path (length root))))))

(defun darcs-root-directory (file)
  "Return the nearest repo root directory for FILE.
This code is modified from Jorgen Schaefer's `vc-darcs.el'"
  (let ((dir (file-name-directory (expand-file-name file)))
        (olddir "/"))
    (while (and (not (equal dir olddir))
                (not (file-directory-p (concat dir "/_darcs"))))
      (setq olddir dir
            dir (file-name-directory (directory-file-name dir))))
    (unless (equal dir olddir) dir)))

(defun xdarcs-xml-get-children* (node child-name)
  "A version of `xml-get-children' that actually works in the presence of text children."
  (let ((result nil))
    (dolist (child (xml-node-children node))
      (when child
        (if (and (listp child)
                 (equal (xml-node-name child) child-name))
            (push child result))))
    (nreverse result)))

(defun darcs-trim-newlines (string)
  "Return STRING with leading and trailing newlines removed."
  (replace-regexp-in-string "\\`\n+\\|\n+\\'" "" string))

(defun darcs-xml-node-text (node)
  "Return the untagged text children of NODE."
  (mapconcat (lambda (child) (when (stringp child) child))
             (xml-node-children node)
             ""))

(defun darcs-set-mode-for-file (filename)
  "Set mode of the current buffer as appropriate for FILENAME."
  (let ((alist auto-mode-alist))
    (while alist
      (let ((cell (car alist)))
        (when (string-match (car cell) filename)
          (if (listp (cdr cell))
              (funcall (car (cdr cell)))
            (funcall (cdr cell)))
          (setq alist nil))
        (setq alist (cdr alist))))))

(defun darcs-manifest (file-or-dir)
  "Return a list of all canonical files that are managed by the repo at or around FILE-OR-DIR."
  (let ((repo (darcs-root-directory file-or-dir)) (lines-left 0) output)
    (unless repo
      (error (format "No darcs repo at or around %s" file-or-dir) ))
    (with-temp-buffer
      (darcs-do-command repo "query" "manifest")
      (goto-char (point-min))
      (while (and (= 0 lines-left)
                  (/= (point-at-bol) (point-at-eol)))
        (push (buffer-substring (point-at-bol) (point-at-eol)) output)
        (setq lines-left (forward-line 1)))
      (nreverse output))))

(defun darcs-file-registered-p (filename)
  "Return non-nil if FILENAME is in a darcs repo."
  (and (darcs-root-directory filename)
       (member (darcs-canonical-name filename) (darcs-manifest filename))))

(defun darcs-associated-file (repo-dir header-string)
  "Return the file that the change described by HEADER-STRING affects."
  (cond
   ((string-match "hunk \\(.*\\) [0-9]" header-string)
    (expand-file-name (concat (darcs-root-directory repo-dir)
                              (match-string 1 header-string))))
   ((string-match "\\(addfile\\|adddir\\|binary\\|rmfile\\) \\(.*\\)" header-string)
    (expand-file-name (concat (darcs-root-directory repo-dir)
                              (match-string 2 header-string))))
   ((string-match darcs-patch-headers-re header-string)
    ;; Otherwise, if we recognize the header but don't currently support treating it as having an
    ;; associated file, just return nil.
    ;; TODO (we should probably recognize more types than we currently do)
    ;; The expected but currently-unhandled types are:
    ;; ("replace" "move" "changepref" "merger" "regrem" "conflict" "tcilfnoc")
    nil)
   (t (error (format "Unparseable header %S" header-string)))))

(defun darcs-associated-line (repo-dir header-string)
  "Return the line associated with the change described by HEADER-STRING.
Returns nil for non-line-specific changes (ie. anything other
than a hunk)."
  (when (string-match "hunk .* \\([0-9]+\\)" header-string)
    (car (read-from-string (match-string 1 header-string)))))

(defun darcs-format-buffername (mode-sym target)
  "Create a standard buffer name.
TARGET is usually a file name, repo directory, or patch name. It
is can also be a file name for a type that expects a repo
directory; the directory will be determined from the file name.
MODE-SYM specifies the type of buffer to create, and should be
one of the following: 'blame 'changes 'conflicts 'describe
'filelog 'pull 'push 'query-manifest 'record 'revert 'whatsnew"
  (cond
   ((member mode-sym '(blame describe filelog))
    (format "*darcs %s: %s*" (symbol-name mode-sym) target))
   ((member mode-sym '(changes conflicts pull push query-manifest record revert whatsnew))
    (format "*darcs %s: (%s)*" (symbol-name mode-sym) (darcs-root-directory target)))
   (t
    (error (format "Unrecognized MODE-SYM %S" mode-sym)))))

(defun darcs-set-buffer (mode-sym target &optional recursive-p)
  "Set up a buffer for the operation specified by MODE-SYM and TARGET.
If RECURSIVE-P is T, the buffer will be set in the current window;
If it is NIL, the buffer will be opened using `switch-to-buffer-other-window';
If it is any other value, the buffer will be opened using `switch-to-buffer'."
  (let* ((new-mode-sym (intern (format "darcs-%s-mode" mode-sym)))
         (new-revert-function
          (let ((refresh-candidate (intern (format "darcs-refresh-%s" mode-sym)))
                (redo-candidate (intern (format "darcs-%s" mode-sym))))
            (if (fboundp refresh-candidate) refresh-candidate redo-candidate))))
    ;; switch to the buffer
    (unless darcs--orig-wincfg
      (setq darcs--orig-wincfg (current-window-configuration)))
    (cond
     ((eq recursive-p t)
      (set-buffer (get-buffer-create (darcs-format-buffername mode-sym target))))
     (recursive-p
      (switch-to-buffer (darcs-format-buffername mode-sym target)))
     (t
      (switch-to-buffer-other-window (darcs-format-buffername mode-sym target))))
    ;; setup mode
    (funcall new-mode-sym)
    (cond ((file-directory-p target)
           (setq default-directory target))
          ((file-exists-p target)
           (setq default-directory (file-name-directory target))))
    (set (make-local-variable 'revert-buffer-function)
         (lambda (ignore-auto noconfirm) (funcall new-revert-function)))))

(defun darcs--define-mode (mode-sym)
  (let ((mode-sym (intern (format "darcs-%s-mode" mode-sym)))
        (mode-name (format "Darcs %s mode" mode-sym)))
    (eval `(define-derived-mode ,mode-sym special-mode ,mode-name))))

(mapc 'darcs--define-mode '(changes describe pull push query-manifest record revert whats-new))

(defun darcs-kill-if-exists (mode-sym target)
  "If the special buffer specified by MODE-SYM and TARGET exists, kill it."
  (let ((name (darcs-format-buffername mode-sym target)))
    (when (get-buffer name) (kill-buffer name))))

(defun xdarcs-plist-get-with-default (plist prop default)
  (or (plist-get plist prop) default))

(defun xdarcs-set-overlay-keymap (overlay keymap)
  (overlay-put overlay 'keymap keymap))

(defun xdarcs-set-overlay-face (overlay face)
  (overlay-put overlay 'face face))

(defun xdarcs-set-overlay-priority (overlay priority)
  (overlay-put overlay 'priority priority))

(defun xdarcs-overlay-at (p &optional property)
  "Return the smallest overlay that contains position P.
This will be the overlay whose start position is closest to P. If
PROPERTY is non-nil, then only overlays with PROPERTY set will be
considered."
  (let (nearest (overlays (overlays-at p)))
    (while overlays
      (when (and (or (null property)
                     (overlay-get (car overlays) property))
                 (or (null nearest)
                     (> (overlay-start (car overlays))
                        (overlay-start nearest))))
        (setq nearest (car overlays)))
      (setq overlays (cdr overlays)))
    nearest))

(defun xdarcs-overlay-string (ov)
  (buffer-substring (overlay-start ov) (overlay-end ov)))

(eval-and-compile
  (unless (fboundp 'event-window)
    (defun event-window (evt)
      (posn-window (event-start evt)))))

(eval-and-compile
  (unless (fboundp 'event-point)
    (defun event-point (evt)
      (posn-point (event-start evt)))))

(defun xdarcs-buffer-string-single-line ()
  (replace-regexp-in-string "\n" " " (buffer-substring (point-min) (point-max))))

(provide 'xdarcs)
;;; xdarcs.el ends here
