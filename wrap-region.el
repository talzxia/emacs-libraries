;;; wrap-region.el --- wrap text with a pair of delimiters or a tag

;; Copyright (C) 2008 Johan Andersson

;; Author: Johan Andersson <johan.rejeep@gmail.com>
;; Maintainer: Johan Andersson <johan.rejeep@gmail.com>
;; Version: 0.0.1
;; Modified-by: Štěpán Němec <stepnem@gmail.com>
;; Last-modified: 2010-03-03 18:39 CET
;; Keywords: speed, convenience
;; URL: http://github.com/stepnem/emacs-libraries/blob/priv/wrap-region.el
;; Original-URL: http://github.com/rejeep/wrap-region

;; This file is NOT part of GNU Emacs.

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; wrap-region is a minor mode that wraps text inside a pair of
;; delimiters -- insertion pair. For some tagged markup modes, such as
;; HTML and XML, it wraps the region with a tag instead.

;; To use wrap-region, make sure that this file is in Emacs' load-path:
;;
;; (add-to-list 'load-path "/directory/containing/this/file")
;;
;;
;; Then require wrap-region:
;;
;; (require 'wrap-region)
;;
;;
;; To enable the minor mode:
;;
;; (wrap-region-mode t) or M-x wrap-region-mode
;;
;;
;; You can enable it automatically for specific modes, e.g.:
;;
;; (add-hook 'ruby-mode-hook 'wrap-region-mode)
;;
;; This will enable all of wrap-region's default insertion pairs in
;; ruby-mode. To enable only some of the pairs defined in
;; `wrap-region-pairs', use `wrap-region-restrict-mode-pairs' before
;; turning on the mode:
;;
;; (add-hook 'ruby-mode-hook
;;           (lambda ()
;;             (wrap-region-restrict-mode-pairs '("\"" "'" "("))
;;             (wrap-region-mode t)))

;; You can also pass a major mode to this function if you want to set
;; all mode specific pairs at the same place:
;;
;; (wrap-region-restrict-mode-pairs '("\"" "'" "(") 'ruby-mode)
;; (wrap-region-restrict-mode-pairs '("[" "{" "(") 'java-mode)

;; If no region is selected so there's nothing to wrap, wrap-region
;; will insert the first part of the pair, unless
;; `wrap-region-insert-both' is set to t. In that case both parts
;; will be inserted and the cursor placed in between them.

;; By default "<" is used as a single character corresponding to ">".
;; This is probably the desired behaviour in languages such as Java,
;; where this syntax is used:
;; Set<String> set = new HashSet<String>();
;;
;; But in markup languages, such as HTML and XML, you use tags. To
;; enable them, set `wrap-region-do-tags' to t before activating
;; wrap-region:
;;
;; (add-hook 'rhtml-mode-hook
;;           (lambda ()
;;             (setq wrap-region-do-tags t)
;;             (wrap-region-mode t)))
;;
;; You can now wrap a region with a tag. When asked for the tag, you
;; may also include attributes, such as class or id.

;; wrap-region comes with a few default pairs (see
;; `wrap-region-pairs'). You can add your own pairs using
;; `wrap-region-add-pair':
;;
;; (wrap-region-add-pair "#" "#")

;;; Change Log:
;; - use alists instead of hash tables
;; - renames:
;;   `wrap-region-set-mode-punctuations' -> `wrap-region-restrict-mode-pairs'
;;   `wrap-region-insert-twice' -> `wrap-region-insert-both'
;;   `wrap-region-tag-active' -> `wrap-region-do-tags'
;;   `wrap-region-add-punctuation' -> `wrap-region-add-pair'
;;   `wrap-region-punctuations-table' -> `wrap-region-pairs'
;;   `wrap-region-mode-punctuations' -> `wrap-region-mode-pairs'
;;   `wrap-region-with-punctuation-or-insert' -> `wrap-region-with-pair-or-insert'
;;   `wrap-region-corresponding-punctuation' -> `wrap-region--corresponding-delimiter'
;; - remove unused `wrap-region-with-punctuations'
;; - fix formatting, docstrings and commentary

;;; FIXME:
;; - I'm not sure whether `wrap-region-{beginning,end}' really make
;;   sense -- won't (region-{beginning,end}) always suffice? --SN

;;; Code:

(defcustom wrap-region-insert-both nil
  "If non-nil, when inserting the left half of a pair,
the corresponding character will be inserted as well and the
cursor will be placed between the two."
  :group 'wrap-region)

(defvar wrap-region-mode-map (make-sparse-keymap)
  "Keymap for `wrap-region-mode'.")

(defvar wrap-region-pairs
  '(("\"" . "\"")
    ("'" .  "'")
    ("(" .  ")")
    ("{" .  "}")
    ("[" .  "]")
    ("<" .  ">")
    ("|" .  "|")
    ("\\" . "\\"))
  "Alist containing all defined character pairs.")

(defvar wrap-region-do-tags nil
  "*If non-nil, \"<\" is regarded as the beginning of a tag.")
(make-variable-buffer-local 'wrap-region-do-tags)

(defvar wrap-region-mode-pairs nil
  "Alist defining the mode-specific insertion pairs.
Elements are of the form (MODENAME (LIST OF LEFTs)) where
\"LEFT\" means the first (i.e. normally left) part of the
insertion pair. The whole pair must be defined in
`wrap-region-pairs'.")

(defvar wrap-region-before-hook nil
  "Evaluated before the region is wrapped.
Two variables are available in the hook: wrap-region-beginning
which is the beginning of the region and wrap-region-end which is
the end of the region.")

(defvar wrap-region-after-hook nil
  "Evaluated after the region is wrapped.
Two variables are available in the hook: wrap-region-beginning
which is the beginning of the region and wrap-region-end which is
the end of the region.")

(defun wrap-region-with-pair-or-insert (left)
  "Wrap a region, if any, else insert the delimiter(s)."
  (interactive)
  (if mark-active
      (wrap-region left (wrap-region--corresponding-delimiter left)
                   (region-beginning) (region-end))
    (wrap-region-insert left)))

(defun wrap-region-with-tag-or-insert ()
  "Wrap a region with a tag if the region is active.
Otherwise insert the delimiter(s)."
  (interactive)
  (if mark-active
      (call-interactively 'wrap-region-with-tag)
    (wrap-region-insert "<")))

(defun wrap-region-with-tag (tag)
  "Wrap a region with a tag."
  (interactive "*sTag (with optional attributes): ")
  (let* ((elements (split-string tag " "))
         (tag-name (car elements))
         (tag-right (concat "</" tag-name ">"))
         (tag-left (concat "<" (if (= (length elements) 1) tag-name tag) ">")))
    (wrap-region tag-left tag-right (region-beginning) (region-end))))

(defun wrap-region-insert (left)
  "Insert LEFT or LEFT and its corresponding right part if `wrap-region-insert-both' is non-nil."
  (insert left)
  (when wrap-region-insert-both
    (insert (wrap-region--corresponding-delimiter left))
    (backward-char)))

(defun wrap-region (left right beg end)
  "Wrap region with LEFT and RIGHT."
  (let ((wrap-region-beginning beg) (wrap-region-end end))
    (run-hooks 'wrap-region-before-hook)
    (save-excursion
      (goto-char beg)
      (insert left)
      (goto-char (+ end (length left)))
      (insert right))
    (run-hooks 'wrap-region-after-hook)))

(defun wrap-region--corresponding-delimiter (c)
  "Return the corresponding delimiter to the one given.
Return nil if such pair does not exist."
  (cdr (assoc c wrap-region-pairs)))

(defun wrap-region-add-pair (left right)
  "Add a new insertion pair to `wrap-region-pairs'."
  (add-to-list 'wrap-region-pairs (cons left right)))

(defun wrap-region-restrict-mode-pairs (lefts &optional mode)
  "Add a list of MODE-specific values to `wrap-region-mode-pairs'.
Use this when the pairs should be customized depending on the
major mode.  MODE argument is optional and defaults to `major-mode'.
See the value of `wrap-region-pairs' for valid LEFTS."
  (add-to-list 'wrap-region-mode-pairs (cons (or mode major-mode) lefts)))

;;;###autoload
(define-minor-mode wrap-region-mode
  "Toggle Wrap Region minor mode.
With ARG, turn Wrap Region mode on if and only if ARG is positive
or t.
Wrap Region mode redefines some self-inserting keys to wrap
current region with delimiter pairs or tags. You can customize
its behaviour using the following variables:

`wrap-region-pairs', `wrap-region-mode-pairs',
`wrap-region-insert-both', `wrap-region-do-tags',
`wrap-region-before-hook' and `wrap-region-after-hook'."
  :init-value nil
  :lighter " wr"
  :keymap wrap-region-mode-map
  (if wrap-region-mode
      (let ((lefts (cdr (assoc major-mode wrap-region-mode-pairs))))
        (unless lefts
          (setq lefts (mapcar 'car wrap-region-pairs)))
        (dolist (left lefts)
          (define-key wrap-region-mode-map left
            `(lambda () (interactive)
               (wrap-region-with-pair-or-insert ,left))))
        (if wrap-region-do-tags
            (define-key
              wrap-region-mode-map "<" 'wrap-region-with-tag-or-insert)))))

(provide 'wrap-region)

;;; wrap-region.el ends here
