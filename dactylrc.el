;;; dactylrc.el --- major mode for editing Dactyl rc files

;; Author: Štěpán Němec <stepnem@gmail.com>
;; Created: 2010-12-09 03:01:56 Thursday +0100
;; Keywords: external, modes, dactyl
;; Licence: Whatever Works

;;; Commentary:

;; http://dactyl.sf.net/

;; To activate the mode for relevant files automatically, you'd usually do
;; something like

;; (add-to-list 'auto-mode-alist
;;              '("\\(\\.penta\\|dactylrc\\)\\'" . dactylrc-mode))

;; The mode uses data collected directly from the running Dactyl instance.
;; You'll need to run something like the following command to sync the data
;; (set the `dactyl-info-file' variable to match the file used):

;; group.commands.add(["dactyl-info-update", "diu"], "Bla bla bla",
;;     function (args) {
;;         let cmds = "Commands:\n" +
;;             array(c.specs for (c in commands.iterator()))
;;             .flatten().join("\n");
;;         let autocmds = "Autocommands:\n" + Object.keys(config.autocommands)
;;             .join("\n");
;;         let boolopts = "Boolean Options:\n" + array(o.realNames.join("\n")
;;             for (o in options) if (o.type == "boolean")).join("\n");
;;         let otheropts = "Other Options:\n" + array(o.names.join("\n")
;;             for (o in options) if (o.type != "boolean")).join("\n");
;;
;;         File("~/.pentadactyl/dactyl-info")
;;             .write([cmds, autocmds, boolopts, otheropts].join("\n\n"));
;;     }, {}, true);

;; If you don't have MozRepl set up, `dactyl-execute' won't work.

;; Corrections and constructive feedback appreciated.

;;; Code:

(require 'dotelib)            ; `.setq-local', `.vim-syntax-keyword-debracket'
(require 'js)

(defconst dactylrc-version 0.1
  "The currently loaded version of the `dactylrc' library.")

(defvar dactyl-info-file "~/.pentadactyl/dactyl-info"
  "*File where the current list of relevant Dactyl identifiers is stored.
The list is auto-generated and used for font lock (syntax highlighting).")

(defvar dactyl-commands nil)
(defvar dactyl-autocommands nil)
(defvar dactyl-boolopts nil)
(defvar dactyl-otheropts nil)

(defun dactyl-info-init ()
  (with-temp-buffer
    (insert-file-contents-literally dactyl-info-file)
    (mapc (lambda (e)
            (let ((var (car e)) (heading (cdr e)) res)
              (goto-char (point-min))
              (search-forward (concat heading ":\n"))
              (while (not (or (and (bolp) (eolp))
                              (= (point) (point-max))))
                (push (buffer-substring (line-beginning-position)
                                        (line-end-position))
                      res)
                (forward-line))
              (set var res)))
          '((dactyl-commands . "Commands")
            (dactyl-autocommands . "Autocommands")
            (dactyl-boolopts . "Boolean Options")
            (dactyl-otheropts . "Other Options")))))

(or dactyl-commands dactyl-autocommands dactyl-boolopts dactyl-otheropts
    (dactyl-info-init))

(defface dactylrc-boolopt-prefix-face
    '((t :inherit font-lock-variable-name-face :italic t))
  "Face for the \"inv\"/\"no\" boolean option prefix.
So I figured, when already going to the pains of distinguishing the boolean
options, let's get fancy, huh?")

(defvar dactylrc-font-lock-keywords
  `((,(mapconcat (lambda (c) (concat "^:?" (.vim-syntax-keyword-debracket c)))
                 dactyl-commands "\\|")
     0 font-lock-function-name-face)
    ("\\<set\\> +" (,(concat "\\<\\(" (regexp-opt dactyl-otheropts) "\\)\\>")
                     nil nil (1 font-lock-variable-name-face))
                   ;; doesn't work !@###$%
                   (,(concat "\\<\\(inv\\|no\\)?\\("
                             (regexp-opt dactyl-boolopts)
                             "\\)\\>")
                     nil nil
                     (1 dactylrc-boolopt-prefix-face)
                     (2 font-lock-variable-name-face)))
    (,(regexp-opt dactyl-autocommands) 0 font-lock-type-face)
    ("^.*?map " ("\\(\\(?:<.*?\\)?>\\)" nil nil (1 font-lock-constant-face)))
    (dactylrc--fontify-js-blocks (0 nil))))

(defun dactylrc--font-lock-string-matcher (limit)
  (when (re-search-forward "\\(\"\\).*?\\(\"\\)" limit t)
    (not (or (eq (char-after (line-beginning-position)) ?\")
             (eq (char-before (1- (point))) ?\\)))))

(defvar dactylrc-font-lock-syntactic-keywords
  '((dactylrc--font-lock-string-matcher (1 "\"") (2 "\""))))

(defvar dactylrc--js-block-start-regexp
  "\\(?:^:?\\| -\\)\\(?:javascript\\|js\\) +<<\\(.+\\)$")

(autoload 'org-src-font-lock-fontify-block "org-src")
(defun dactylrc--fontify-js-blocks (limit)
  (let* ((beg (when (search-forward-regexp dactylrc--js-block-start-regexp
                                           nil t)
                (match-beginning 0)))
         (cbeg (line-beginning-position 2))
         (end (when beg
                (search-forward-regexp
                 (concat "^" (match-string 1) "$") nil t)
                (match-end 0)))
         (cend (1- (line-beginning-position))))
    (when end
      (add-text-properties
       beg end `(font-lock-fontified t font-lock-multiline t
                                     syntax-table ,js-mode-syntax-table))
      (org-src-font-lock-fontify-block "js" cbeg cend)
      t)))

(defun dactylrc--current-js-block-bounds ()
  (save-excursion
    (let* ((p (point))
           (beg (when (search-backward-regexp dactylrc--js-block-start-regexp
                                              nil t)
                  (line-beginning-position 2)))
           (end (when beg
                  (search-forward-regexp
                   (concat "^" (match-string 1) "$") nil t)
                  (line-beginning-position))))
      (and end (<= beg p) (< p end)
           (cons beg (1- end))))))

(defun dactylrc-indent-line ()
  (if (dactylrc--current-js-block-bounds)
      (js-indent-line)
    (indent-relative)))

;;; FIXME this is just a workaround until Emacs improves its multi-syntax
;;; support, cf. GNU Emacs bug #9148
(defadvice comment-normalize-vars (around dactylrc activate)
  (when (derived-mode-p 'dactylrc-mode)
    (.setq-local comment-start (if (dactylrc--current-js-block-bounds)
                                   "//"
                                 "\"")))
  ad-do-it)

;;;###autoload
(define-derived-mode dactylrc-mode prog-mode "Dactyl"
  (.setq-local comment-start nil
               comment-use-syntax t
               indent-line-function 'dactylrc-indent-line)
  (mapc (apply-partially 'apply 'modify-syntax-entry)
        '((?\" "<") (?\n ">") (?' "\"")))
  (setq font-lock-defaults
        '(dactylrc-font-lock-keywords
          nil nil nil nil
          (font-lock-syntactic-keywords .
           dactylrc-font-lock-syntactic-keywords))))

(mapc (apply-partially 'apply 'define-key dactylrc-mode-map)
      '(("\C-c\C-e" dactyl-execute)))

(autoload 'moz-send-region "moz")
;;;###autoload
(defun dactyl-execute (&optional beg end go)
  "Execute lines in the region as Dactyl Ex commands.
When the region is not active, use the current line. Uses MozRepl
to send the commands to the running Dactyl instance. When GO is
non-nil or an interactive argument is provided, display the
MozRepl buffer."
  (interactive "r\nP")
  (let ((f (lambda ()
             (moz-send-region
              (concat "dactyl.execute('"
                      (buffer-substring-no-properties (line-beginning-position)
                                                      (line-end-position))
                      "')")))))
    (if (region-active-p)
        (save-excursion
          (goto-char beg)
          (let ((end (save-excursion (goto-char end)
                                     (line-beginning-position))))
            (while (<= (point) end)
              (funcall f)
              (forward-line))))
      (funcall f))
    (when go (inferior-moz-switch-to-mozilla))))

(provide 'dactylrc)
;;; dactylrc.el ends here
