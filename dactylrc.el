;;; dactylrc.el --- major mode for editing Dactyl rc files

;; Author: Štěpán Němec <stepnem@gmail.com>
;; Time-stamp: "2011-07-28 22:30:20 CEST stepnem"
;; Created: 2010-12-09 03:01:56 Thursday +0100
;; Keywords: external, modes, dactyl
;; Licence: Whatever Works

;;; Commentary:

;; The mode uses data collected directly from the running Dactyl instance.
;; You'll need to run something like the following command (set the
;; `dactyl-info-file' variable to match the file used).

;; To use it for relevant files automatically, you'd usually do something like

;; (add-to-list 'auto-mode-alist '("\\(\\.dactyl\\|dactylrc\\)\\'" . dactylrc-mode))

;; commands.addUserCommand(["dactyl-info-update", "diu"], "Bla bla bla",
;;     function (args) {
;;         let cmds = "Commands:\n" + array.flatten(commands._exCommands.map(function (c) c.specs)).join("\n");
;;         let autocmds = "Autocommands:\n" + Object.keys(config.autocommands).join("\n");
;;         let opts = "Options:\n" + options._options.map(function (o) o.names.join("\n")).join("\n");

;;         let storefile = File("~/.pentadactyl/dactyl-info");
;;         let (s = [cmds, autocmds, opts].join("\n\n")) storefile.write(s);
;;     }, {}, true);

;; If you don't have MozRepl set up, `dactyl-execute' won't work.

;;; Code:

(require 'dotelib)
(require 'js)
(require 'mozrepl nil t)

(defconst dactylrc-version 0.1
  "The currently loaded version of the `dactylrc' library.")

(defvar dactyl-info-file "~/.pentadactyl/dactyl-info"
  "*File where the current list of relevant Dactyl identifiers is stored.
The list is auto-generated and used for font lock (syntax highlighting).")

(defvar dactyl-commands nil)
(defvar dactyl-autocommands nil)
(defvar dactyl-options nil)

(defun dactyl-info-init ()
  (with-temp-buffer
    (insert-file-contents-literally dactyl-info-file)
    (mapc (lambda (e)
            (let ((var (car e)) (heading (cdr e)) res)
              (goto-char (point-min))
              (search-forward (concat heading "\n"))
              (while (not (or (and (bolp) (eolp))
                              (= (point) (point-max))))
                (push (buffer-substring (line-beginning-position)
                                        (line-end-position))
                      res)
                (forward-line))
              (set var res)))
          '((dactyl-commands . "Commands:")
            (dactyl-autocommands . "Autocommands:")
            (dactyl-options . "Options:")))))

(or dactyl-commands dactyl-autocommands dactyl-options (dactyl-info-init))

(defvar dactyl-command-regexp
  (mapconcat (lambda (c) (concat "^:?" (.vim-syntax-keyword-debracket c)))
             dactyl-commands "\\|"))
(defvar dactyl-autocommand-regexp (regexp-opt dactyl-autocommands))
(defvar dactyl-option-regexp (regexp-opt dactyl-options))
(defvar dactylrc-font-lock-keywords
  `((,dactyl-command-regexp 0 font-lock-function-name-face)
    (,(concat "set +\\(" dactyl-option-regexp "\\)") 1 font-lock-variable-name-face)
    (,dactyl-autocommand-regexp 0 font-lock-type-face)
    ;; ("^[^\"\n]*\\(\"[^\"\n]*\\)$" 1 font-lock-comment-face)
    ;; ("^\".*$" 0 font-lock-comment-face t)
    ;; ("\\([\"']\\).*?\\1" . font-lock-string-face)
    ("^.*?map " ("\\(\\(?:<.*?\\)?>\\)" nil nil (1 font-lock-constant-face)))
    ;; ("^:?\\(?:javascript\\|js\\) +<<\\(.+\\)\n\\(?:\n\\|.\\)*?\n\\1" 0 nil t)
    (dactylrc--fontify-js-blocks (0 nil))))

(defun dactylrc--font-lock-string-matcher (limit)
  (when (re-search-forward "\\(\"\\).*?\\(\"\\)" limit t)
    (not (or (eq (char-after (line-beginning-position)) ?\")
             (eq (char-before (1- (point))) ?\\)))))

(defvar dactylrc-font-lock-syntactic-keywords
  '(
    ;;("^:?\\(?:javascript\\|js\\) +<<\\(.+\\)\n\\(?:\n\\|.\\)*?\\(?:^\\s *\\(/+\\)\n\\1" 0 nil t)
    ;; ("\\(\"\\).*?\\(\"\\)" (1 "\"") (2 "\""))
    (dactylrc--font-lock-string-matcher (1 "\"") (2 "\""))
    ))

(declare-function org-src-font-lock-fontify-block "org-src.el" (lang start end))
(defun dactylrc--fontify-js-blocks (limit)
  (let* ((beg (when (search-forward-regexp
                     "\\(?:^:?\\| -\\)\\(?:javascript\\|js\\) +<<\\(.+\\)$" nil t)
                (match-beginning 0)))
         (cbeg (line-beginning-position 2))
         (end (when beg
                (search-forward-regexp
                 (concat "^" (match-string 1) "$") nil t)
                (match-end 0)))
         (cend (1- (line-beginning-position))))
    (when end
      (add-text-properties beg end
                           `(font-lock-fontified t font-lock-multiline t
                                                 syntax-table ,js-mode-syntax-table))
      (org-src-font-lock-fontify-block "js" cbeg cend)
      t)))

(defun dactylrc--current-js-block-bounds ()
  (save-excursion
    (let* ((p (point))
           (beg (when (search-backward-regexp
                       "\\(?:^:?\\| -\\)\\(?:javascript\\|js\\) +<<\\(.+\\)$" nil t)
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
  (mapc (& 'apply 'modify-syntax-entry) '((?\" "<") (?\n ">") (?' "\"")))
  (setq font-lock-defaults
        '(dactylrc-font-lock-keywords
          nil nil nil nil
          (font-lock-syntactic-keywords . dactylrc-font-lock-syntactic-keywords))))

(mapc (apply-partially 'apply 'define-key dactylrc-mode-map)
      '(("\C-c\C-e" dactyl-execute)))

;;;###autoload
(defun dactyl-execute (&optional beg end)
  "Execute lines in the region as Dactyl Ex commands.
When the region is not active, use the current line.
Uses MozRepl to send the commands to the running Dactyl instance."
  (interactive "r")
  (let ((f (lambda ()
             (moz-send-region
              (concat "dactyl.execute('"
                      (buffer-substring-no-properties (line-beginning-position)
                                                      (line-end-position))
                      "')")))))
    (if (region-active-p)
        (save-excursion
          (goto-char beg)
          (let ((end (save-excursion (goto-char end) (line-beginning-position))))
            (while (<= (point) end)
              (funcall f)
              (forward-line))))
      (funcall f))))

(provide 'dactylrc)
;;; dactylrc.el ends here
