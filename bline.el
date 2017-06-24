(require 'whitespace)

(defvar bline-minor-mode-font-lock-keywords
  ;; cf. `whitespace-color-on'
  (list
   (list
    (let ((line-column (or whitespace-line-column fill-column)))
      (format
       "^\\([^\t\n]\\{%s\\}\\|[^\t\n]\\{0,%s\\}\t\\)\\{%d\\}%s\\(.+\\)$"
       tab-width
       (1- tab-width)
       (/ line-column tab-width)
       (let ((rem (% line-column tab-width)))
         (if (zerop rem)
             ""
           (format ".\\{%d\\}" rem)))))
    (if t                     ;(memq 'lines whitespace-active-style)
        0                     ; whole line
      2)                      ; line tail
    (check-face 'whitespace-line) t)))  ; stupid compiler warnings

;;;###autoload
(define-minor-mode bline-minor-mode "Overlong lines can make you blined."
  nil nil nil
  (if bline-minor-mode
      (font-lock-add-keywords nil bline-minor-mode-font-lock-keywords t)
    (font-lock-remove-keywords nil bline-minor-mode-font-lock-keywords))
  (font-lock-mode 1))

(defun bline-minor-mode--insin ()
  (add-hook 'after-change-functions 'bline-minor-mode--uate nil t))

(defun bline-minor-mode--uate (&rest ignore)
  (bline-minor-mode 1)
  (remove-hook 'after-change-functions 'bline-minor-mode--uate t))

(add-hook 'prog-mode-hook 'bline-minor-mode--insin)

(provide 'bline)
