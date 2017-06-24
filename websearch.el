;;; websearch.el --- Search SITE for TERM in the default browser.

;; Author: Štěpán Němec <stepnem@gmail.com>
;; Time-stamp: "2017-06-20 11:59:11 CEST stepnem"
;; Created: 2017-06-20 11:31:45 Tuesday +0200
;; Keywords: www, convenience
;; Licence: Whatever Works
;; Tested-with: GNU Emacs 24

;;; Commentary:

;; Corrections and constructive feedback appreciated.

;;; Code:

(require 'dotelib)

(defvar websearch-alist nil "Zizi")
(defvar websearch-site-history nil "")
(defvar websearch-term-history nil "")
(defun websearch--interactive-args ()
  (list (.complete-with-default
         "Site" websearch-alist 'websearch-site-history nil t)
        (.read-string-with-default
         "Term" 'websearch-term-history (.region-or 'word) t)))

(defvar websearch-dactyl-history nil "")
(defvar websearch-dactyl-keywords '("tls") "")
(autoload 'laproscope-send-region "laproscope")
(defun websearch-dactyl (site &optional term)
  (interactive
   (list (completing-read "Site: " websearch-dactyl-keywords)
         (.read-string-with-default
          "Term" 'websearch-term-history (.region-or 'word))))
  (laproscope-send-region
    (concat "dactyl.execute('tabopen " site " " term "')")))

;;;###autoload
(defun websearch (site &optional term)
  "Search SITE for TERM in the default browser.
See `websearch-alist' for the search definitions."
  (interactive (websearch--interactive-args))
  (let* ((plist (assoc-default site websearch-alist))
         (get (& 'plist-get plist))
         (handler ($ get :handler)))
    (if handler ($ handler term)
      (let ((template ($ get :url))
            (coding ($ get :coding)))
        (browse-url (format template (.urlencode term coding)))))))

(defmacro define-websearch-quick-cmd (site-form &optional term-form name)
  (declare (debug t) (indent 1))
  `(defun ,(.format-symbol "websearch-quick-%s" (or name site-form)) ()
     (interactive)
     (websearch ,site-form ,(or term-form `(.region-or 'Word)))))

(define-websearch-quick-cmd "zh-youdao"
  (.region-or (substring (thing-at-point 'line) 5 -1))
  "fraus-zh-youdao")
(define-websearch-quick-cmd "g-zh-cn")
(define-websearch-quick-cmd "zdic" (.region-or 'char))

(defun websearch-init ()
  (interactive)
  (cl-labels ((parse-bmark ()
                (when (re-search-forward "^bmark +\\(.*\\)" nil t)
                  (goto-char (match-beginning 1))
                  (let ((eol (line-end-position)) plist url)
                    (while (< (point) eol)
                      (let ((thing (symbol-name (read (current-buffer)))))
                        (if (string-prefix-p "-" thing)
                            (let* ((=pos (cl-position ?= thing))
                                   (key (substring thing 1 =pos))
                                   (val (if (= (1+ =pos) (length thing))
                                            (read (current-buffer))
                                          (substring thing (1+ =pos)))))
                              (push (intern (concat ":" key)) plist)
                              (push (if (string= key "charset")
                                        (intern (downcase val))
                                      val)
                                    plist))
                          (setq url thing))))
                    (setq plist (nreverse plist))
                    (or (and (plist-get plist :post)
                             (plist-get plist :keyword))
                        (cons url plist))))))
    (setq websearch-alist
         `(("Hanjadic Bravender" :url "http://hanjadic.bravender.us/%s")
           ("Hanja Naver" :url "http://hanja.naver.com/search?query=%s")
           ("Endic Naver" :url "http://endic.naver.com/search?query=%s")
           ,@(let (b r)
               (with-current-buffer
                   (find-file-noselect "~/.pentadactyl/bmark.penta")
                 (save-excursion
                   (goto-char (point-min))
                   (search-forward "\" BOOKMARKS")
                   (while (setq b (parse-bmark))
                     (if (stringp b)
                         (add-to-list 'websearch-dactyl-keywords b)
                       (push `(,(plist-get (cdr b) :keyword)
                               :url ,(car b)
                               :coding ,(plist-get (cdr b) :charset))
                             r)))))
               r)
           ,@(mapcar (λ (k) `(,k :handler (lambda (term) ; λ doesn't work here
                                            (websearch-dactyl ,k term))))
                     websearch-dactyl-keywords)
           ("vi-nom-tccn"
            :handler (lambda (term)
                       (if (= (string-width term) (length term))
                           (websearch "vi-nom-tccn-qn" term)
                         (moz-send-region
                          (concat "dactyl.open([[ \
\"http://nomfoundation.org/vn/cong-cu-nom/Tra-cuu-chu-Nom/Tra-cuu-chu-Nom\",
\"anything=rNom&inputText=" (.urlencode term) "\" ]], dactyl.NEW_TAB)")))))))))

(websearch-init)

(defconst websearch-version 0.1 "The currently loaded version of the `websearch' library.")

(defun websearch-reload ()
  "Reload the `websearch' library."
  (interactive)
  (unload-feature 'websearch t)
  (require 'websearch))

(provide 'websearch)
;;; websearch.el ends here
(defun websearch-test ()
  (interactive)
  (start-process "websearch-test" nil "emacs" "-Q" "-r"
                 "-l" "~/.emacs.d/init/test-init.el"
                 "-l" "/home/stepnem/.emacs.d/load/websearch.el"))
