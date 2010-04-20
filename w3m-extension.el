;;; w3m-extension.el --- emacs-w3m extensions

;; Author: Andy Stewart <lazycat.manatee@gmail.com>
;; Maintainer: Andy Stewart <lazycat.manatee@gmail.com>
;; Copyright (C) 2008, 2009, Andy Stewart, all rights reserved.
;; Created: 2008-06-07 22:06:58
;; Version: 0.2.5
;; Modified-by: Štěpán Němec <stepnem@gmail.com>
;; Last-Updated: 2008-09-24 15:53:19
;; URL: not distributed yet
;; Keywords: emacs-w3m, w3m, Google, search
;; Compatibility: GNU Emacs 23.0.60.1

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.

;; Features that might be required by this library:
;;
;; `w3m-util' `w3m-proc' `w3m-form' `w3m-lnum' `w3m' `auto-install'
;; `basic-edit-toolkit' `lazycat-toolkit' `wget' `org-compat'
;; `rcirc-notify+'
;;

;;; Installation:
;;
;; Copy w3m-extension.el to your load-path and add the following to
;; your `user-init-file':
;;
;;  (require 'w3m-extension)

;;; Commentary:
;;
;; A collection of functions that extend functionality of emacs-w3m.
;;

;;; Change log:
;;
;;      2010/02/23
;;              Code cleanup.
;;
;;              Rename `w3m-emacswiki-view-regexp' to
;;              `w3m-view-regexp-url'; the functionality is general,
;;              has nothing to do with EmacsWiki in particular.
;;              Similarly:
;;              `w3m-gmail-toggle-mark' -> `w3m-form-toggle-marks'
;;              `w3m-gmail-mark-all' -> `w3m-form-mark-all'
;;
;;              Remove redundant `w3m-gmail-unmark-all'
;;
;;              `w3m-auto-logon-gmail' -> `w3m-goto-gmail'
;;              `w3m-gmail-login-string' -> `w3m-gmail-login-url'
;;              `w3m-search-advance' -> `w3m-search-plus'
;;              `w3m-search-emacswiki-random' -> `w3m-goto-emacswiki-random'
;;              `w3m-search-advance-search-object' -> `w3m-search-plus-search-term'
;;              `toggle-w3m-with-other-buffer' -> `w3m-toggle-with-other-buffer'
;;              prettyfy -> prettify
;;
;;              Remove some useless comments.
;;
;;              Fix docstrings.
;;---------------------------------------------------------------------------------
;;      2008/09/24
;;              Remove function `w3m-search+' and replace with `w3m-search-advance'
;;
;;      2008/09/22
;;              Add functions: `w3m-goto-linknum' `w3m-gmail-toggle-mark'
;;              `w3m-gmail-mark-all' `w3m-gmail-unmark-all'.
;;
;;      2008/08/30
;;              Add function `w3m-search+', and modified some old search function
;;              to make these base on `w3m-search+'.
;;
;;      2008/08/28
;;              Add two function `w3m-search-slang' and `w3m-search-slang+'.
;;
;;      2008/07/20
;;              Modified some function about Google search.
;;
;;              Make all search function open new search result page in background session.
;;
;;              Extension function `w3m-search-dict-cn+' and `w3m-search-google-web+' to make
;;              them can search current mark sentence.
;;
;;      2008/06/18
;;              Replace some `w3m-goto-url-new-session' with `w3m-view-this-url-1'
;;              to make pages is open background and don't wink screen case by 'w3m-goto-url-new-session.
;;
;;              Add some function that make can jump between Google search titles.
;;
;;      2008/06/17
;;              Add function that return the random pages from EmacsWiki.
;;
;;      2008/06/07
;;              Create this collect of search function. ^_^
;;

;;; Acknowledgments:
;;
;; Emacs guys.
;;


;;; Require
(require 'w3m-util)
(require 'w3m-proc)
(require 'w3m-form)
(require 'w3m-lnum)
(require 'w3m)
(require 'basic-edit-toolkit)

;;; Code:

(defvar google-desktop-search-url nil
  "The unique string per computer that Google Desktop Search uses.
You can copy it from a web browser's address bar.")

(defvar w3m-search-plus-prettify-string-length 25
  "The length of SEARCH-TERM displayed by `w3m-search-plus'.")

(defvar w3m-search-plus-search-term nil
  "The search term used by `w3m-search-plus'.")

(defvar w3m-gmail-login-url ""
  "The URL string for Gmail login.")

;;; FIXME this all is kinda weird -- dynamic variables,
;;; mentioning an internal variable in the docstring...
;;; and all the design
(defun w3m-search-plus (search-url prompt-string
                                   &optional coding
                                   prefix-search-term postfix-search-term
                                   search-url-follow search-url-last
                                   foreground
                                   upcase-p downcase-p capitalize-p)
  "Advanced w3m search function.
Default, if mark active, will set SEARCH-TERM to the contents
of current region, otherwise, use current word.

Set SEARCH-URL for special search.
PROMPT-STRING is used as a prompt.
If CODING is set, use it to encode SEARCH-TERM.
PREFIX-SEARCH-TERM is added before SEARCH-TERM
POSTFIX-SEARCH-TERM is appended to SEARCH-TERM.
SEARCH-URL-FOLLOW is an URL fragment appended to SEARCH-URL.
SEARCH-URL-LAST is a fragment appended to SEARCH-URL after all other elements.
If FOREGROUND is non-nil, make search page open in foreground, otherwise open in background.
If UPCASE-P is non-nil, upcase SEARCH-TERM.
If DOWNCASE-P is non-nil, downcase SEARCH-TERM.
If CAPITALIZE-P is non-nil, capitalize SEARCH-TERM."
  (let* ((guessed-term
          (if mark-active
              (prog1
                  (buffer-substring (region-beginning) (region-end))
                (deactivate-mark))
            (current-word)))
         (search-term (read-string
                        (concat
                         prompt-string
                         (format " (%-s): "
                                 (prettify-string
                                  guessed-term
                                  w3m-search-plus-prettify-string-length))) nil nil guessed-term)))
    ;; save `search-term' to enable user to edit it conveniently
    ;; If you want to edit `search-term' by default when input in `read-string'
    ;; just use `w3m-search-plus-insert-search-term' to yank and edit it. :)
    (setq w3m-search-plus-search-term search-term)

    ;; `search-term' transform.
    (setq search-term
          (cond (upcase-p
                 (upcase search-term))
                (downcase-p
                 (downcase search-term))
                (capitalize-p
                 (capitalize search-term))
                (t search-term)))
    ;; encode `search-term' with `coding'
    (or prefix-search-term (setq prefix-search-term ""))
    (or postfix-search-term (setq postfix-search-term ""))
    (or search-url-follow (setq search-url-follow ""))
    (or search-url-last (setq search-url-last ""))
    (setq search-term (w3m-url-encode-string (concat prefix-search-term search-term postfix-search-term) coding))
    (setq search-url (concat search-url search-url-follow search-term search-url-last))
    (if foreground
        (w3m-browse-url search-url t)           ;open in foreground
      (w3m-view-this-url-1 search-url nil t)))) ;open in background


(defun w3m-search-plus-insert-search-term ()
  "Insert `w3m-search-plus-search-term' at point."
  (interactive)
  (when w3m-search-plus-search-term
    (insert w3m-search-plus-search-term)))

(defun w3m-search-slang ()
  "Search for a word on urbandictionary.com."
  (interactive)
  (w3m-search-plus "http://www.urbandictionary.com/define.php?term=" "English Slang" 'utf-8))

(defun w3m-search-dict-cn ()
  "Search for a word on dict.cn."
  (interactive)
  (w3m-search-plus "http://dict.cn/search/?q=" "English Dict.cn" 'gbk))

(defun w3m-search-google-code ()
  "Search using Google Code search."
  (interactive)
  (w3m-search-plus "http://www.google.com/codesearch?hl=zh-CN&lr=&q=" "Google Code" 'utf-8))

(defun w3m-search-google-lucky ()
  "Search using Google Lucky search."
  (interactive)
  (w3m-search-plus "http://www.google.com/search?btnI=I%27m+Feeling+Lucky&q=" "Google Lucky" 'utf-8))

(defun w3m-search-google-image ()
  "Search using Google image search."
  (interactive)
  (w3m-search-plus "http://images.google.com/images?sa=N&tab=wi&q=" "Google Image" 'utf-8))

(defun w3m-search-google-blog-cn ()
  "Search using Google (Chinese) blog search."
  (interactive)
  (w3m-search-plus "http://blogsearch.google.com/blogsearch?hl=zh-CN&ie=UTF-8&oe=UTF-8&q=" "Google Blog CN" 'utf-8))

(defun w3m-search-google-blog-en ()
  "Search using Google (English) blog search."
  (interactive)
  (w3m-search-plus "http://blogsearch.google.com/blogsearch?hl=en&ie=UTF-8&oe=UTF-8&q=" "Google Blog EN" 'utf-8))

(defun w3m-search-google-group ()
  "Search Google Groups."
  (interactive)
  (w3m-search-plus "http://groups.google.com/groups?hl=zh-CN&ie=UTF-8&oe=UTF-8&q=" "Google Groups" 'utf-8))

(defun w3m-search-google-file ()
  "Use Google to search for a file named FILE.
This function adds some Google search syntax, making the search simpler.
Example:
If you want to search for a PDF of CHM about Emacs, type \"emacs pdf|chm\"."
  (interactive)
  (w3m-search-plus "http://www.google.com/search?&ie=UTF-8&oe=UTF-8&q=" "Google File" 'utf-8
                      "+intitle:(\"index of\"\|\"last modified\"\|\"parent of\") -inurl:htm -inurl:html -inurl:php -inurl:asp "))

(defun w3m-search-baidu-mp3 ()
  "Search mp3 on mp3.baidu.com."
  (interactive)
  (w3m-search-plus "http://mp3.baidu.com/m?f=ms&tn=baidump3&ct=134217728&lf=&rn=&lm=0&word=" "Baidu Mp3 Search" 'gbk))

(defun w3m-search-emacswiki ()
  "Search using EmacsWiki's Google Custom Search."
  (interactive)
  (w3m-search-plus "http://www.google.com/cse?cx=004774160799092323420:6-ff2s0o6yi&q=" "Emacswiki" 'utf-8))

(defun w3m-goto-emacswiki-random ()
  "Go to a random page on EmacsWiki."
  (interactive)
  (w3m-view-this-url-1 "http://www.emacswiki.org/cgi-bin/wiki?action=random" nil t))

(defun w3m-search-haskell-wiki ()
  "Search using HaskellWiki's Google Custom Search."
  (interactive)
  (w3m-search-plus "http://www.google.com/cse?cx=014102838545582129901%3Anhonl7a8bw8&q=" "Haskell Wiki" 'utf-8))

(defun w3m-search-rfc-number ()
  "Search for a RFC number on www.ietf.org."
  (interactive)
  (w3m-search-plus "http://www.ietf.org/rfc/rfc" "RFC Number" 'utf-8 nil ".txt"))

(defun w3m-search-lispdoc-basic ()
  "Search using lispdoc.com `basic-search'."
  (interactive)
  (w3m-search-plus "http://lispdoc.com?q=" "Lispdoc basic search" nil nil nil nil "&search=Basic+search/"))

(defun w3m-search-lispdoc-full ()
  "Search using lispdoc.com `full-search'."
  (interactive)
  (w3m-search-plus "http://lispdoc.com?q=" "Lispdoc basic search" nil nil nil nil "&search=Full+search/"))

(defun w3m-search-google-web-cn ()
  "Search www.google.cn."
  (interactive)
  (w3m-search-plus "http://www.google.cn/search?&hl=zh-CN&lr=lang_zh-CN%7Clang_zh-TW&inlang=zh-CN&q=" "Google Web CN" 'utf-8))

(defun w3m-search-google-web-en ()
  "Search using Google (English)."
  (interactive)
  (w3m-search-plus "http://www.google.com/search?&ie=UTF-8&oe=UTF-8&q=" "Google Web EN" 'utf-8))

(defun w3m-search-answers ()
  "Search on www.answers.com."
  (interactive)
  (w3m-search-plus "http://www.answers.com/" "answers.com" 'utf-8))

(defun w3m-search-haskell-hoogle ()
  "Search on haskell.org/hoogle/."
  (interactive)
  (w3m-search-plus "http://haskell.org/hoogle/?hoogle=" "Haskell Hoogle" 'utf-8))

(defun w3m-search-wikipedia-cn ()
  "Search on zh.wikipedia.org."
  (interactive)
  (w3m-search-plus "http://zh.wikipedia.org/wiki/" "zh.wikipedia.org" 'utf-8))

(defun w3m-search-wikipedia-en ()
  "Search on en.wikipedia.org."
  (interactive)
  (w3m-search-plus "http://en.wikipedia.org/wiki/" "en.wikipedia.org" 'utf-8))

;;; FIXME weren't these meant to also use w3m-search-plus?
;;; or at least s/search/goto/
(defun w3m-search-google-news-cn-Sci/Tech ()
  "Go to Google technology news."
  (interactive)
  (w3m-view-this-url-1 "http://news.google.cn/nwshp?tab=wn&ned=tcn&topic=t" nil t))

(defun w3m-search-google-news-en-Sci/Tech ()
  "Go to Google news search."
  (interactive)
  (w3m-view-this-url-1 "http://news.google.com/news?ned=tus&topic=t" nil t))

(defun w3m-download-with-wget-current-position()
  "Download current linked of W3m use Wget."
  (interactive)
  (if (and (require 'wget nil t)
           (require 'lazycat-toolkit nil t)
           (or (w3m-anchor)
               (w3m-image)))
      (progn
        (wget (or (w3m-anchor) (w3m-image)))
        (when wget-hide-status
          (wget-hide)))
    (message "Nothing to download at point")))

(defun w3m-search-google-desktop ()
  "Go to Google Desktop search.
The search URL of Google Desktop Search is created randomly when
first run. So if you want to use this function, you need to set
the variable `google-desktop-search-url' (you can copy the value
from your browser)."
  (interactive)
  (w3m-view-this-url-1 google-desktop-search-url nil t))

(defun w3m-goto-gmail ()
  "Go to your Gmail mailbox using w3m.
To use this, set the variable `w3m-gmail-login-url'."
  (interactive)
  (w3m-view-this-url-1 w3m-gmail-login-url nil t))

(defun w3m-auto-install-elisp ()
  "Automatic download and install elisp."
  (interactive)
  (when (require 'auto-install nil t)
    (if (eq major-mode 'w3m-mode)
        (save-excursion
          (goto-char (point-min))
          (if (search-forward-regexp "^Download")
              (progn
                (deactivate-mark)
                (auto-install-download (w3m-anchor)))
            (message "No download anchor found")))
      (message "Current mode is not `w3m-mode'"))))

(defun w3m-toggle-with-other-buffer ()
  "Switch to a w3m buffer or return to the previous buffer."
  (interactive)
  (let ((blist (buffer-list))
        (test (if (derived-mode-p 'w3m-mode)
                  (lambda () (not (derived-mode-p 'w3m-mode)))
                (lambda () (derived-mode-p 'w3m-mode)))))
    (while blist
      (if (with-current-buffer (car blist)
            (and (not (minibufferp)) (funcall test)))
          (progn
            (switch-to-buffer (car blist))
            (setq blist nil))
        (setq blist (cdr blist))))))

(defun w3m-open-rcirc-window ()
  "Open rcirc window in w3m."
  (interactive)
  (when (require 'rcirc-notify+ nil t)
    (split-window-vertically 10)
    (rcirc-notify+-jump-last-message-channel)
    (windmove-down)))

(defun w3m-startup-background ()
  "Startup w3m background."
  (interactive)
  (w3m-view-this-url-1 (w3m-input-url nil nil nil w3m-quick-start
                                      'feeling-lucky) nil t))

(defun w3m-google-desktop-url-open ()
  "Open file link that Google Desktop Search shows."
  (interactive)
  (let ((file (w3m-print-this-url))
        (url (w3m-print-current-url))
        (google-search-url google-desktop-search-url)) ;google-search-url is a unique string generated by Google Desktop Search
    (string-match "/\\?.*" google-search-url)
    (setq google-search-url (replace-match "" nil nil google-search-url 0))
    (if (string-match google-search-url url) ;the result of Google Desktop Search
        (progn
          (string-match ".*&url=file://" file) ;cut front of file
          (setq file (replace-match "" nil nil file 0))
          (string-match "&s.*" file)                    ;cut behind of file
          (setq file (replace-match "" nil nil file 0)) ;get local file path
          (find-file file))             ;open file is my function for open diversified files
      (message "This is not a valid Google Desktop Search result."))))

(defun w3m-delete-buffer-and-select-right ()
  "Delete current w3m buffer.
If current tab is at right side of tabs, select left tab, otherwise, select right tab."
  (interactive)
  (when (require 'tabbar nil t)
    (let* ((tabset (tabbar-current-tabset t))
           selected tab)
      (when tabset
        (setq selected (tabbar-selected-tab tabset))
        (setq tab (tabbar-tab-next tabset selected))
        (w3m-delete-buffer)
        (if tab                      ;if tab is not right side of tabs
            (tabbar-forward-tab))))))

(defun w3m-visual-scroll-up (&optional arg)
  "Visual scroll up with image and text."
  (interactive)
  (or arg (setq arg 1))
  (if (pos-visible-in-window-p (point-max))
      (message "End of buffer")
    (let ((cur (point))
          pos visible)
      (setq pos
            (save-excursion
              (while (and (search-forward "\n" nil t)
                          (= (length (pos-visible-in-window-p (point) nil t)) 2)))
              (1- (point))))
      (setq visible
            (pos-visible-in-window-p pos nil t))
      ;; if point is fully visible, we can go there
      (when (and (= (length visible) 2)
                 (not (= pos cur)))
        (goto-char pos))
      ;; if point is partly visible, we only go there if we absolutely
      ;; have to (point is already at the top)
      (when (and (= pos cur)
                 (null (pos-visible-in-window-p (1- (point)))))
        (forward-line 1))
      (set-window-vscroll nil (+ (window-vscroll) arg)))))

(defun w3m-visual-scroll-down (&optional arg)
  "Visual scroll down with image and text."
  (interactive)
  (or arg (setq arg 1))
  (if (pos-visible-in-window-p (point-min))
      (message "Beginning of buffer")
    (let ((cur (point))
          pos visible)
      (setq pos
            (save-excursion
              (while (and (search-backward "\n" nil t)
                          (= (length (pos-visible-in-window-p (point) nil t)) 2)))
              (+ 1 (point))))
      (setq visible
            (pos-visible-in-window-p pos nil t))
      (when (and (= (length visible) 2)
                 (not (= pos cur)))
        (goto-char pos))
      (when (and (= pos cur)
                 (null (pos-visible-in-window-p
                        (save-excursion (forward-line 1) (point)))))
        (goto-char (1- (point))))
      (when (zerop (window-vscroll))
        (message "vscroll is 0. Reverting to scroll-down.")
        (scroll-down arg))
      (set-window-vscroll nil (- (window-vscroll) arg)))))

(defun w3m-goto-linknum ()
  "Turn on link numbers and ask for one to go to."
  (interactive)
  (let ((active w3m-link-numbering-mode)
        action
        number)
    (when (not active) (w3m-link-numbering-mode))
    (unwind-protect
        (w3m-move-numbered-anchor (read-number "Anchor number: "))
      (when (not active) (w3m-link-numbering-mode)))))

;;; Supposed to work on Gmail, see ChangeLog
(defun w3m-form-toggle-marks ()
  "Toggle form marks on a web page."
  (interactive)
  (goto-char (point-min))
  (when (search-forward-regexp "\\[\\(\\*\\| \\)\\]" nil t)
    (backward-char 4)
    (w3m-form-goto-next-field)
    (while (or
            (looking-at "[*]")
            (looking-at "[ ]"))
      (w3m-view-this-url)
      (w3m-form-goto-next-field))))

(defun w3m-form-mark-all (&optional ARG)
  "Mark (or unmark, with ARG) all form fields on a web page."
  (interactive "P")
  (goto-char (point-min))
  (when (search-forward (if unmark "[*]" "[ ]") nil t)
    (backward-char 4)
    (w3m-form-goto-next-field)
    (while (looking-at (if unmark "[*]" "[ ]"))
      (w3m-view-this-url)
      (w3m-form-goto-next-field))))

(defun w3m-open-dead-link-with-external-browser ()
  "Automatic open dead link."
  (interactive)
  (call-interactively 'w3m-process-stop)
  (if (search-forward-regexp "Reading " nil t)
      (browse-url-firefox (thing-at-point 'url))))

(defun w3m-emacswiki-view-diff ()
  "View a diff of the current EmacsWiki page."
  (interactive)
  (w3m-view-regexp-url
   "^\\(Last edited\\|Edited\\) [0-9]\\{4\\}\\(-[0-9]\\{2\\}\\)\\{2\\} [0-9]\\{2\\}:[0-9]\\{2\\} UTC by .*(diff)$"
   "different"))

(defun w3m-emacswiki-view-other-version ()
  "View other versions of the current EmacsWiki page."
  (interactive)
  (w3m-view-regexp-url
   "^Edit this page View other revisions"
   "other version"))

(defun w3m-view-regexp-url (regexp echo-string)
  "View regexp link in emacswiki.org"
  (let ((remember-pos (point)))
    (w3m-redisplay-this-page)
    (goto-char (point-min))
    (if (search-forward-regexp
         regexp
         nil t)
        (progn
          (backward-char)
          (w3m-view-this-url t) ; t -> force reload
          (message (format "Read %s with current wiki page." echo-string)))
      (goto-char remember-pos)
      (message (format "Don't %s in current wiki page." echo-string)))))

(defun w3m-emacswiki-recent-changes ()
  "View recent EmacsWiki changes using w3m."
  (interactive)
  (w3m-goto-url-new-session "http://www.emacswiki.org/emacs/?action=rc;days=7;all=1;showedit=1"))
  ;(w3m-goto-url-new-session "http://www.emacswiki.org/cgi-bin/wiki/RecentChanges" t))

(defun w3m-copy-link-in-region ()
  "Copy all link anchors in region into the kill ring."
  (interactive)
  (let* ((regionp (org-region-active-p))
         (transform-start (point-min))
         (transform-end (point-max))
         out-bound)
    (when regionp
      (setq transform-start (region-beginning))
      (setq transform-end (region-end))
      (if (fboundp 'deactivate-mark) (deactivate-mark)))
    (message "Copy links...")
    (save-excursion
      (goto-char transform-start)
      (while (and (not out-bound)
                  (not (w3m-no-next-link-p)))
        (w3m-get-next-link-start)
        (if (<= (point) transform-end)
            (if (w3m-anchor (point))
                (kill-new (w3m-anchor (point))))
          (setq out-bound t)
          ))
      (message "Copy links...done."))))

;;; FIXME de-duplicate these functions (use an argument to specify next/prev)
(defun w3m-get-anchor-start ()
  "Move cursor to the start of current anchor. Return point."
  (interactive)
  (goto-char (or (previous-single-property-change (point) 'w3m-anchor-sequence)
                 (point))))

(defun w3m-get-anchor-end ()
  "Move cursor to the end of current anchor. Return point."
  (interactive)
  (goto-char (or (next-single-property-change (point) 'w3m-anchor-sequence)
                 (point))))

(defun w3m-get-next-link-start ()
  "Move cursor to the start of next link. Return point."
  (interactive)
  (catch 'reach
    (while (next-single-property-change (point) 'w3m-anchor-sequence)
      (goto-char (next-single-property-change (point) 'w3m-anchor-sequence))
      (when (w3m-anchor (point))
        (throw 'reach nil))))
  (point))

(defun w3m-get-prev-link-start ()
  "Move cursor to the start of the previous link. Return point."
  (interactive)
  (catch 'reach
    (while (previous-single-property-change (point) 'w3m-anchor-sequence)
      (goto-char (previous-single-property-change (point) 'w3m-anchor-sequence))
      (when (w3m-anchor (point))
        (throw 'reach nil))))
  (point))

(defun w3m-no-next-link-p ()
  "Return t if there is no next link after cursor."
  (save-excursion
    (equal (point) (w3m-get-next-link-start))))

(defun w3m-no-prev-link-p ()
  "Return t if there is no previous link after cursor."
  (save-excursion
    (equal (point) (w3m-get-prev-link-start))))

(provide 'w3m-extension)

;;; LocalWords:  lnum utilties linknum unmark uniqure prettyfy logon login bw
;;; LocalWords:  postfix urbandictionary intitle inurl HaskellWiki's Anhonl txt
;;; LocalWords:  zh TW inlang tabset pos vscroll zerop UTC
