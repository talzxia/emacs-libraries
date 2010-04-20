;;; yaoddmuse.el --- yet another Oddmuse mode for Emacs

;; Author: Andy Stewart <lazycat.manatee@gmail.com>
;;         Štěpán Němec <stepnem@gmail.com>
;; Maintainer: Štěpán Němec <stepnem@gmail.com>
;; Copyright (C) 2009, Andy Stewart, all rights reserved.
;; Copyleft (Ↄ) 2010 Štěpán Němec, all rites reversed.
;; Created: 2009-01-06 12:41:17
;; Version: 1.0
;; Last-Updated: Tue Apr 20 15:02:58 2010 (+0200)
;;           By: Štěpán Němec
;; URL: http://github.com/stepnem/emacs-libraries/raw/priv/yaoddmuse.el
;; Original-URL: http://www.emacswiki.org/emacs/download/yaoddmuse.el
;; Keywords: oddmuse, wiki
;; Compatibility: GNU Emacs 22-23

;;; This file is NOT part of GNU Emacs

;;; License
;;
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

;;; Commentary:
;;
;; This mode can be used to edit or post wiki pages *asynchronously*,
;; so it won't hang your Emacs.
;; You can do other work while downloading or posting wiki pages.
;;

;;; Installation:
;;
;; Put yaoddmuse.el to your `load-path'.
;;
;; Add the following to your `user-init-file':
;;
;;      (require 'yaoddmuse)
;;
;; If your computer is always connected to Internet when Emacs starts up,
;; you can update the wiki page indexes automatically from your init file:
;;
;;      (yaoddmuse-update-pagenames t)
;;
;; This will avoid unnecessary delay when calling Yaoddmuse functions
;; for the first time in a session.
;;
;;

;;; Tips:
;;
;;   Following a link around point:
;;      The command `yaoddmuse-follow' tries to follow a page link around
;;      point. With a prefix argument, it asks you (using completion) for
;;      a page to edit.
;;
;;   Editing a page:
;;      When you use `yaoddmuse-edit' or `yaoddmuse-edit-default',
;;      it will prefer to just display the existing page if already downloaded.
;;      If you want to reload the page forcibly before editing, use a prefix
;;      argument.
;;
;;   Smart window/buffer handling:
;;      By default, the edit buffer will pop up when current major-mode
;;      is not `yaoddmuse-mode', or just switch to the buffer in current window
;;      when already in `yaoddmuse-mode'.
;;
;;   Reloading the edited page:
;;      `yaoddmuse-reload' reloads the current page.
;;
;;   Previewing the edited page in a browser:
;;      Use the `yaoddmuse-preview-current-buffer' command.
;;
;;   Viewing the page after successful posting:
;;      Use a prefix argument with the posting commands.
;;
;;   Posting buffers and files:
;;      `yaoddmuse-post-buffer' posts a buffer to the current wiki, to post the
;;      current buffer, use `yaoddmuse-post-current-buffer'.
;;      `yaoddmuse-post-file' posts a file to a wiki.
;;      `yaoddmuse-post-dired' posts files marked in Dired.
;;      `yaoddmuse-post-library' and `yaoddmuse-post-library-default' can be used
;;      to post Emacs Lisp library files.
;;
;;   Remember last summary:
;;      By default, the last edit summary is remembered; you can re-use it by
;;      hitting RET at the interactive prompt.
;;
;;   Picking up a file name at point:
;;      By default, when you use `yaoddmuse-post-library' and
;;      `yaoddmuse-post-library-default', those commands try to guess a filename
;;      around point.
;;
;;   Picking up a page name at point:
;;      When you use commands `yaoddmuse-browse-page' or
;;      `yaoddmuse-browse-page-default', it will try to guess a page name around
;;      point.
;;
;;   Encoding a special file:
;;      If you post a non-text file, such as a picture or a compressed archive,
;;      it will be encoded automatically before posting.
;;
;;   Redirecting a page:
;;      Use `yaoddmuse-redirect' to redirect a page.
;;
;;   Deleting a page:
;;      Use `yaoddmuse-delete' to delete a page.
;;
;;   Inserting a special file:
;;      You can use `yaoddmuse-insert-file-contents' insert
;;      file content.
;;      If it is a non-text file, such as a picture or a compressed archive,
;;      it will be encoded automatically before inserting.
;;
;;   Saving a page:
;;      You can use`yaoddmuse-save-as' to save page into a file.
;;      Special contents (such as picture or archive) will be decoded
;;      automatically and the correct filename extension added.
;;
;;   Toggling image view:
;;      By default, image pages are rendered automatically.
;;      You can use `yaoddmuse-toggle-image-status' to manually toggle
;;      rendered/raw view.
;;

;;; Change Log:
;;
;; 2010/
;;   Incompatible change:
;;      * `yaoddmuse-directory' now defaults to "~/.emacs.d/.yaoddmuse"
;;   Other changes:      
;;      * Preview functionality.
;;      * Font-lock corrections and improvements.
;;      * Use an alist and flat lists instead of a hash and nested
;;        lists for storing page lists.
;;      * Renamed identifiers:
;;        `yaoddmuse-display-page' -> `yaoddmuse-maybe-display-page'
;;        `yaoddmuse-get-coding' -> `yaoddmuse-get-wiki-coding'
;;        `yaoddmuse-get-page-buffer-name' -> `yaoddmuse-format-page-buffer-name'
;;        `yaoddmuse-get-pagename' -> `yaoddmuse-get-update'
;;        `yaoddmuse-get-pagename-callback' -> `yaoddmuse-get-update-callback'
;;        `yaoddmuse-get-pagename-table' -> `yaoddmuse-get-wiki-pagenames'
;;        `yaoddmuse-get-post-args' -> `yaoddmuse-get-wiki-post-args'
;;        `yaoddmuse-get-url' -> `yaoddmuse-get-wiki-url'
;;        `yaoddmuse-image-link-p' -> `yaoddmuse-image-link'
;;        `yaoddmuse-insert-file-content' -> `yaoddmuse-insert-file-contents'
;;        `yaoddmuse-lisp-file-link-p' -> `yaoddmuse-lisp-file-link'
;;        `yaoddmuse-pages-hash' -> `yaoddmuse-pages-alist'
;;        `yaoddmuse-retrieve-request' -> `yaoddmuse-set-request-parameters'
;;        `yaoddmuse-revert' -> `yaoddmuse-reload'
;;        `yaoddmuse-separate' -> `yaoddmuse-horizontal-rule'
;;        `yaoddmuse-url' -> `yaoddmuse-wikipage-url'
;;        `yaoddmuse-url-diff' -> `yaoddmuse-wikipage-diff-url'
;;        `yaoddmuse-wikiname-p' -> `yaoddmuse-wikiname'
;;      * Add cookies.
;;      * Delete useless comments and duplicate autogenerated content.
;;      * Improve documentation and comments, fix typos.
;;      * General cleanup, code fixes.

;;; Acknowledgements:
;;
;;      Alex Schroeder  <kensanata@gmail.com>

;;; Code:

(eval-when-compile (require 'cl))
(require 'sgml-mode)
(require 'skeleton)
(require 'url)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Customize ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defgroup yaoddmuse nil
  "Yet another Oddmuse mode for Emacs."
  :group 'edit)

(defcustom yaoddmuse-directory "~/.emacs.d/.yaoddmuse"
  "Directory to store Oddmuse pages."
  :type 'string
  :group 'yaoddmuse)

(defcustom yaoddmuse-assoc-mode t
  "Use `yaoddmuse-mode' for files inside `yaoddmuse-directory'.
If non-nil, all files under `yaoddmuse-directory' are
automatically recognized as YAOddmuse files.
Defaults to t."
  :type 'boolean
  :set (lambda (symbol value)
         (set symbol value)
         (if value
             (add-to-list
              'auto-mode-alist
              `(,(expand-file-name yaoddmuse-directory) . yaoddmuse-mode))
           ;; FIXME what a hack...(?) maybe the whole option should go?
           (remove-hook
            'auto-mode-alist
            `(,(expand-file-name yaoddmuse-directory) . yaoddmuse-mode))))
  :group 'yaoddmuse)

(defcustom yaoddmuse-wikis
  '(("TestWiki" "http://www.emacswiki.org/cgi-bin/test" utf-8 "question=1;")
    ("EmacsWiki" "http://www.emacswiki.org/cgi-bin/emacs" utf-8 "uihnscuskc=1;")
    ("CommunityWiki" "http://www.communitywiki.org/cw" utf-8 "uihnscuskc=1;")
    ("RatpoisonWiki" "http://ratpoison.antidesktop.net/wiki" utf-8 "uihnscuskc=1;")
    ("StumpwmWiki" "http://stumpwm.antidesktop.net/wiki" utf-8 "uihnscuskc=1;")
    ("OddmuseWiki" "http://www.oddmuse.org/cgi-bin/oddmuse" utf-8 "uihnscuskc=1;"))
  "Alist mapping wiki names to URLs.
First element of each sublist is the wiki name.
Second element is the URL.
Third element is encoding for the wiki.
Fourth element is the captcha string for editing protection.

You can add new sublists in format (WikiName WikiURL CodingSystem CaptchaString),
but you should not modify the default elements."
  :type '(repeat (list (string :tag "Wiki")
                       (string :tag "URL")
                       (symbol :tag "Coding System")
                       (string :tag "Captcha")))
  :group 'yaoddmuse)

(defcustom yaoddmuse-default-wiki "EmacsWiki"
  "The default wiki for editing.
The value must match first element of a sublist of `yaoddmuse-wikis'."
  :type 'string
  :group 'yaoddmuse)

(defcustom yaoddmuse-username user-full-name
  "Username to use when posting.
Signing your edits is the right thing to do."
  :type 'string
  :set (lambda (symbol value)
         (setq value (replace-regexp-in-string " " "" value))
         (set symbol value))
  :group 'yaoddmuse)

(defcustom yaoddmuse-password ""
  "Password to use when posting.
You only need this if you want to edit locked pages and you
know an administrator password."
  :type 'string
  :group 'yaoddmuse)

(defcustom yaoddmuse-transform-image t
  "Whether to render images automatically.
If non-nil, image content will be rendered automatically.
Otherwise, the source text will be left untransformed.
Default is t."
  :type 'boolean
  :group 'yaoddmuse)

(defcustom yaoddmuse-display-after-get t
  "If non-nil, display `yaoddmuse-mode' buffer after GET.
Default is t."
  :type 'boolean
  :group 'yaoddmuse)

(defcustom yaoddmuse-close-after-post nil
  "If non-nil, close `yaoddmuse-mode' buffer after POST.
Default is nil."
  :type 'boolean
  :group 'yaoddmuse)

(defcustom yaoddmuse-post-dired-confirm t
  "If non-nil, confirmation is needed to post files marked in Dired."
  :type 'boolean
  :group 'yaoddmuse)

(defcustom yaoddmuse-edit-protect t
  "If non-nil, automatically add captcha string when posting.
Some wikis, such as EmacsWiki, use a text captcha
to protect pages from malicious editing.
If you edit such a wiki, make sure this option is set."
  :type 'boolean
  :group 'yaoddmuse)

(defcustom yaoddmuse-use-always-minor nil
  "If non-nil, all edits are posted as minor.
You can change major/minor edit status anytime using
`yaoddmuse-toggle-minor'."
  :type 'boolean
  :group 'yaoddmuse)

(defcustom yaoddmuse-browse-function 'browse-url
  "The function used to browse pages.
Defaults to `browse-url'."
  :type 'function
  :group 'yaoddmuse)

(defcustom yaoddmuse-notify-function 'yaoddmuse-notify-default
  "Notify function for getting and posting.
It accepts one argument, the message string."
  :type 'function
  :group 'yaoddmuse)

(defcustom yaoddmuse-highlight-elisp-page t
  "If non-nil, use syntax highlighting for elisp code.
Defaults to t."
  :type 'boolean
  :group 'yaoddmuse)

(defcustom yaoddmuse-screenshot-program "import"
  "The default screenshot tool."
  :type 'string
  :group 'yaoddmuse)

(defcustom yaoddmuse-screenshot-filename "/tmp/yaoddmuse-screenshot.png"
  "Default filename for saving screenshots."
  :type 'string
  :group 'yaoddmuse)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Faces ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defface yaoddmuse-tag
  '((t (:foreground "Gold")))
  "Face used to highlight tags."
  :group 'yaoddmuse)

(defface yaoddmuse-link
  '((t (:foreground "Khaki")))
  "Face used to highlight links."
  :group 'yaoddmuse)

(defface yaoddmuse-url
  '((t (:foreground "Grey20")))
  "Face used to highlight URLs."
  :group 'yaoddmuse)

(defface yaoddmuse-url-name
  '((t (:foreground "Orange")))
  "Face used to highlight URL names."
  :group 'yaoddmuse)

(defface yaoddmuse-dialog
  '((t (:foreground "Peru")))
  "Face used to highlight dialogs."
  :group 'yaoddmuse)

(defface yaoddmuse-lisp-keyword
  '((t (:foreground "PaleGreen")))
  "Face used to highlight `Lisp:'."
  :group 'yaoddmuse)

(defface yaoddmuse-lisp-file
  '((t (:foreground "GreenYellow")))
  "Face used to highlight elisp code filenames."
  :group 'yaoddmuse)

(defface yaoddmuse-source-code
  '((t (:foreground "Yellow")))
  "Face used to highlight source code."
  :group 'yaoddmuse)

(defface yaoddmuse-image-link
  '((t (:foreground "DarkRed")))
  "Face used to highlight image links."
  :group 'yaoddmuse)

(defface yaoddmuse-image-link-name
  '((t (:foreground "Chocolate")))
  "Face used to highlight image-link names."
  :group 'yaoddmuse)

(defface yaoddmuse-heading
  '((t (:foreground "Green")))
  "Face used to highlight headings."
  :group 'yaoddmuse)

(defface yaoddmuse-tables
  '((t (:foreground "Aquamarine")))
  "Face used to highlight tables."
  :group 'yaoddmuse)

(defface yaoddmuse-indent
  '((t (:foreground "Tomato")))
  "Face used to highlight indent."
  :group 'yaoddmuse)

(defface yaoddmuse-bold
  '((default (:foreground "DodgerBlue"))
    (((type x)) (:weight bold)))
  "Face used to highlight bold text."
  :group 'yaoddmuse)

(defface yaoddmuse-underline
  '((default (:foreground "Purple"))
    (((type x)) (:underline t)))
  "Face used to highlight underlined text."
  :group 'yaoddmuse)

(defface yaoddmuse-italic
  '((default (:foreground "Brown"))
    (((type x)) (:slant italic)))
  "Face used to highlight italic text."
  :group 'yaoddmuse)

(defface yaoddmuse-symbol
  '((t (:foreground "Cyan")))
  "Face used to highlight symbols inside matching quotes like `this'."
  :group 'yaoddmuse)

(defface yaoddmuse-escaped
  '((t (:foreground "Grey")))
  "Face used to highlight escaped text."
  :group 'yaoddmuse)

(defface yaoddmuse-escaped-monospaced
  '((t (:foreground "DarkGrey")))
  "Face used to highlight escaped monospaced text."
  :group 'yaoddmuse)

(defface yaoddmuse-short-dash
  '((t (:foreground "Pink2")))
  "Face used to highlight short (en) dash."
  :group 'yaoddmuse)

(defface yaoddmuse-long-dash
  '((t (:foreground "LawnGreen")))
  "Face used to highlight long (em) dash."
  :group 'yaoddmuse)

(defface yaoddmuse-horizontal-rule
  '((t (:foreground "DarkRed")))
  "Face used to highlight horizontal rules."
  :group 'yaoddmuse)

(defface yaoddmuse-level-1
  '((t (:foreground "Grey100")))
  "Face used to highlight indent level 1."
  :group 'yaoddmuse)

(defface yaoddmuse-level-2
  '((t (:foreground "Grey70")))
  "Face used to highlight indent level 2."
  :group 'yaoddmuse)

(defface yaoddmuse-level-3
  '((t (:foreground "Grey40")))
  "Face used to highlight indent level 3."
  :group 'yaoddmuse)

(defface yaoddmuse-new-page
  '((t (:foreground "Red" :bold t)))
  "Face used to highlight the new page message."
  :group 'yaoddmuse)

(defface yaoddmuse-edit-status-face
  nil
  "Face used to highlight major/minor edit status."
  :group 'yaoddmuse)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Variables ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar yaoddmuse-wikiname nil
  "Name of the current wiki.
The value must match a first element of a sublist of `yaoddmuse-wikis'.")
(make-variable-buffer-local 'yaoddmuse-wikiname)

(defvar yaoddmuse-pagename nil
  "Page name of the current buffer.")
(make-variable-buffer-local 'yaoddmuse-pagename)

(defvar yaoddmuse-minor nil
  "If non-nil, this edit is a minor change.")
(make-variable-buffer-local 'yaoddmuse-minor)

(defvar yaoddmuse-retrieve-buffer nil
  "The download buffer used by `url-retrieve'.")
(make-variable-buffer-local 'yaoddmuse-retrieve-buffer)

(defvar yaoddmuse-image-status nil
  "Image status of the current page.
This status will be set when rendering image content.
Default is nil.")
(make-variable-buffer-local 'yaoddmuse-image-status)

(defvar yaoddmuse-pages-alist nil
  "Alist containing the wikiname / pagelist pairs.")

(defvar yaoddmuse-last-summary nil
  "The last edit summary.")

(defvar yaoddmuse-args-get
  "action=browse;raw=1;id=%t"
  "URL parameters to use for getting pages.

%t  URL-encoded pagename, e.g. HowTo, How_To, or How%20To")

(defvar yaoddmuse-args-index
  "action=index;raw=1"
  "URL parameters to use for index pages.")

(defvar yaoddmuse-args-post
  (concat "title=%t;"
          "summary=%s;"
          "username=%u;"
          "pwd=%p;"
          "recent_edit=%m;"
          "Preview=%P;"
          "text=%x")
  "URL parameters to use for publishing pages.

%t  pagename
%s  summary
%u  username
%p  password
%m  minor edit
%P  preview
%x  text")

(defvar yaoddmuse-post-mime-alist
  '((".css" . "text/css")
    (".xml" . "text/xml")
    (".tar" . "application/x-tar")
    (".tar.gz" . "application/x-gzip")
    (".gzip" . "application/x-gzip-compressed")
    (".zip" . "application/x-zip-compressed")
    (".jpeg" . "image/jpeg")
    (".png"  . "image/png"))
  "An alist of file extensions and corresponding MIME content-types.")

(defvar yaoddmuse-imenu-regexp "^\\(=+\\)\\s-*\\(.*?\\)\\s-*\\1"
 "A regular expression for headings to be added to the index menu.")

;; obviously, some of these bindings are not going to work in terminal, but I
;; currently don't consider it enough of a problem to provide alternatives
(defvar yaoddmuse-mode-map
  (let ((map (make-sparse-keymap)))
    ;; edit
    (define-key map (kbd "C-c C-e") 'yaoddmuse-edit-default)
    (define-key map (kbd "C-c C-S-e") 'yaoddmuse-edit)
    (define-key map (kbd "C-c C-o") 'yaoddmuse-follow)
    (define-key map (kbd "C-c C-t") 'sgml-tag)
    ;; preview
    (define-key map (kbd "C-c C-S-p") 'yaoddmuse-preview-current-buffer)
    ;; post
    (define-key map (kbd "C-c C-c") 'yaoddmuse-post-current-buffer)
    (define-key map (kbd "C-c C-S-c") 'yaoddmuse-post-buffer)
    (define-key map (kbd "C-c C-l") 'yaoddmuse-post-library-default)
    (define-key map (kbd "C-c C-S-l") 'yaoddmuse-post-library)
    (define-key map (kbd "C-c C-f") 'yaoddmuse-post-file)
    (define-key map (kbd "C-c C-S-f") 'yaoddmuse-post-file-default)
    (define-key map (kbd "C-c C-y") 'yaoddmuse-post-screenshot)
    (define-key map (kbd "C-c C-S-y") 'yaoddmuse-post-screenshot-default)
    ;; view
    (define-key map (kbd "C-c C-v") 'yaoddmuse-browse-page-default)
    (define-key map (kbd "C-c C-S-v") 'yaoddmuse-browse-page)
    (define-key map (kbd "C-c C-'") 'yaoddmuse-browse-page-default-diff)
    (define-key map (kbd "C-c C-S-'") 'yaoddmuse-browse-page-diff)
    (define-key map (kbd "C-c C-s") 'yaoddmuse-browse-current-page)
    (define-key map (kbd "C-c C-r") 'yaoddmuse-reload)
    ;; navigation
    (define-key map (kbd "C-c C-n") 'yaoddmuse-navi-next-heading)
    (define-key map (kbd "C-c C-p") 'yaoddmuse-navi-prev-heading)
    ;; update
    (define-key map (kbd "C-c C-j") 'yaoddmuse-update-pagenames)
    ;; insert
    (define-key map (kbd "C-c C-i") 'yaoddmuse-insert-pagename)
    (define-key map (kbd "C-c C-x") 'yaoddmuse-insert-file-contents)
    ;; misc
    (define-key map (kbd "C-c C-u") 'yaoddmuse-kill-url)
    (define-key map (kbd "C-c C-m") 'yaoddmuse-toggle-minor)
    (define-key map (kbd "C-c C-d") 'yaoddmuse-delete)
    (define-key map (kbd "C-c C-S-d") 'yaoddmuse-redirect)
    (define-key map (kbd "C-c C-S-t") 'yaoddmuse-toggle-image-status)
    (define-key map (kbd "C-c C-w") 'yaoddmuse-save-as)
    map)
  "Keymap used by `yaoddmuse-mode'.")

(defun yaoddmuse-highlight-keywords ()
  "Set up font lock highlighting for Yaoddmuse mode."
  (font-lock-add-keywords
   nil
   ;; figure out how's that with those newlines?
   '(("%%\\([^%\n]+\\)%%" . 'yaoddmuse-escaped)
     ("##\\([^#\n]+\\)##" . 'yaoddmuse-escaped-monospaced)
     ("\\`This page does not exist.*$" . 'yaoddmuse-new-page)
     ;; FIXME really only lowercase?
     ("<\\(/?[a-z]+\\)>" . 'yaoddmuse-tag)
     ("^\\(=\\{2,6\\}\\)\\([^=\n]+\\)\\1" 2 'yaoddmuse-heading)
     ("^\\[new:?[^][\n]*\\]$" . 'yaoddmuse-dialog)
     ("\\[\\[\\(image:\\)[^][\n]+\\]\\]" 1 'yaoddmuse-image-link)
     ("\\[\\[image:\\([^][\n]+\\)\\]\\]" 1 'yaoddmuse-image-link-name)
     ("\\[\\[\\([^][\n]+\\)\\]\\]" 1 'yaoddmuse-link)
     ("\\[\\([^][\n]+?\\)[[:blank:]]+[^][\n]+\\]" 1 'yaoddmuse-url)
     ("\\[[^][\n]+?[[:blank:]]+\\([^][\n]+\\)\\]" 1 'yaoddmuse-url-name)
     ("[^!]\\<[A-Z]+[a-z\x80-\xff]+[A-Z][A-Za-z\x80-\xff]*\\>" . 'yaoddmuse-link)
     ("\\b\\(Lisp:\\)[^[:space:]]+\\.el\\b" 1 'yaoddmuse-lisp-keyword)
     ("\\bLisp:\\([^[:space:]]+\\.el\\)\\b" 1 'yaoddmuse-lisp-file)
     ("^\\({{{\\|}}}\\|;;;\\)\\s-" 1 'yaoddmuse-source-code)
     ("|\\{2,\\}" . 'yaoddmuse-tables)
     ;; hm... spaces necessary?
     ("^\\(:+\\)\\s-" 1 'yaoddmuse-indent)
     ("\\s-\\(--\\)\\s-" . 'yaoddmuse-short-dash)
     ("\\s-\\(---\\)\\s-" . 'yaoddmuse-long-dash)
     ("^----$" . 'yaoddmuse-horizontal-rule)
     ("\\(?:[[:space:]]\\|^\\)`\\([^'[:space:]]+\\)'[^\w]" 1 'yaoddmuse-symbol)
     ("\\(?:[[:space:]]\\|^\\)''\\([^'\n]+\\)''[^\w]" 1 'yaoddmuse-italic)
     ("\\(?:[[:space:]]\\|^\\)'''\\([^'\n]+\\)'''[^\w]" 1 'yaoddmuse-bold)
     ("\\(?:[[:space:]]\\|^\\)\\(\\*\\{1,2\\}\\)\\([^\\*\n]+\\)\\1[^\w]" 2 'yaoddmuse-bold)
     ("\\(?:[[:space:]]\\|^\\)\\(/\\{1,2\\}\\)\\([^/\n]+\\)\\1[^\w]" 2 'yaoddmuse-italic)
     ("\\(?:[[:space:]]\\|^\\)\\(_\\{1,2\\}\\)\\([^_\n]+\\)\\1[^\w]" 2 'yaoddmuse-underline)
     ("^\\s-*\\([*#]\\)\\s-" 1 'yaoddmuse-level-1)
     ("^\\s-*\\([*#]\\{2\\}\\)\\s-" 1 'yaoddmuse-level-2)
     ("^\\s-*\\([*#]\\{3\\}\\)\\s-" 1 'yaoddmuse-level-3)
     ))
  (font-lock-mode 1))

(define-derived-mode yaoddmuse-mode text-mode "Yaoddmuse"
  "Yet another mode to edit Oddmuse wiki pages."
  (yaoddmuse-highlight-keywords)
  (set (make-local-variable 'sgml-tag-alist)
       `(("b") ("code") ("em") ("i") ("strong") ("nowiki")
         ("pre" \n) ("tt") ("u")))
  ;; FIXME Why this? Why not just let it be? Commenting out for now.
  ;(set (make-local-variable 'skeleton-transformation) 'identity)
  (setq imenu-generic-expression (list (list nil yaoddmuse-imenu-regexp 2)))
  (and buffer-file-name
       (setq yaoddmuse-wikiname
             (file-name-nondirectory
              (substring (file-name-directory buffer-file-name) 0 -1)))
       (setq yaoddmuse-pagename
             (file-name-nondirectory buffer-file-name))
       (setq yaoddmuse-minor
             yaoddmuse-use-always-minor))
  (when (and yaoddmuse-highlight-elisp-page
             (string-match "^.*\\.el$" yaoddmuse-pagename))
    (set-syntax-table emacs-lisp-mode-syntax-table)
    (setq major-mode 'emacs-lisp-mode)
    (lisp-mode-variables))
  (use-local-map yaoddmuse-mode-map)
  (goto-address)
  (setq indent-tabs-mode nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Interactive Functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Edit
;;;###autoload
(defun yaoddmuse-edit (wikiname pagename &optional arg)
  "Edit PAGENAME on WIKINAME.
WIKINAME is the name of the wiki as defined in `yaoddmuse-wikis',
Use a prefix argument to force a reload of the page."
  (interactive (list nil nil current-prefix-arg))
  (yaoddmuse-get-update wikiname pagename
                          (if arg
                              'yaoddmuse-handle-get
                            'yaoddmuse-handle-get-or-display)))

;;;###autoload
(defun yaoddmuse-edit-default (&optional arg)
  "Edit a page with default wiki `yaoddmuse-default-wiki'.
Use a PREFIX argument to force a reload of the page."
  (interactive "P")
  (yaoddmuse-edit yaoddmuse-default-wiki nil arg))

(defun yaoddmuse-follow (&optional arg)
  "Edit a page on the current wiki.
By default, this tries to pick the page name around point. With a
prefix argument, prompt the user instead."
  (interactive "P")
  (if arg
      (yaoddmuse-get-update yaoddmuse-wikiname nil 'yaoddmuse-handle-follow)
    (let ((page (yaoddmuse-pagename-at-point)))
      (if page (yaoddmuse-get-page yaoddmuse-wikiname page)
        (message "No valid link around point")))))

;;; Post
(defun yaoddmuse-post-buffer (post-buffer summary &optional arg)
  "Post POST-BUFFER to the current wiki, prompt the user when nil.
SUMMARY is the posting summary.
With a prefix argument, view the page after successful posting."
  (interactive (list (read-buffer "Buffer name: " nil t)
                     nil
                     current-prefix-arg))
  (set-buffer post-buffer)
  ;; FIXME: do we really want to save the file before posting?
  (when buffer-file-name
    (flet ((message (&rest args)))
      (basic-save-buffer)))
  (yaoddmuse-post yaoddmuse-wikiname
                  yaoddmuse-pagename
                  (buffer-string)
                  summary
                  arg))

(defun yaoddmuse-post-current-buffer (&optional arg)
  "Post the current buffer to the current wiki.
With a prefix argument, view the page after successful posting."
  (interactive "P")
  (yaoddmuse-post-buffer (current-buffer) nil arg))

(defun yaoddmuse-preview-current-buffer (&optional arg)
  "Preview the currently edited buffer."
  (interactive "P")
  (let ((yaoddmuse-preview "Preview"))
    (yaoddmuse-post-preview yaoddmuse-wikiname yaoddmuse-pagename
                            (buffer-string) nil)))

;;;###autoload
(defun yaoddmuse-post-file (&optional filename wikiname pagename summary arg)
  "Post a FILENAME to WIKINAME.
WIKINAME must be one of `yaoddmuse-wikis'
SUMMARY is the posting summary.
With a prefix argument, view the page after successful posting."
  (interactive (list (read-file-name "File name: ")
                     nil nil nil
                     current-prefix-arg))
  (if (and (file-exists-p filename)
           (not (file-directory-p filename)))
      (yaoddmuse-post wikiname
                      pagename
                      (yaoddmuse-encode-file filename)
                      summary
                      arg)
    (message "Invalid file name: %s" filename)))

;;;###autoload
(defun yaoddmuse-post-file-default (&optional arg)
  "Post a file to the default wiki.
With a prefix argument, view the page after successful posting."
  (interactive "P")
  (yaoddmuse-post-file nil yaoddmuse-default-wiki nil nil arg))

;;;###autoload
(defun yaoddmuse-post-library (&optional library wikiname pagename summary arg)
  "Post a library to a wiki.
Arguments not provided are prompted for interactively.
The current wiki is taken from `yaoddmuse-wikis'.
LIBRARY is the library name you want post.
WIKINAME is the wiki name for posting.
PAGENAME is the page name for posting.
SUMMARY is the posting summary.
With a prefix argument, view the page after successful posting."
  (interactive (list (yaoddmuse-get-library) nil nil nil current-prefix-arg))
  (let ((filename (find-library-name library)))
    (yaoddmuse-post-file filename wikiname pagename summary arg)))

;;;###autoload
(defun yaoddmuse-post-library-default (&optional arg)
  "Post a library file to the default wiki.
Use a prefix argument to view the page after successful posting."
  (interactive "P")
  (let* ((library (yaoddmuse-get-library))
         (filename (find-library-name library))
         (pagename (file-name-nondirectory filename)))
    (yaoddmuse-post-file filename yaoddmuse-default-wiki pagename nil arg)))

;;;###autoload
(defun yaoddmuse-post-dired (&optional wikiname summary arg)
  "Post files marked in Dired to the current wiki.
The current wiki is taken from `yaoddmuse-wikis'.
WIKINAME is wiki name for post.
SUMMARY is the posting summary.
With a prefix argument, view the page after successful posting.";FIXME which means...?
  (interactive (list nil nil current-prefix-arg))
  (if (eq major-mode 'dired-mode)
      (when (or (not yaoddmuse-post-dired-confirm)
                (y-or-n-p "Post marked files to wiki? "))
        (let (filename pagename)
          (or summary (setq summary (yaoddmuse-read-summary)))
          (dolist (file (dired-get-marked-files))
            (setq filename file)
            (setq pagename (file-name-nondirectory filename))
            (yaoddmuse-post-file filename wikiname pagename summary arg))))
    (message "You can only call this command from Dired")))

;;;###autoload
(defun yaoddmuse-post-dired-default (&optional arg)
  "Post dired marked files to default wiki.
With a prefix argument, view the page after successful posting.";FIXME which means...?
  (interactive "P")
  (yaoddmuse-post-dired yaoddmuse-default-wiki nil arg))

;;;###autoload
(defun yaoddmuse-post-screenshot (&optional wikiname summary arg)
  "Post screenshot to current wiki.
WIKINAME is wiki name for post.
SUMMARY is summary for post.
If PREFIX is non-nil, will view page after successful posting."
  (interactive (list nil nil current-prefix-arg))
  (if (executable-find yaoddmuse-screenshot-program)
      (progn
        ;; FIXME how is this applicable to other shooting programs?
        (message "Please use mouse select region for screenshot")
        (call-process yaoddmuse-screenshot-program nil nil nil yaoddmuse-screenshot-filename)
        (yaoddmuse-post-file yaoddmuse-screenshot-filename wikiname nil summary arg))
    (message "'%s' not-found. Install it or change `yaoddmuse-screenshot-program'" yaoddmuse-screenshot-program)))

;;;###autoload
(defun yaoddmuse-post-screenshot-default (&optional arg)
  "Post screenshot to default wiki.
Use a prefix argument to browse page after successful posting."
  (interactive "P")
  (yaoddmuse-post-screenshot yaoddmuse-default-wiki nil arg))

;;; View
(defun yaoddmuse-reload ()
  "Reload the currently edited page."
  (interactive)
  (yaoddmuse-get-page yaoddmuse-wikiname yaoddmuse-pagename))

;;;###autoload
(defun yaoddmuse-browse-page (&optional wikiname pagename)
  "Browse PAGENAME on WIKINAME.
WIKINAME is the name of the wiki as defined in `yaoddmuse-wikis',
PAGENAME is the pagename of the page you want to edit."
  (interactive)
  (yaoddmuse-get-update wikiname pagename 'yaoddmuse-handle-browse))

;;;###autoload
(defun yaoddmuse-browse-page-default ()
  "Browse a page on `yaoddmuse-default-wiki'."
  (interactive)
  (yaoddmuse-browse-page yaoddmuse-default-wiki))

;;;###autoload
(defun yaoddmuse-browse-page-diff (&optional wikiname pagename)
  "Browse diff of PAGENAME on WIKINAME.
WIKINAME is the name of the wiki as defined in `yaoddmuse-wikis',
PAGENAME is the pagename of the page you want to edit."
  (interactive)
  (yaoddmuse-get-update wikiname pagename 'yaoddmuse-handle-browse-diff))

;;;###autoload
(defun yaoddmuse-browse-page-default-diff ()
  "Browse diff of a page on `yaoddmuse-default-wiki'."
  (interactive)
  (yaoddmuse-browse-page-diff yaoddmuse-default-wiki))

(defun yaoddmuse-browse-current-page ()
  "Browse current page."
  (interactive)
  (yaoddmuse-browse-page yaoddmuse-wikiname yaoddmuse-pagename))

;;; Navigation
(defun yaoddmuse-navi-next-heading ()
  "Goto next heading."
  (interactive)
  (if (bolp)
      (forward-char +1))
  (unless (re-search-forward "^=+" nil t)
    (message "No more headings below point"))
  (move-beginning-of-line 1))

(defun yaoddmuse-navi-prev-heading ()
  "Goto previous heading."
  (interactive)
  (move-beginning-of-line 1)
  (unless (re-search-backward "^=+" nil t)
    (message "No more headings above point")))

;;; Misc
(defun yaoddmuse-insert-pagename (&optional pagename)
  "Insert name of a page on current wiki.
Interactively, prompt for PAGENAME with completion."
  (interactive)
  (yaoddmuse-get-update yaoddmuse-wikiname pagename 'yaoddmuse-handle-insert))

(defun yaoddmuse-insert-file-contents (file)
  "Insert the contents of FILE.
This function will encode special file contents, such as picture or archive."
  (interactive "fFile: ")
  (insert (yaoddmuse-encode-file file)))

(defun yaoddmuse-kill-url ()
  "Make URL of the current Oddmuse page the latest kill in the kill ring."
  (interactive)
  (let ((url (yaoddmuse-wikipage-url yaoddmuse-wikiname yaoddmuse-pagename)))
   (kill-new url)
   (message "'%s' copied into kill ring" url)))

(defun yaoddmuse-update-pagenames (&optional arg)
  "Update page indexes of all wikis in `yaoddmuse-wikis'.
By default, this function will run the update unconditionally.
With a prefix argument, update the page names only when not yet
downloaded."
  (interactive "P")
  (unless (and arg
               (consp yaoddmuse-pages-alist))
    (dolist (wiki yaoddmuse-wikis)
      (yaoddmuse-get-update (car wiki) nil nil t))))

(defun yaoddmuse-toggle-minor (&optional arg)
  "Toggle minor mode state.
If ARG is non-nil, always turn on."
  (interactive)
  (let ((num (prefix-numeric-value arg)))
    (cond
     ((or (not arg) (equal num 0))
      (setq yaoddmuse-minor (not yaoddmuse-minor)))
     ((> num 0) (set 'yaoddmuse-minor t))
     ((< num 0) (set 'yaoddmuse-minor nil)))
    (yaoddmuse-update-edit-status)
    yaoddmuse-minor))

(defun yaoddmuse-redirect ()
  "Redirect a page."
  (interactive)
  (yaoddmuse-get-update yaoddmuse-wikiname nil 'yaoddmuse-handle-redirect))

(defun yaoddmuse-delete ()
  "Delete a page."
  (interactive)
  (yaoddmuse-get-update yaoddmuse-wikiname nil 'yaoddmuse-handle-delete))

(defun yaoddmuse-toggle-image-status ()
  "Toggle image status.
If content is raw text format, transform it to image format.
If content is image format, transform it to raw text format."
  (interactive)
  (save-excursion
    (if (string-match "\\`#FILE image/\\(png\\|jpeg\\)$"
                      (buffer-substring-no-properties (goto-char (point-min))
                                                      (line-end-position)))
        (if yaoddmuse-image-status
            (yaoddmuse-turn-off-image-status)
          (yaoddmuse-turn-on-image-status))
      (message "Invalid image content"))))

(defun yaoddmuse-save-as ()
  "Save the current page as a file.
Try to decode special page content (such as picture or archive) before saving."
  (interactive)
  (save-excursion
    (let ((first-line (buffer-substring-no-properties
                        (goto-char (point-min))
                        (line-end-position)))
          (data (buffer-substring-no-properties
                 (point-min)
                 (point-max)))
          suffix)
      (when (string-match "\\`#FILE \\([^ \n]+\\)$" first-line)
        (setq data (yaoddmuse-decode-string data))
        (setq suffix (car (rassoc (match-string 1 first-line)
                                  yaoddmuse-post-mime-alist))))
      (with-temp-buffer
        (insert data)
        ;; FIXME
        (write-file (read-file-name (format "File: (Recommended suffix: %s) " suffix)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Utility Functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun yaoddmuse-get-update (&optional wikiname pagename handler-function forced)
  "Ensure that we have the list of pages for WIKINAME.
If so, continue by calling HANDLER-FUNCTION right away.
By default, page name list is not updated when downloaded already,
unless FORCED is non-nil.

WIKINAME is the name of the wiki as defined in `yaoddmuse-wikis',
PAGENAME is page name.
HANDLER-FUNCTION is the function to call after updating the page name list."
  (or wikiname (setq wikiname (yaoddmuse-read-wikiname)))
  ;; Create a storage directory.
  (make-directory (concat yaoddmuse-directory "/" wikiname) t)
  (if (and (yaoddmuse-get-wiki-pagenames wikiname)
           (not forced))
      (when (and handler-function (fboundp handler-function))
        (funcall handler-function wikiname pagename))
    (let* ((url (yaoddmuse-get-wiki-url wikiname))
           (coding (yaoddmuse-get-wiki-coding wikiname))
           (buffer (yaoddmuse-get-unique-buffer))
           (bufname (buffer-name buffer)))
      (yaoddmuse-set-request-parameters "GET")
      (setq url (yaoddmuse-format yaoddmuse-args-index coding url))
      (with-current-buffer buffer
        (setq yaoddmuse-retrieve-buffer
              (url-retrieve url
                            'yaoddmuse-get-update-callback
                            (list bufname coding
                                  wikiname pagename handler-function)))))))

(defun yaoddmuse-get-update-callback (status bufname coding wikiname pagename
                                               handler-function)
  "The callback function for `yaoddmuse-get-update'.
STATUS is the request status list passed to the function by `url-retrieve'.
BUFNAME is the name of temporary buffer for decoding downloaded content.
CODING is coding system for decoding.
WIKINAME is wiki name.
PAGENAME is page name.
HANDLER-FUNCTION is the function to call after updating the page name list."
  (let (pagelist)
    (yaoddmuse-retrieve-decode bufname coding)
    (with-current-buffer (get-buffer bufname)
      (setq pagelist (split-string (buffer-string)))
      (setq yaoddmuse-pages-alist (cons (cons wikiname pagelist) yaoddmuse-pages-alist))
      (kill-buffer nil))                ; argument needed in Emacs < 23
    (when (and handler-function (fboundp handler-function))
      (funcall handler-function wikiname pagename))))

(defun yaoddmuse-get-page (wikiname pagename)
  "Download PAGENAME from WIKINAME.
WIKINAME is the name of the wiki as defined in `yaoddmuse-wikis'.
PAGENAME is the name of the page you want to edit."
  (let* ((url (yaoddmuse-get-wiki-url wikiname))
         (coding (yaoddmuse-get-wiki-coding wikiname))
         (yaoddmuse-wikiname wikiname)
         (yaoddmuse-pagename pagename)
         (buffer (yaoddmuse-get-unique-buffer))
         (bufname (buffer-name buffer)))
    (yaoddmuse-set-request-parameters "GET")
    (setq url (yaoddmuse-format yaoddmuse-args-get coding url))
    (with-current-buffer buffer
      (setq yaoddmuse-retrieve-buffer
            (url-retrieve url
                          'yaoddmuse-get-page-callback
                          (list bufname coding wikiname pagename))))))

(defun yaoddmuse-get-page-callback (status bufname coding wikiname pagename)
  "The callback function for `yaoddmuse-get-page'.
STATUS is the request status list passed to the function by `url-retrieve'.
BUFNAME is the name of temporary buffer for decoding downloaded content.
CODING is coding system for decode.
WIKINAME is wiki name for post.
PAGENAME is page name for post."
  (if (eq (car status) :error)
      (with-current-buffer (get-buffer bufname)
        (funcall yaoddmuse-notify-function
                 (format "Getting page '%s' from '%s' failed" pagename wikiname))
        (kill-buffer bufname))
    ;; FIXME shouldn't this use some standard path-handling functions?
    (let ((buffer (find-file-noselect
                   (format "%s/%s/%s" yaoddmuse-directory wikiname pagename)))
          (pbufname (yaoddmuse-format-page-buffer-name wikiname pagename)))
      (yaoddmuse-retrieve-decode bufname coding)
      ;; Refresh page content.
      (set-buffer buffer)
      ;; FIXME why would these _ever_ be equal? (i.e. w:p vs p)
      (unless (equal pbufname (buffer-name))
        (rename-buffer pbufname))
      (erase-buffer)
      (insert
       (with-current-buffer (get-buffer bufname)
         (prog1
             (buffer-string)
           (kill-buffer nil))))         ; argument needed in Emacs < 23
      (funcall yaoddmuse-notify-function
               (format "Getting '%s' from '%s' successful" pagename wikiname))
      (yaoddmuse-mode)
      (yaoddmuse-update-edit-status)
      (yaoddmuse-page-content-adjust)
      (set-buffer-modified-p nil)
      (yaoddmuse-maybe-display-page pbufname))))

(defun yaoddmuse-page-content-adjust ()
  "Adjust page content."
  (goto-char (point-min))
  (save-excursion
    (cond
     ((looking-at "\\`This page does not exist") ;this ain't gonna happen; you get 404
      (erase-buffer)
      (insert "This page does not exist, you can create it now. :)"))
     ((and (looking-at "\\`#FILE image/\\(png\\|jpeg\\)$")
           yaoddmuse-transform-image)
      (yaoddmuse-turn-on-image-status)))))

(defun yaoddmuse-post (wikiname pagename post-string summary
                                &optional browse-page)
  "Post POST-STRING to PAGENAME on WIKINAME.
WIKINAME is the name of the wiki as defined in `yaoddmuse-wikis',
PAGENAME is the name of the page posted to.
POST-STRING is the string you want post.
SUMMARY is summary for post.
If BROWSE-PAGE is non-nil, browse the page after successful posting."
  (unless wikiname
    (setq wikiname (yaoddmuse-read-wikiname)))
  (unless pagename
    (setq pagename (yaoddmuse-read-pagename wikiname)))
  (unless summary
    (setq summary (yaoddmuse-read-summary)))
  (unless browse-page
    (setq browse-page current-prefix-arg))
  (let* ((url (yaoddmuse-get-wiki-url wikiname))
         (coding (yaoddmuse-get-wiki-coding wikiname))
         (yaoddmuse-minor (if yaoddmuse-minor "on" "off"))
         (yaoddmuse-wikiname wikiname)
         (yaoddmuse-pagename pagename)
         (text post-string))
    (yaoddmuse-set-request-parameters
     "POST"
     (yaoddmuse-format (yaoddmuse-get-wiki-post-args wikiname) coding))
    (url-retrieve url
                  'yaoddmuse-post-callback
                  (list wikiname pagename browse-page))))

(defun yaoddmuse-post-callback (&optional status wikiname pagename browse-page)
  "The callback function for `yaoddmuse-post'.
STATUS is a request status list passed to the function by `url-retrieve'.
WIKINAME is wiki name for post.
PAGENAME is page name for post.
If BROWSE-PAGE is non-nil, will browse page after successful posting."
  (if (eq (car status) :redirect)
      (let ((pagelist (yaoddmuse-get-wiki-pagenames wikiname)))
        (unless (member pagename pagelist)
          (setq pagelist (cons pagename pagelist))
          (setq yaoddmuse-pages-alist (cons (cons wikiname pagelist) yaoddmuse-pages-alist)))
        (when yaoddmuse-close-after-post
          (kill-buffer (yaoddmuse-format-page-buffer-name wikiname pagename)))
        (funcall yaoddmuse-notify-function
                 (format "Posting page '%s' to '%s' succeeded" pagename wikiname))
        (when browse-page
          (funcall yaoddmuse-browse-function
                   (yaoddmuse-wikipage-url wikiname pagename))))
    (funcall yaoddmuse-notify-function
             (format "Posting page '%s' to '%s' failed" pagename wikiname))))

(defun yaoddmuse-post-preview (wikiname pagename post-string summary)
  "Post POST-STRING to PAGENAME on WIKINAME.
WIKINAME is the name of the wiki as defined in `yaoddmuse-wikis',
PAGENAME is the name of the page posted to.
POST-STRING is the string you want post.
SUMMARY is summary for post."
  (unless wikiname
    (setq wikiname (yaoddmuse-read-wikiname)))
  (unless pagename
    (setq pagename (yaoddmuse-read-pagename wikiname)))
  (unless summary
    (setq summary (yaoddmuse-read-summary)))
  (let* ((url (yaoddmuse-get-wiki-url wikiname))
         (coding (yaoddmuse-get-wiki-coding wikiname))
         (yaoddmuse-minor (if yaoddmuse-minor "on" "off"))
         (yaoddmuse-wikiname wikiname)
         (yaoddmuse-pagename pagename)
         (text post-string))
    (yaoddmuse-set-request-parameters
     "POST"
     (yaoddmuse-format (yaoddmuse-get-wiki-post-args wikiname) coding))
    (url-retrieve url
                  'yaoddmuse-post-preview-callback
                  (list wikiname pagename))))

(defun yaoddmuse-post-preview-callback (status wikiname pagename)
  "The callback function for `yaoddmuse-post'.
STATUS is a request status list passed to the function by `url-retrieve'.
WIKINAME is wiki name for post.
PAGENAME is page name for post.
If BROWSE-PAGE is non-nil, will browse page after successful posting."
  (if (eq (car status) :error)
      (funcall yaoddmuse-notify-function
               (format "Previewing page '%s' to '%s' failed" pagename wikiname))
    (let ((buffer (current-buffer)))
      (switch-to-buffer buffer)
      (goto-char url-http-end-of-headers)
      (narrow-to-region (point) (point-max))
      (browse-url-of-buffer)
      (kill-buffer buffer))))

;;; FIXME -get and -follow are almost identical
(defun yaoddmuse-handle-get (wikiname &optional pagename)
  "Get PAGENAME on WIKINAME."
  (or pagename (setq pagename (yaoddmuse-read-pagename wikiname)))
  (yaoddmuse-get-page wikiname pagename))

(defun yaoddmuse-handle-get-or-display (wikiname &optional pagename)
  "Retrieve or display PAGENAME on WIKINAME.
The action executed depends on whether there already is a buffer
displaying the page."
  (or pagename (setq pagename (yaoddmuse-read-pagename wikiname)))
  (let ((bufname (yaoddmuse-format-page-buffer-name wikiname pagename)))
    (if (get-buffer bufname)
        (yaoddmuse-maybe-display-page bufname)
      (yaoddmuse-get-page wikiname pagename))))

(defun yaoddmuse-handle-follow (&optional wikiname pagename)
  "Get PAGENAME on WIKINAME."
  (or pagename (setq pagename (yaoddmuse-read-pagename wikiname "Edit page")))
  (yaoddmuse-get-page wikiname pagename))

(defun yaoddmuse-handle-browse (&optional wikiname pagename)
  "Browse PAGENAME on WIKINAME."
  (or pagename (setq pagename (yaoddmuse-read-pagename wikiname "Browse page")))
  (funcall yaoddmuse-browse-function (yaoddmuse-wikipage-url wikiname pagename)))

(defun yaoddmuse-handle-browse-diff (&optional wikiname pagename)
  "Browse a diff of PAGENAME on WIKINAME."
  (or pagename (setq pagename (yaoddmuse-read-pagename wikiname "Browse page diff")))
  (funcall yaoddmuse-browse-function (yaoddmuse-wikipage-diff-url wikiname pagename)))

(defun yaoddmuse-handle-insert (&optional wikiname pagename)
  "Insert PAGENAME on WIKINAME at point."
  (insert (or pagename
              (setq pagename
                    (yaoddmuse-read-pagename wikiname "Insert name of page")))))

;; FIXME PAGENAME unused
(defun yaoddmuse-handle-redirect (&optional wikiname pagename)
  "Redirect a page on WIKINAME."
  (let* ((pagelist (yaoddmuse-get-wiki-pagenames wikiname))
         (from-page (yaoddmuse-read-pagename wikiname "Redirect from page"))
         (to-page (yaoddmuse-read-pagename wikiname "Redirect to page"))
         (string (format "#REDIRECT [[%s]]" to-page))
         (summary (format "Redirect to %s" to-page)))
    (when yaoddmuse-pagename
      (erase-buffer)
      (insert string))
    (yaoddmuse-post wikiname
                    from-page
                    string
                    summary
                    current-prefix-arg)))

(defun yaoddmuse-handle-delete (&optional wikiname pagename)
  "Delete PAGENAME on WIKINAME."
  (or pagename (setq pagename (yaoddmuse-read-pagename wikiname "Delete page")))
  (yaoddmuse-post wikiname
                  pagename
                  "DeletedPage"
                  "Deleted"
                  current-prefix-arg))

(defun yaoddmuse-maybe-display-page (bufname)
  "Display a special page buffer.
BUFNAME is the buffer name of the displayed page."
  (when yaoddmuse-display-after-get
    (set-buffer (window-buffer))
    (if (eq major-mode 'yaoddmuse-mode)
        (switch-to-buffer bufname)
      (pop-to-buffer bufname))))

(defun yaoddmuse-read-wikiname (&optional prompt default)
  "Read wiki name interactively with completion."
  (completing-read (format (concat (or prompt "Wiki name")
                                     (and default " (%s)")
                                     ": ")
                             default)
                     yaoddmuse-wikis
                     nil t nil nil
                     default))

(defun yaoddmuse-read-pagename (wikiname &optional prompt default)
  "Read a pagename on WIKINAME interactively with completion.
Use \"Page name: \" as prompt, unless PROMPT is non-nil. Defaults
to the page name around point or DEFAULT, if specified."
  (let ((default (or default (yaoddmuse-pagename-at-point))))
    (completing-read (format (concat (or prompt "Page name")
                                     (and default " (%s)")
                                     ": ")
                             default)
                     (yaoddmuse-get-wiki-pagenames wikiname)
                     nil nil nil nil ;FIXME REQUIRE-MATCH nil?
                     default)))

(defun yaoddmuse-read-summary ()
  "Read the edit summary interactively."
  (setq yaoddmuse-last-summary
        (read-string (format (concat "Summary"
                                     (and yaoddmuse-last-summary " (%s)")
                                     ": ")
                             yaoddmuse-last-summary)
                     nil nil yaoddmuse-last-summary)))

(defun yaoddmuse-wikipage-url (wikiname pagename)
  "Return the URL of PAGENAME on WIKINAME."
  (condition-case nil
      (concat (or (yaoddmuse-get-wiki-url wikiname) (error)) "/" pagename)
    (error (format "Invalid wiki name: '%s'" wikiname))))

(defun yaoddmuse-wikipage-diff-url (wikiname pagename)
  "Return the diff URL of PAGENAME on WIKINAME."
  (condition-case nil
      (concat (or (yaoddmuse-get-wiki-url wikiname) (error)) "/?action=browse;diff=2;id=" pagename)
    (error (format "Invalid wiki name: '%s'" wikiname))))

(defun yaoddmuse-get-wiki-pagenames (wikiname)
  "Return the list of WIKINAME's pages from `yaoddmuse-pages-alist'."
  (assoc-default wikiname yaoddmuse-pages-alist))

(defun yaoddmuse-get-wiki-url (wikiname)
  "Return the WIKINAME URL from `yaoddmuse-wikis'."
  (cadr (assoc wikiname yaoddmuse-wikis)))

(defun yaoddmuse-get-wiki-coding (wikiname)
  "Return coding for WIKINAME from `yaoddmuse-wikis'."
  (caddr (assoc wikiname yaoddmuse-wikis)))

(defun yaoddmuse-get-wiki-post-args (wikiname)
  "Return HTTP POST args for WIKINAME for use in `yaoddmuse-format'."
  (if yaoddmuse-edit-protect
      (concat (cadddr (assoc wikiname yaoddmuse-wikis)) yaoddmuse-args-post)
    yaoddmuse-args-post))

(defun yaoddmuse-format-page-buffer-name (wikiname pagename)
  "Format a buffer name for PAGENAME on WIKINAME."
  (format "%s:%s" wikiname pagename))

(defun yaoddmuse-get-unique-buffer ()
  "Create and return a buffer for temporary storage of downloaded content.
Uses `current-time' to make buffer name unique."
  (let (time-now buffer)
    (setq time-now (current-time))
    (get-buffer-create
     (format " *%s<%s-%s-%s>*"
             "yaoddmuse"
             (nth 0 time-now) (nth 1 time-now) (nth 2 time-now)))))

(defun yaoddmuse-get-library ()
  "Prompt for Elisp library name with completion."
  (let* ((dirs load-path)
         (default (yaoddmuse-region-or-thing)))
    (completing-read (format (concat "Library name" (and default " (%s)") ": ")
                             default)
                     (yaoddmuse-get-library-list)
                     nil nil nil nil
                     default)))

(defun yaoddmuse-region-or-thing (&optional thing)
  "Return region or thing around point.
Return region, if active.
If THING is non-nil, return THING around point;
otherwise return the symbol around point."
  (if (and mark-active transient-mark-mode)
      (buffer-substring-no-properties (region-beginning)
                                      (region-end))
    (setq thing (or thing 'symbol))
    (ignore-errors
      (save-excursion
        (buffer-substring-no-properties (beginning-of-thing thing)
                                        (end-of-thing thing))))))

(defun yaoddmuse-get-library-list (&optional dirs string)
  "Return a list of library filename completions of STRING in DIRS.
Used in `yaoddmuse-get-library'.
DIRS should be a list of directories."
  (or dirs (setq dirs load-path))
  (or string (setq string ""))
  (let ((string-dir (file-name-directory string))
        name
        names)
    (dolist (dir dirs names)
      (unless dir
        (setq dir default-directory))
      (when string-dir
        (setq dir (expand-file-name string-dir dir)))
      (when (file-directory-p dir)
        (dolist (file (file-name-all-completions
                       (file-name-nondirectory string) dir))
          (setq name (if string-dir (concat string-dir file) file))
          (when (string-match
                 (format "^.*\\.el%s$" (regexp-opt load-file-rep-suffixes))
                 name)
            (add-to-list 'names name)))))))

(defun yaoddmuse-get-symbol-non-blank ()
  "Return the space-delimited \"word\" at point."
  (save-excursion
    (let (start end)
      (search-backward-regexp " \\|^" nil t)
      (skip-chars-forward " ")
      (setq start (point))
      (search-forward-regexp " \\|$" nil t)
      (skip-chars-backward " ")
      (setq end (point))
      (and start
           end
           (>= end start)
           (buffer-substring-no-properties start end)))))

(defun yaoddmuse-pagename-at-point ()
  "Return the page name at point."
  (let* ((word (word-at-point))
         (big-word (yaoddmuse-get-symbol-non-blank)))
    (cond
     ;; [[image:PAGENAME]]
     ((yaoddmuse-image-link big-word))
     ;; Lisp:PAGENAME.el
     ((yaoddmuse-lisp-file-link big-word))
     ((yaoddmuse-current-free-link-contents))
     ((yaoddmuse-wikiname word))
     (t yaoddmuse-pagename))))

(defun yaoddmuse-current-free-link-contents ()
  "If point is on a free link (\"[[foo bar]]\"), return its contents."
  (save-excursion
    (let* ((pos (point))
           (start (search-backward "[[" (line-beginning-position) t))
           (end (search-forward "]]" (line-end-position) t)))
      (and start end (>= end pos)
           (replace-regexp-in-string
            " " "_"
            (buffer-substring (+ start 2) (- end 2)))))))

(defun yaoddmuse-wikiname (str)
  "Return STR when it is a Wiki page name.
Otherwise return nil."
  (let (case-fold-search)
    (and str
         (string-match "[A-Z]+[a-z\x80-\xff]+[A-Z][A-Za-z\x80-\xff]*" str)
         str)))

(defun yaoddmuse-lisp-file-link (str)
  "If STR match Lisp:PAGENAME, return PAGENAME.
Otherwise return nil."
  (let (case-fold-search)
    (and str
         (string-match "Lisp:\\([^[:space:]]+\\.el\\)" str)
         (match-string 1 str))))

(defun yaoddmuse-image-link (str)
  "If STR matches [[image:PAGENAME]], return PAGENAME.
Otherwise return nil."
  (let (case-fold-search)
    (and str
         (string-match "\\[\\[image:\\([^][\n]+\\)\\]\\]" str)
         (match-string 1 str))))

(defun yaoddmuse-set-request-parameters (method &optional data)
  "Initialize parameters for the next request.
`url-request-method' is set to METHOD, `url-request-data' is set to DATA."
  (setq url-request-extra-headers
        (and (string= method "POST")
             '(("Content-type: application/x-www-form-urlencoded;"))))
  (setq url-request-method method)
  (setq url-request-data data))

(defun yaoddmuse-retrieve-decode (bufname coding)
  "Insert contents of `yaoddmuse-retrieve-buffer' into BUFNAME decoded using CODING."
  (declare (special url-http-end-of-headers))
  (with-current-buffer (get-buffer bufname)
    (insert
     (with-current-buffer yaoddmuse-retrieve-buffer
       (set-buffer-multibyte t)
       (goto-char (1+ url-http-end-of-headers))
       (decode-coding-region
        (point) (point-max)
        (coding-system-change-eol-conversion coding 'dos))
       (buffer-substring (point) (point-max))))
    (goto-char (point-min))))

(defvar yaoddmuse-preview ""
  "Internal variable holding the current value for the \"Preview\" request parameter.")

(defun yaoddmuse-format (args coding &optional url)
  "Format the HTTP request string.
Substitute format directives in ARGS according to variables set by the caller (`yaoddmuse-pagename',
`yaoddmuse-username', `yaoddmuse-minor', `yaoddmuse-password', `summary' and `text').
Every substituted value is url-encoded using CODING.
URL, if provided, is prepended to the resulting string (separated by \"?\")."
  (let (case-fold-search)
    (dolist (pair '(("%t" . yaoddmuse-pagename)
                    ("%u" . yaoddmuse-username)
                    ("%m" . yaoddmuse-minor)
                    ("%p" . yaoddmuse-password)
                    ("%s" . summary)
                    ("%P" . yaoddmuse-preview)
                    ("%x" . text)))
      (when (and (boundp (cdr pair)) (stringp (symbol-value (cdr pair))))
        (setq args
              (replace-regexp-in-string
               (car pair)
               (url-hexify-string
                (encode-coding-string (symbol-value (cdr pair))
                                      coding))
               args t t))))
    (if url
        (concat url "?" args)
      args)))

(defun yaoddmuse-notify-default (msg)
  "Default notify function for string MSG."
  (message "%s" msg))

(defun yaoddmuse-encode-file (file)
  "Encode FILE and return result.
Only encode file when its suffix matches `yaoddmuse-post-mime-alist'."
  (let* ((suffix (replace-regexp-in-string "^[^.]+" "" (file-name-nondirectory file)))
         (mime-type (cdr (assoc suffix yaoddmuse-post-mime-alist))))
    (if (or (string-equal suffix "")
            (not mime-type))
        (with-temp-buffer
          (insert-file-contents file)
          (buffer-string))
      (let ((coding-system-for-read 'binary)
            (coding-system-for-write 'binary)
            default-enable-multibyte-characters)
        (format "#FILE %s\n%s\n"
                mime-type
                (base64-encode-string
                 (with-temp-buffer
                   (insert-file-contents file)
                   (buffer-string))))))))

(defun yaoddmuse-decode-string (str)
  "Decode STR and return result."
  (with-temp-buffer
    (insert str)
    (string-make-unibyte
     (base64-decode-string
      (buffer-substring-no-properties
       (progn
         (goto-char (point-min))
         (forward-line +1)  ;skip MIME type information
         (point))
       (point-max))))))

(defun yaoddmuse-turn-on-image-status ()
  "Turn on image status.
Transform raw text format to image format."
  (save-excursion
    (if yaoddmuse-image-status
        (message "Already in image format")
      (let* ((data
              (yaoddmuse-decode-string
               (buffer-substring-no-properties
                (point-min)
                (point-max))))
             (image (create-image data nil t))
             (props
              `(display ,image
                        yank-handler
                        (image-file-yank-handler nil t)
                        intangible ,image
                        rear-nonsticky (display intangible))))
        (add-text-properties (point-min) (point-max) props)
        (setq yaoddmuse-image-status t)))))

(defun yaoddmuse-turn-off-image-status ()
  "Turn off image status.
Transform image format to raw text."
  (save-excursion
    (if yaoddmuse-image-status
        (let* ((data (buffer-substring-no-properties
                      (point-min)
                      (point-max))))
          (erase-buffer)
          (insert data)
          (setq yaoddmuse-image-status nil))
      (message "Already in raw text format"))))

(defun yaoddmuse-update-edit-status ()
  "Update edit status in the mode line.
If currently in major edit mode, display [Major].
Otherwise display [Minor]."
  (setq mode-name `("Yaoddmuse" ,(propertize
                                  (if yaoddmuse-minor
                                      (prog1
                                          " [Minor]"
                                        (message "Minor edit mode"))
                                    (prog1
                                        " [Major]"
                                      (message "Major edit mode")))
                                  'face 'yaoddmuse-edit-status-face)))
  (force-mode-line-update))

(provide 'yaoddmuse)

;;; yaoddmuse.el ends here
