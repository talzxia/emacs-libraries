;;; dotelib.el --- Elisp pain killer utility functions

;; Author: Štěpán Němec <stepnem@gmail.com>
;; Keywords: elisp, convenience
;; Licence: Whatever Works

;;; Commentary:

;; This is a minimal collection of utilities some of my released libraries
;; depend upon.

;; All identifiers defined here start with a dot, except for:

;; λ -- alias for `lambda'
;; $ -- alias for `funcall'
;; & -- alias for `apply-partially'
;; ∘ -- alias for `.compose'

;; Sorry for those, but making them two-char would kill half the fun.
;; Nonetheless, I acknowledge the slight sloppiness and am willing to prefix
;; them with a dot, too, upon request.

;; Corrections and constructive feedback appreciated.

;;; Code:

(eval-when-compile (require 'cl))
(require 'thingatpt)
(require 'url-util)
;;;_ . ELISP
(defmacro .aif (test then &rest else)
  "Anaphoric `if'. `.it' is bound to TEST result in the scope of THEN and ELSE."
  (declare (debug t) (indent 2))
  `(let ((.it ,test))
     (if .it ,then ,@else)))

;;;_ . FUNCTIONAL
(fset '$ 'funcall)     ; Haskell
;; J http://www.jsoftware.com/help/dictionary/d630n.htm
(fset '& 'apply-partially)
(fset '∘ '.compose)
(modify-syntax-entry ?∘ "_" emacs-lisp-mode-syntax-table)
(fset 'λ 'lambda)
;; (functionp #'(λ ...)) => nil anyway :-(
(setplist 'λ (append (symbol-plist 'λ) (symbol-plist 'lambda)))

(defun .compose (&rest fns)
  "Variadic function composition."
  (lexical-let ((fns fns))
    (if (cdr fns)
        (lambda (&rest args)
          (funcall (car fns) (apply (apply '.compose (cdr fns)) args)))
      (car fns))))

(defun .flip (f)
  "Return the dyadic function F with arguments reversed: (flip f a b = f b a)"
  (lexical-let ((f f))
    (lambda (x y) (funcall f y x))))

;;;_ . DATA STRUCTURES

;;;_  . SYMBOLS
(defsubst .format-symbol (&rest args)
  "Return the interned symbol named by applying `format' to ARGS."
  (intern (apply 'format args)))

(defmacro .with-made-symbols (syms &rest body)
  "Elisp equivalent of the familiar `with-gensyms' macro."
  (declare (debug (sexp body)) (indent 1))
  `(let ,(mapcar (lambda (s) `(,s (make-symbol ,(format "-*-%s-*-" s)))) syms)
     ,@body))

;;;_  . FILES
(defmacro .with-input-from-file (file &rest body)
  "Execute BODY with `standard-input' coming from FILE.
More precisely, the current buffer during evaluation of BODY is a
temporary buffer whose contents is the same as that of FILE, and
`standard-input' is bound to that buffer."
  (declare (debug t) (indent 1))
  `(with-temp-buffer
     (insert-file-contents-literally ,file)
     (let ((standard-input (current-buffer)))
       ,@body)))

(defmacro .with-output-to-file (file &rest body)
  "Evaluate BODY with `standard-output' going to FILE.
This is just to get rid of the `with-temp-file' misnomer; usually
`write-region' is just fine."
  (declare (debug t) (indent 1))
  `(with-temp-file ,file
     (let ((standard-output (current-buffer)))
       ,@body)))

(defmacro .with-open-file (file &rest body)
  "Evaluate BODY temporarily visiting FILE, then save FILE."
  (declare (debug t) (indent 1))
  `(with-current-buffer (find-file-noselect ,file t t)
     ,@body
     (basic-save-buffer-1)
     (kill-buffer nil)))

(defun .file-string (file)
  "Return contents of FILE as string."
  (.with-input-from-file file (buffer-string)))

(defun .replace-extension (fname ext)
  "Return FNAME with file extension replaced by EXT."
  (concat (file-name-sans-extension fname) ext))

;;;_  . BUFFERS
(defun .walk-buffers (fn &optional blist)
  "Call FN in all buffers in BLIST or (buffer-list).
Incidentally, B is bound to the current buffer at the time FN is
called."
  (dolist (b (or blist (buffer-list)))
    (with-current-buffer b (funcall fn))))

;;;_  . LISTS
(defun .partition (p l)
  "Return a cons containing elements of list L satisfying predicate P in its car and those that do not in its cdr."
  (let (y n)
    (dolist (e l (cons y n))
      (if (funcall p e) (push e y) (push e n)))))

;;;_  . STRUCTS
;;; Yeah, there's no way to get the :conc-name from the struct... :-|
(defmacro .update-struct (struct conc-name &rest specs)
  "Update STRUCT slots according to SPECS and return the updated STRUCT.

Example:

SPECS of the form (foo 8) (bar 42), with CONC-NAME `lulu.', will
set (lulu.foo STRUCT) and (lulu.bar STRUCT) to 8 and 42,
respectively."
  (declare (debug t) (indent 2))
  (.with-made-symbols (estruct)
    `(let ((,estruct ,struct))
       ,@(mapcar (lambda (s)
                   `(setf (,(.format-symbol "%s%s" conc-name (car s))
                           ,estruct)
                          ,(cadr s)))
                 specs)
       ,estruct)))

(defmacro .with-struct-accessors (specs struct conc-name &rest body)
  "Evaluate BODY with bindings for accessing STRUCT according to SPECS.

Example:

SPECS of the form (foo (bar baz)), with CONC-NAME `blaargh.', will
bind `foo' and `bar' in the extent of BODY for
accessing (blaargh.foo STRUCT) and (blaargh.baz STRUCT),
respectively."
  (declare (debug t) (indent 3))
  (.with-made-symbols (estruct)
    `(let ((,estruct ,struct))
       (symbol-macrolet
         ,(mapcar
           (lambda (s)
             `(,(if (consp s) (car s) s)
               (,(.format-symbol "%s%s" conc-name (if (consp s) (cadr s) s))
                ,estruct)))
           specs)
         ,@body))))

;;;_  . PROCESSES
(defun* .call-process-to-string-or-die (program &rest args)
  (with-temp-buffer
    (let ((ret (apply 'call-process program nil t nil args)))
      (if (and (numberp ret) (zerop ret))
          (buffer-string)
        (error "Process exited with status %s. Output: %s"
               ret (buffer-string))))))

;;;_  . STRINGS
(defsubst .non-empty-string (string)
  "Return STRING if not empty (\"\") or nil, otherwise return nil."
  (and (> (length string) 0) string))

(defun .string-inits (s)
  (let (r)
    (dotimes (i (length s) r)
      (push (substring s 0 (1+ i)) r))))

(defun .vim-syntax-keyword-debracket (s)
  (regexp-opt `(,@(if (string-match "\\(.+?\\)\\[\\(.+?\\)\\]" s)
                      (let ((prefix (match-string 1 s)))
                        (cons prefix
                              (mapcar (& 'concat prefix)
                                      (.string-inits (match-string 2 s)))))
                    (list s)))
              'words))

;;;_  . WINDOWS
(defun .get-mru-window (&optional all-frames avoid-selected)
  (let (best-window best-time time)
    (dolist (window (window-list);(window-list-1 nil nil all-frames)
                    )
      (setq time (window-use-time window))
      (unless (and avoid-selected
                   (eq window (selected-window)))
        (when (or (not best-time) (> time best-time))
          (setq best-time time)
          (setq best-window window))))
    best-window))

(defun .goto-mru-window ()
  (interactive)
  (select-window (.get-mru-window nil t)))

;;;_ . WWW
(defvar .url-regexp "\\<[a-zA-Z]+?://[^[:space:]\"<>]+\\>")

;; `mm-url' does (require 'gnus) ROFLMAO
;; cf. `mm-url-unreserved-chars' and `url-unreserved-chars' in url-util.el,
;; based on the obsolete RFC 2396 (cf. eg.
;; <https://secure.wikimedia.org/wikipedia/en/wiki/Percent-encoding>)
(defconst .url-unreserved-chars
  '(?a ?b ?c ?d ?e ?f ?g ?h ?i ?j ?k ?l ?m
    ?n ?o ?p ?q ?r ?s ?t ?u ?v ?w ?x ?y ?z
    ?A ?B ?C ?D ?E ?F ?G ?H ?I ?J ?K ?L ?M
    ?N ?O ?P ?Q ?R ?S ?T ?U ?V ?W ?X ?Y ?Z
    ?0 ?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9 ?- ?_ ?. ?~)
  "List of characters not reserved in the URL spec. Cf. RFC 3986.")

;; cf. eg. `mm-url-form-encode-xwfu'
(defun .urlencode (string &optional coding)
  (mapconcat
   (lambda (char)
     (cond ((= char ?\s) "+")
           ((memq char .url-unreserved-chars) (char-to-string char))
           (t (format "%%%02X" char))))
   (string-as-unibyte (encode-coding-string string (or coding 'utf-8)))
   ""))

(defvar xml-entity-alist)
(defun .xml-unescape-string (string)
  "Return the string with entity substitutions made from `xml-entity-alist'."
  (require 'xml)
  (with-temp-buffer
    (insert string)
    (mapc (lambda (e)
            (goto-char (point-min))
            (while (search-forward (concat "&" (car e) ";") nil t)
              (replace-match (cdr e))))
          xml-entity-alist)
    (buffer-string)))

(defun .url-query-string (&rest params)
  "PARAMS should be a list of query parameter specifications of
one of the following forms:

1. (KEY . VAL) -- strings specifying the parameter name and value

2. KEY -- symbol specifying the parameter name, value of which is
   stored in the symbol's value cell"
  (mapconcat (lambda (p)
               (let ((name (or (car-safe p) (symbol-name p)))
                     (val (or (cdr-safe p) (symbol-value p))))
                 (when val (mapconcat 'url-hexify-string `(,name ,val) "="))))
             params "&"))

(defun .url-retrieve-callback (fn)
  "Return a function suitable as the callback argument to `url-retrieve'.
The function signals any error occuring during the retrieval,
otherwise calls FN with any supplied arguments."
  (lexical-let ((fn fn))
    (lambda (status &rest cbargs)
      (.aif (plist-get status :error)
          (signal (car .it) (cdr .it))
        (apply fn cbargs)))))

(defun .url-retrieve-200-cb (fn)
  "Return a function suitable as the callback argument to `url-retrieve'.
The function signals any error occuring during the retrieval or
in case of non 200 HTTP status, otherwise calls FN with any
supplied arguments and point positioned after the response
headers."
  (lexical-let ((fn fn))
    (.url-retrieve-callback
     (lambda (&rest cbargs)
       (goto-char (point-min))
       (let ((status (when (looking-at "HTTP/.+ \\([0-9]+\\) ")
                       (match-string 1))))
         (if (string= status "200")
             (progn (search-forward "\n\n") (apply fn cbargs))
           (error "HTTP status %s (%S)" status (match-string 0))))))))

;;;_ . SEARCH & REPLACE
(defun .collect-matches (regexp &optional group from-beginning buffer)
  "Return a list of all matches for REGEXP in BUFFER following point.
GROUP (an integer) specifies which of the paren group matches to
collect. When FROM-BEGINNING is non-nil, search from the
beginning of BUFFER instead of from point."
  (let (res)
    (with-current-buffer (or buffer (current-buffer))
      (save-excursion
        (when from-beginning (goto-char (point-min)))
        (while (search-forward-regexp regexp nil t)
          (push (match-string-no-properties (or group 0)) res))))
    res))

(defun .match-nearest-point (regexp &optional skippees walk-the-line)
  "Return the match for REGEXP nearest point.
If REGEXP contains parenthesis group, first group match is
returned. When WALK-THE-LINE is non-nil, prefer the match on the
current line, even when the match in the opposite direction is
nearer. SKIPPEES can specify an argument to `skip-syntax-forward'
to first skip out of a potential match to work around the
brain-dead Emacs \"backward search\" implementation."
  (save-excursion
    (when skippees (skip-syntax-forward skippees))
    (let* ((pt (point))
           (bol (line-beginning-position))
           (eol (line-end-position))
           (before (when (re-search-backward regexp nil t)
                     (or (match-string-no-properties 1)
                         (match-string-no-properties 0))))
           (before-pos (point))
           (after (when (progn (goto-char pt)
                               (re-search-forward regexp nil t))
                    (or (match-string-no-properties 1)
                        (match-string-no-properties 0))))
           (after-pos (point)))
      (or (and before after
               (if (< (- pt before-pos) (- after-pos pt))
                   (if (and walk-the-line
                            (< before-pos bol)
                            (<= after-pos eol))
                       after
                     before)
                 (if (and walk-the-line
                          (> after-pos eol)
                          (>= before-pos bol))
                     before
                   after)))
          before
          after))))

;; note there's `delete-and-extract-region' in editfns.c
(defun .snap-region (beg end)
  "Delete region between BEG and END and return it as a string.
Text properties are stripped."
  (prog1 (buffer-substring-no-properties beg end)
    (delete-region beg end)))

(defun .snap-thing (thing)
  "Delete THING and return it as a string."
  (let ((bounds (bounds-of-thing-at-point thing)))
    (when bounds (.snap-region (car bounds) (cdr bounds)))))

(defmacro .region-or (form)
  "Return the region if active, FORM otherwise.
If FORM evaluates to a symbol, the thing of that name is returned
as per `thing-at-point'; otherwise the evaluation result is
returned."
  (declare (debug t) (indent nil))
  `(if (region-active-p)
       (buffer-substring-no-properties (region-beginning) (region-end))
     (let ((res (eval ',form)))
       (if (symbolp res)
           (thing-at-point res)
         res))))

;;;_ . EDITING
;; Refactored from `copy-from-above-command' from misc.el
(defun .copy-from-below (&optional n)
  "Copy characters from next non-blank line, starting just below point.
Copy N characters, but not past the end of that line.
The characters copied are inserted in the buffer before point."
  (interactive "p")
  (.copy-from-above n t))

(defun .copy-from-above (&optional n below)
  "Copy characters from previous non-blank line, starting just above point.
Copy N characters, but not past the end of that line.
The characters copied are inserted in the buffer before point."
  (interactive "p")
  (let ((cc (current-column)) string)
    (insert
     (save-excursion
       (if below (forward-line) (beginning-of-line))
       (funcall (if below 'skip-chars-forward 'skip-chars-backward) " \t\n")
       (move-to-column cc)
       ;; If current column winds up in the middle of a tab, copy appropriate
       ;; number of spaces.
       (when (< cc (current-column))
         (if (= (preceding-char) ?\t)
             (let ((len (min n (- (current-column) cc))))
               (setq string (make-string len ?\s)
                     n (- n len)))
           ;; In middle of ctl char => copy that whole char.
           (forward-char -1)))
       (concat string
               (buffer-substring
                (point) (min (line-end-position) (+ n (point)))))))))

(defun .invert-case-region (beg end)
  (interactive "r")
  (let ((s (buffer-substring beg end)))
    (save-excursion
      (goto-char beg)
      (delete-region beg end)
      (insert (mapconcat
               (lambda (c)
                 (let ((upper (upcase c)))
                   (char-to-string (if (eq upper c) (downcase c) upper))))
               s "")))))

;;;_ . INTERACTIVE
;; Work around `read-event' and friends returning -1 when
;; `executing-kbd-macro' is non-nil. Cf. similar workarounds e.g. in
;; `read-char-choice' (or previously `dired-query').
(defmacro .with-executing-kbd-macro-nil (&rest body)
  (declare (debug t) (indent 0))
  `(let ((executing-kbd-macro executing-kbd-macro))
     (when executing-kbd-macro (setq executing-kbd-macro nil))
     ,@body))

;;;_  . KEYBINDINGS
;; FIXME would be nice to have some location (i.e. file/line number/form)
;; information, too
(defun .define-keys (keymap keys)
  (mapc (apply-partially 'apply 'define-key keymap) keys)
  keymap)

(defmacro .defprefix (name keys &optional doc menu-name)
  (declare (debug t) (indent 1))
  (let ((mapsym (intern (replace-regexp-in-string "-prefix\\'" "-map"
                                                  (symbol-name name)))))
    `(progn
       ,(when doc `(defvar ,mapsym nil ,doc))
       (define-prefix-command ',name ',mapsym ,menu-name)
       (.define-keys ,mapsym ,keys)
       ',name)))

(defmacro .defkeymap (name keys &optional doc)
  (declare (debug t) (indent 1))
  `(progn
     (defvar ,name (make-sparse-keymap) ,doc)
     (.define-keys ,name ,keys)
     ',name))

;;;_  . MINIBUFFER
(defun .complete-with-default (prompt-prefix
                               collection &optional hist default require-match
                               predicate inherit-input-method initial-input)
  (let ((def (or default
                 (when hist (car (symbol-value hist))))))
    (completing-read (.prompt-with-default def prompt-prefix)
                     collection predicate require-match initial-input hist
                     def inherit-input-method)))

(defun .read-string-with-default (prompt-prefix
                                  &optional hist default inherit-input-method
                                            initial-input)
  (let ((def (or default (car (symbol-value hist)))))
    (read-string (.prompt-with-default def prompt-prefix)
                 initial-input hist def inherit-input-method)))

(defun .prompt-with-default (default prefix &optional deffmt suffix)
  "Return a prompt suitable for functions like `read-string'.
The outcome depends on whether DEFAULT is actually non-nil. This
saves you from typing boiler-plate all the time when using the
braindead Emacs minibuffer-reading functions. This is more of a
lower-level function, see `.read-string-with-default' and friends
which use it."
  (concat prefix
          (and default (format (or deffmt " (%s)")
                               ;; propertised text doesn't display in the
                               ;; prompt; noticed with 24.4
                               (substring-no-properties
                                (or (car-safe default) default))))
          (or suffix ": ")))

;;;_  . VARIABLES
;; caveat: C-M-x reevaluation doesn't work the same as with `defvar'
(defmacro .deflocalvar (name value &optional doc permanent)
  (declare (debug (symbolp form &optional stringp form)) (indent 1))
  `(progn
     (defvar ,name ,value ,doc)
     (make-variable-buffer-local ',name)
     ,(when permanent `(put ',name 'permanent-local t))
     ',name))

(defmacro .setq-local (&rest specs)
  "Make each VAR local in the current buffer and set it to VAL.
Each VAR should be the variable symbol and VAL evaluate to its
value.

\(fn [VAR VAL]...)"
  (declare (debug t) (indent 0))
  `(progn
     ,@(let (r)
         (while specs
           (push `(set (make-local-variable ',(pop specs)) ,(pop specs))
                 r))
         r)))

(provide 'dotelib)
;;; dotelib.el ends here

;;;_ . PARAPHERNALIA
(font-lock-add-keywords
 'emacs-lisp-mode
 `((,(concat "(\\("
             (regexp-opt '(".aif" ".setq-local" ".update-struct"
                           ".with-executing-kbd-macro-nil" ".with-made-symbols"
                           ".with-input-from-file" ".with-open-file"
                           ".with-output-to-file" ".with-struct-accessors"
                           "λ"))
             "\\)\\>")
     . 1)))

(font-lock-add-keywords
 'emacs-lisp-mode
 `((,(concat "(\\("
             (regexp-opt '(".defkeymap" ".deflocalvar" ".defprefix"))
             "\\)\\s-+\\(\\sw+\\)")
     (1 font-lock-keyword-face)
     (2 font-lock-variable-name-face))))

;;;_. fileLocalVariables
;; Local Variables:
;; allout-layout: (* 0 :)
;; End:
