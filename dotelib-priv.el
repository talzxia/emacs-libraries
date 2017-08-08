;;;_. DOTELIB a.k.a. "UTILITY    -*- lexical-binding: t -*-

;; All identifiers defined here start with a dot, except for:

;; `kill-this-buffer' -- trivial replacement of the built-in definition
;; λ -- alias for `lambda'
;; $ -- alias for `funcall'
;; & -- alias for `apply-partially'
;; ∘ -- alias for `.compose'

(require 'cl-lib)
(require 'thingatpt)
(require 'url-util)
(require 'dotelib)

;;;_ . ELISP
;;;_  . ADVICE
(autoload 'ad-disable-advice "advice")
(autoload 'ad-read-advice-specification "advice")
(autoload 'ad-update "advice")
(defun .advice-disable (function class name)
  "`ad-disable-advice' + `ad-activate'"
  (interactive (ad-read-advice-specification "Disable advice of"))
  (ad-disable-advice function class name)
  (ad-update function))

;;;_  . AUTOLOAD
(defvar generated-autoload-file)
(defun .update-file-autoloads (&optional arg)
  "Call `update-file-autoloads' and reload `generated-autoload-file'.
With a prefix arg, only do the former."
  (interactive "P")
  (call-interactively 'update-file-autoloads)
  (unless arg (load generated-autoload-file)))

;;;_  . COMPILE
(defun .byte-compile-external (&optional cleanup)
  (interactive "P")
  (let ((compile-command
         (concat "emacs -batch -eval "
                 (shell-quote-argument
                  (prin1-to-string `(setq load-path ',load-path)))
                 " -f batch-byte-compile " buffer-file-name)))
    ;; nice crap shot, huh? :-)))
    (when cleanup
      (run-at-time 10 nil '.remove-elc buffer-file-name))
    (call-interactively 'compile)))

(defun .remove-elc (&optional filename)
  "Remove the byte-compiled version of FILENAME (visited file by default)."
  (interactive "fRemove .elc for: ")
  (let ((cf (byte-compile-dest-file (or filename buffer-file-name))))
    (when (file-exists-p cf)
      (delete-file cf)
      (message "Removed %s" cf))))

(defun .remove-elc-on-save (&optional recompile)
  "Removing the byte-compiled version of the file being visited at each save.
With a prefix argument or RECOMPILE non-nil, also recompile the
file. With a double C-u or (equal RECOMPILE '(16)), load the file
after recompiling.

Works by adding a function to `after-save-hook'."
  (interactive "P")
  (add-hook
   'after-save-hook
   `(lambda ()
      (.remove-elc)
      ,(when recompile
         `(byte-compile-file buffer-file-name ,(equal recompile '(16)))))
   nil t))

;;;_  . EVAL
;; more useful version of `eval-print-last-sexp'
(defun .eval-print-last-sexp (&optional arg)
  "With a prefix argument, call `pp-eval-last-sexp'.
Without argument same as `eval-print-last-sexp'"
  (interactive "P")
  (if arg (pp-eval-last-sexp t) (eval-print-last-sexp)))

(defun .test-eval (&optional insertp)
  "Evaluate the form following point, prompting for any unbound symbols' values.
Useful for testing stuff."
  (interactive "P")
  (let ((form (save-excursion (read (current-buffer)))) vars)
    (.mapc-tree (λ (s)
                  (and (symbolp s)
                       (not (boundp s))  ; obviously not really correct,
                       (not (fboundp s)) ; but usually good enough
                       (cl-pushnew s vars :test #'eq)))
                form)
    (eval-expression
     `(let ,(mapcar (λ (s) (list s (read-from-minibuffer
                                    (format "Value for `%s': " s)
                                    nil nil t)))
             vars)
        ,form)
     insertp)))

;;;_  . LOAD
(defun .unload-regexp (regexp &optional force)
  "Unload all `features' matching REGEXP.
Returns the list of features unloaded."
  (interactive "sRegexp: \nP")
  (let ((uf (cl-loop for f in features
                  if (string-match regexp (symbol-name f))
                  do (unload-feature f force) and collect f)))
    (message "%s unloaded" (mapconcat 'symbol-name uf ", "))
    uf))

;;;_  . PROFILING
(defmacro .time (&rest body)
  "Run GC, evaluate BODY and print the number of seconds the latter took."
  (declare (debug t) (indent progn))
  (let ((t1 (make-symbol "t1")))
    `(progn (garbage-collect)
            (let ((,t1 (float-time)))
              (unwind-protect (progn ,@body)
                (print (- (float-time) ,t1)))))))

;;;_  . REFACTORING
(autoload 'ad-arglist "advice")
(autoload 'find-function-read "find-func")
(defun .insert-arglist (&optional name)
  "Insert arglist of function NAME at point."
  (interactive (find-function-read))
  (when name (insert (format "%S" (ad-arglist (symbol-function name))))))

(defun .insert-declare-function (&optional name)
  "Insert proper `declare-function' form for NAME at point."
  (interactive (find-function-read))
  ;; (let ((def (function-called-at-point)))
  ;;   (list (completing-read (.prompt-with-default def "Function")
  ;;                          obarray 'fboundp))
  ;;   )
  ;; there's `help-function-arglist' and `ad-arglist'; the former
  ;; returns `t' for C functions; blech
  (when name
    (insert (format "(declare-function %s %S %S)"
                    name
                    (file-name-nondirectory (symbol-file name 'defun))
                    (ad-arglist (symbol-function name))))))

(defun .dotelib-compat ()
  (interactive)
  (let* ((dotebuf (find-file-noselect (locate-library "dotelib.el" t)))
         used)
    (let ((dots (delete-dups
                 (.collect-matches
                  "[`'( \t]\\(\\.\\(?:\\s_\\|\\sw\\)+\\)\\_>" 1 t)))
          ;; could also just use `load-history'
          (defs (with-current-buffer dotebuf (.definitions))))
      (mapc (λ (d) (when (member d defs) (push d used))) dots))
    (when used
      (goto-char (point-min))
      (re-search-forward "^;;; Code:\n\n")
      (let ((start ";;;; Autoinserted dotelib definitions\n")
            (end ";;;; End of autoinserted dotelib definitions\n")
            (forms (concat "(eval-and-compile\n"
                           "  (unless (require 'dotelib nil t)\n"
                           (with-current-buffer dotebuf
                             (mapconcat (& (.flip '.snarf-definition) t)
                                        used "\n"))
                           "))")))
        (when (search-forward start nil t)
          (delete-region (match-beginning 0) (search-forward end)))
        (insert start forms ?\n end)))))

(defvar .definition-start
  "\\(?:^ *\\|[^'\\]\\)(def\\(?:un\\|var\\|macro\\|face\\|struct\\) +")
(defun .snarf-definition (symstr &optional string)
  (goto-char (point-min))
  (re-search-forward (concat .definition-start (regexp-quote symstr) "[\n ]"))
  (goto-char (match-beginning 0))
  (if string
      (buffer-substring-no-properties (point) (progn (forward-sexp) (point)))
    (read (current-buffer))))

(defun .definitions ()
  (.collect-matches (concat .definition-start "\\([^,'][^()\n]*?\\)[\n ]")
                    1 t))

(defun .unused-definitions ()
  (interactive)
  (save-excursion
    (cl-delete-if
     (lambda (s)
       (or (null s)
           (interactive-form (intern s))))
     (mapcar
      (lambda (d)
        (goto-char (point-min))
        (unless (search-forward-regexp
                 (concat "\\_<" (regexp-quote d) "\\_>") nil t 2)
          d))
      (.definitions)))))

;;;_ . DATA STRUCTURES

(defun .keymapp (object)
  (or (keymapp object)
      (and (symbolp object)
           (boundp object)
           (keymapp (symbol-value object)))))

;;;_  . GENERIC

(defun .ypath (type object path)
  "Extract value stored at PATH in OBJECT of type TYPE.
Example: (.ypath 'plistk '(:a (:b (:c (:d 8)))) \"a/b/c/d\") ;=> 8
Recognised types include `alist', `alistq', `hash', `plist' and
`plistk'."
  (let* ((getter (cl-case type
                   (alist 'assoc-default)
                   (alistq (∘ 'cdr 'assq))
                   (hash 'gethash)
                   ((plist plistk) 'plist-get)))
         (steps1 (split-string path "/"))
         (steps (cl-case type
                  ((alist hash) steps1)
                  ((alistq plist) (mapcar 'intern steps1))
                  (plistk (mapcar (∘ 'intern (& 'concat ":")) steps1)))))
    (while steps
      (setq object (funcall getter object (car steps))
            steps (cdr steps)))
    object))

;;;_  . SYMBOLS
(defun .unintern-regexp (regexp &optional obary)
  (interactive
   (list (.read-string-with-default "Regexp" nil (thing-at-point 'symbol))))
  (unless obary (setq obary obarray))
  (mapatoms (lambda (s) (and (string-match regexp (symbol-name s))
                             (unintern s obary)))
            obary))

;;;_  . FILES
(defmacro .with-ephemeral-files (specs &rest body)
  "Bind variables to files according to SPECS, evaluate BODY and return contents of the files.
SPECS is of the form ((VAR FILESPEC) ...), where FILESPEC can
evaluate to a relative file name, in which case it is passed to
`make-temp-file', or something different, in which case it is
left alone and had better be a writable file name.

Deletes the created files afterwards.

Useful when using poorly designed packages with
output-to-file-only functions, not uncommon with Emacs (see the
Org table export code for instance; be sure to have a bucket
ready)."
  (declare (debug ((&rest (symbolp form)) body)) (indent 1))
  (.with-made-symbols (files file)
    `(let (,files)
       (let ,(mapcar
              (lambda (spec)
                `(,(car spec)
                  (let ((,file (let ((fname ,(cadr spec)))
                                 (if (and (stringp fname)
                                          (not (file-name-absolute-p fname)))
                                     (make-temp-file fname)
                                   fname))))
                    (push ,file ,files)
                    ,file)))
              specs)
         (unwind-protect
             (progn ,@body
                    (let ((ret (mapcar '.file-string ,files)))
                      (if (cdr ret) ret (car ret))))
           (condition-case e (mapc 'delete-file ,files)
             (file-error (warn ".with-ephemeral-files: File error\
 encountered while removing files: %S" (cdr e)))))))))

(defun .fifo-printer (name)
  (let ((name name))
    (lambda (string)
      (start-process-shell-command
       (concat name "writer") nil (concat "echo " string  " > " name)))))
;; ($ (.fifo-printer "~/testfifo") "ahoj")
(defun .region-file-names (beg end &optional ignore-regexp careful)
  "Return list of file names on lines between BEG and END in the current buffer.
When CAREFUL is set, parse file names using \"(thing-at-point 'filename)\",
otherwise just use the whole line.
Ignore lines starting with IGNORE-REGEXP."
  (let ((predicate (if ignore-regexp
                       (lambda () (when ignore-regexp
                                    (looking-at ignore-regexp)))
                     (lambda nil)))
        list)
    (save-excursion
      (goto-char beg)
      (while (< (point) end)
        (unless (funcall predicate)
          (let ((fname (thing-at-point 'filename)))
            (when (> (length fname) 0)
              (push (if careful fname
                      (buffer-substring-no-properties (line-beginning-position)
                                                      (line-end-position)))
                    list))))
        (forward-line)))
    list))

(defun .directory-files (&optional dir dotfilesp full match nosort)
  "Return the list of files in DIR (current directory by default).
DOTFILESP non-nil causes dot files to be included in the list.
Regardless of the value of DOTFILESP, the two files . and .. are
never included. The remaining arguments are passed to
`directory-files'."
  (let ((raw (directory-files (or dir default-directory) full match nosort))
        (regexp (concat "\\`\\." (when dotfilesp "\\'\\|\\`\\.\\.\\'"))))
    (cl-delete-if (∘ (& 'string-match regexp) 'file-name-nondirectory) raw)))

(defun .dired-on-region (beg end &optional name ignore-regexp careful)
  "Open `dired' on the files between BEG and END.
NAME is the name of the virtual dired directory; defaults to the
current buffer name.
See `.region-file-names' for the meaning of the other arguments."
  (interactive `(,@(if (region-active-p)
                       (list (region-beginning) (region-end))
                     (list (point-min) (point-max)))
                 ,(.read-string-with-default "Name" nil (buffer-name))))
  (unless ignore-regexp (setq ignore-regexp comment-start-skip))
  (let ((files (.region-file-names beg end ignore-regexp careful)))
    (when files (dired (cons name files)))))

;; adapted from http://oremacs.com/2017/03/18/dired-ediff/
;; -*- lexical-binding: t -*-
;; (setq dired-dwim-target t)
(defun .dired-ediff-files ()
  (interactive)
  (let ((files (dired-get-marked-files))
        ;; (wnd (current-window-configuration))
        )
    (if (<= (length files) 2)
        (let ((file1 (car files))
              (file2 (if (cdr files)
                         (cadr files)
                       (read-file-name
                        "File: "
                        (dired-dwim-target-directory)))))
          (if (file-newer-than-file-p file1 file2)
              (ediff-files file2 file1)
            (ediff-files file1 file2))
          ;; (add-hook 'ediff-after-quit-hook-internal
          ;;           (lambda ()
          ;;             (setq ediff-after-quit-hook-internal nil)
          ;;             (set-window-configuration wnd)))
          )
      (error "No more than 2 files should be marked"))))

;; (defun .discover-file-upwards (name)
;;   "If you're searching for a file in the root directory, FORGET IT!
;; If you're wondering why \"discover\", you'd better wonder why `find-file'."
;;   (if buffer-file-name
;;       (let ((file (expand-file-name name
;;                                     (file-name-directory buffer-file-name))))
;;         (catch 'found
;;           (while (not (string= (concat "/" name) file))
;;             (when (file-exists-p file)
;;               (throw 'found file))
;;             (setq file (expand-file-name name
;;                                          (file-name-directory
;;                                           (directory-file-name
;;                                            (file-name-directory file))))))
;;           (message "No file named \"%s\" between us and root. Nothing. Really." name)
;;           nil))
;;     (error "Buffer is not visiting a file")))
(defun .discover-file-upwards (name &optional from)
  (concat
   (locate-dominating-file (or from buffer-file-name default-directory) name)
   name))

(defun .map-directory (fun dir &optional match nosort full)
  "(mapc fun (.directory-files dir t (or full t) match (or nosort t)))"
  (mapc fun (.directory-files dir t (or full t) match (or nosort t))))

;; adapted from https://emacs.stackexchange.com/questions/31621/handle-stale-desktop-lock-files-after-emacs-system-crash
(defun .purge-stale-desktop-lock (&optional dir)
  (require 'desktop)
  (when-let ((pid (desktop-owner dir)))
    (let ((retval (call-process "ps" nil nil nil "-p"
                                (number-to-string pid))))
      (unless (and (numberp retval) (zerop retval))
        (let ((lock-file (desktop-full-lock-name dir)))
          (message "Removing stale desktop lock file %s..." lock-file)
          (delete-file lock-file)
          (message "Removing stale desktop lock file %s...done" lock-file))))))

;;;_  . LISTS
(defun .ensure-list (o)
  "(if (listp o) o (list o))"
  (if (listp o) o (list o)))

(defun .index (o l)
  "Return zero-based index of OBJECT in LIST.
Comparison done with `equal'."
  (let ((i 0))
    (catch 'match
      (dolist (el l)
        (if (equal o el)
            (throw 'match i)
          (setq i (1+ i)))))))

(defun .take (n l)
  "Return first N elements of the list L."
  (let (r (l l))
    (while (and l (> n 0))
      (push (car l) r)
      (setq l (cdr l)
            n (1- n)))
    (nreverse r)))

(defun .take-while (p l)
  "Return all elements of list L until the first that doesn't satisfy predicate P."
  (let (r (l l))
    (while (and l (funcall p (car l)))
      (push (car l) r)
      (setq l (cdr l)))
    (nreverse r)))

(defun .zip (l1 l2)
  "(mapcar* 'cons l1 l2)"
  (cl-mapcar 'cons l1 l2))

;; (defun .mapcar-nth (function sequence &rest indices)
;;   "Map FUNCTION over elements of elements of SEQUENCE at zero-based INDICES.
;; FUNCTION should take as many arguments as there are INDICES and
;; return the same number of values."
;;   (mapcar (lambda (el)
;;             (let* ((newel (copy-sequence el))
;;                    (updates
;;                     (.zip indices
;;                           (funcall (if (cdr indices) 'identity 'list)
;;                                    (apply function
;;                                           (mapcar (& 'elt newel) indices))))))
;;               (mapc (lambda (update)
;;                       (setf (elt newel (car update)) (cdr update)))
;;                     updates)
;;               newel))
;;           sequence))

(defun .add-to-list (listvar el &optional append)
  (set listvar (if append
                   (append (delete el (symbol-value listvar)) (list el))
                 (cons el (delete el (symbol-value listvar))))))

(defun .mad-to-list (lvar els &optional append cmpfun)
  "Add elements contained in the list ELS to LVAR.
In short, multi- `add-to-list', which also see for explanation of
the optional arguments.
Returns the new value of LVAR."
  (dolist (el els (symbol-value lvar)) (add-to-list lvar el append cmpfun)))

(defun .mad-hooks (hooks &rest funcs)
  "Add each of FUNCS to each of HOOKS."
  (mapc (lambda (hook) (mapc (& 'add-hook hook) funcs)) hooks))

;;; FIXME this is weird
(defmacro .replace-in-list (lst exp rep)
  "Replace the first element of LST matching `pcase' EXP with REP."
  (declare (debug t))
  `(let ((orig (catch 'done
                 (let* ((tmp ,lst) (i (car tmp)))
                   (while i
                     (pcase i
                       (,exp (throw 'done i)))
                     (setq tmp (cdr tmp)
                           i (car tmp)))))))
     (setq ,lst (cons ,rep (if orig (delete orig ,lst) ,lst)))))

(defun .member-regexp (string list)
  "Return non-nil if STRING matches one of the regexps in LIST."
  (catch 'match
    (dolist (rx list) (and (string-match rx string) (throw 'match rx)))))

(defun .member-recursive (elt tree &optional equalp)
  "Return non-nil if TREE (= non-flat list) contains ELT.
Test for equivalence using EQUALP or `eq'."
  (unless equalp (setq equalp 'eq))
  (cl-flet ((rec (el tr eq)
           (cond ((null tr) nil)
                 ((funcall eq (car tr) el) tr)
                 ((consp (car tr)) (or (rec el (car tr) eq)
                                       (rec el (cdr tr) eq)))
                 (t (rec el (cdr tr) eq)))))
    (rec elt tree equalp)))

(defun .hash-table-to-list (ht &optional values)
  "Return a list containing all keys from hash table HT.
If VALUES is non-nil, return a list of non-nil values instead."
  (let (ret)
    (maphash (lambda (key val)
               (if values (when val (push val ret)) (push key ret)))
             ht)
    ret))

(defun .mapc-tree (function tree)
  "Call FUNCTION on every leaf of TREE (= general list).
Recursive."
  (cond ((null tree) nil)
        ((consp (car tree))
         (.mapc-tree function (car tree))
         (.mapc-tree function (cdr tree)))
        (t (funcall function (car tree))
           (.mapc-tree function (cdr tree)))))

;;;_  . FUNCTIONAL
;; (put 'λ 'lisp-indent-function 'defun)
;; (put 'λ 'edebug-form-spec 'lambda)
;; (defun .compose2 (f g)
;;   (let ((f f) (g g))
;;     (lambda (&rest args)
;;       (funcall f (apply g args)))))
;; ($ (.multiple-value-compose (& 'cons 8) (∘ 'values (lambda (a b) a))) 'a 'b)

;; Not really necessary. Just compose the function with `values'.
;; (defun .values-call (fn)
;;   "Return a function same as FN but returning its values using `values'.
;; Useful to work around deficient implementation of multiple values
;; in Elisp."
;;   (let ((fn fn))
;;     (lambda (&rest args) (values (apply fn args)))))

;; (defun .multiple-value-compose (&rest fns)
;;   "Variadic function composition usable with the cl.el multiple values hack.
;; Note that you have to return values from component functions
;; using `values' even if they only return a single value."
;;   (let ((fns fns))
;;     (if (cdr fns)
;;         (lambda (&rest args)
;;           (multiple-value-call
;;            (car fns)
;;            (apply (apply '.multiple-value-compose (cdr fns)) args)))
;;       (car fns))))

(defun .const (v)
  "Return a function that accepts any arguments and returns V."
  (let ((v v))
    (lambda (&rest _) v)))

;; (defun .negate (f)
;;   "Return a function that returns `not' of F's result."
;;   (let ((f f))
;;     (lambda (&rest args)
;;       (not (apply f args)))))

;;;_  . COMBINATORS
;; (defun .mapcfuns (args &rest funs)
;;   (unless (listp args) (setq args (list args)))
;;   (mapc (lambda (f) (apply f args)) funs))

;;;_  . STRINGS
(defun .string-tails (s)
  (let (r)
    (dotimes (i (length s) r)
      (push (substring s i) r))))

(defun .string-inits (s)
  (let (r)
    (dotimes (i (length s) r)
      (push (substring s 0 (1+ i)) r))))

;; (defun .string->utf-8-list (s)
;;   "Return list containing UTF-8 bytes encoding the string S."
;;   (let* ((u8s (encode-coding-string s 'utf-8))
;;          (idx (1- (length u8s)))
;;          r)
;;     (while (>= idx 0)
;;       (push (aref u8s idx) r)
;;       (setq idx (1- idx)))
;;     r))

;;;_  . KEYWORDS
;;; FIXME
;; (defsubst .kwarg (arg) (plist-get kwargs arg))

;;;_  . BUFFERS
(defun .get-buffer/pred (predicate)
  "Return the first buffer in buffer list for which PREDICATE returns true.
PREDICATE can be any function, receiving a single buffer as its
argument (the buffer is also current during the PREDICATE call)."
  (catch 'found
    (dolist (b (buffer-list))
      (with-current-buffer b
        (when (funcall predicate b)
          (throw 'found b))))))

(defun kill-this-buffer (&optional window)
  "The built-in `kill-this-buffer' somehow sucks.
Namely, strange things happen with emacsclient frames."
  (interactive "P")
  (if window (kill-buffer-and-window) (kill-buffer nil)))

(defun .kill-buffer-other-window (&optional arg)
  "Kill the buffer in the next (i.e. non-selected) visible window.
Without a prefix argument, kill the window itself, too."
  (interactive "P")
  (kill-buffer (window-buffer (next-window)))
  (unless arg
    (and (not (one-window-p)) (delete-window (next-window)))))

(defvar .buffer-ignore-regexps
  `("\\` "
    ,(regexp-opt '("*anything" "*Apropos*" "*Article*" "*BBDB*"
                   "*Buffer List*" "*buffer-selection*" "*Completions*"
                   "*Group*" "*Help*" "*Ibuffer*" "*magit" "*Messages*"
                   "*Quail" "*w3m" " (Sunrise)")))
  "List of regexps matching buffer names to skip when switching buffers.")

(defun .next-buffer (&optional arg)
  "Switch to the next buffer not matching `.buffer-ignore-regexps'.
A prefix argument prevents the check and just calls
`next-buffer'."
  (interactive "P")
  (next-buffer)
  (when (and (not arg)
             (.member-regexp (buffer-name (current-buffer))
                             .buffer-ignore-regexps))
    (.next-buffer)))

(defun .previous-buffer (&optional arg)
  "Switch to the previous buffer not matching `.buffer-ignore-regexps'.
A prefix argument prevents the check and just calls
`previous-buffer'."
  (interactive "P")
  (previous-buffer)
  (when (and (not arg)
             (.member-regexp (buffer-name (current-buffer))
                             .buffer-ignore-regexps))
    (.previous-buffer)))

(defun .snap-region (start end)
  "Delete region between BEG and END and return it as a string.
Text properties are stripped."
  (prog1 (buffer-substring-no-properties start end)
    (delete-region start end)))

(defun .snap-thing (thing)
  "Delete THING and return it as a string."
  (let ((bounds (bounds-of-thing-at-point thing)))
    (when bounds (.snap-region (car bounds) (cdr bounds)))))

;;;_  . WINDOWS
;; Based on http://www.cabochon.com/~stevey/blog-rants/my-dot-emacs-file.html
(defun .swap-windows ()
  "Swap two windows."
  (interactive)
  (unless (= (count-windows) 2)
    (error "You need exactly 2 windows to do this"))
  (let* ((w1 (cl-first (window-list)))
         (w2 (cl-second (window-list)))
         (b1 (window-buffer w1))
         (b2 (window-buffer w2))
         (s1 (window-start w1))
         (s2 (window-start w2)))
    (set-window-buffer w1 b2)
    (set-window-buffer w2 b1)
    (set-window-start w1 s2)
    (set-window-start w2 s1)))

;; (defun .goto-mru-window ()
;;   (interactive)
;;   (select-window (frame-parameter nil '.last-selected-window)))

;; (defadvice select-window (before .save-selected-window activate)
;;   (set-frame-parameter nil '.last-selected-window (selected-window)))

(defun .only-two-windows (&optional kill)
  "Leave only two windows on the current frame."
  (interactive "P")
  (mapc (if kill (lambda (w) (with-current-buffer (window-buffer w)
                               (kill-buffer-and-window)))
          'delete-window)
        (cddr (window-list))))

(defun .fullscreen ()
  (interactive)
  (set-frame-parameter nil 'fullscreen
                       (if (frame-parameter nil 'fullscreen) nil 'fullboth)))

;; (defun pretty-lambdas ()
;;   (font-lock-add-keywords
;;    nil `(("(?\\(lambda\\>\\)"
;;           (0 (progn (compose-region (match-beginning 1) (match-end 1)
;;                                     ,(make-char 'greek-iso8859-7 107))
;;                     nil))))))

;; courtesy Steve Borgmann (<emacs-devel@gnu.org> Message-ID: <87ocnopmw8.fsf@escher.local.home>)
(defun .display-mode-line-string (&optional arg)
  "Display the unpropertized mode-line string in the echo area.
With prefix argument ARG, insert it at point in the current buffer."
  (interactive "P")
  (let ((mode-line-string
         (eval-expression '(format-mode-line mode-line-format 0))))
    (and arg (insert mode-line-string))))

;;;_  . FRAMES

(defun .raise-frame (&optional frame)
  (if (eq (window-system frame) 'x)
      (x-send-client-message nil 0 frame "_NET_ACTIVE_WINDOW" 32 '(1))
    (raise-frame frame)))


;;;_  . FACES
;; courtesy Miles Bader
;; <http://permalink.gmane.org/gmane.emacs.devel/131270>
;; You can also have face properties that are lists of
;; faces/face-attributes, and add or remove stuff from these.

;; Some example functions below, `add-to-face-property' and
;; `remove-from-face-property', with which you can do stuff like:

;;   (add-to-face-property 'variable-pitch START END)
;;   (add-to-face-property '(:weight bold) (+ START 5) (- END 10))
;;   (remove-from-face-property 'variable-pitch START END)
(defun .single-face-p (face)
  "Return non-nil if FACE seems to be a single face or face-attribute list
\(as oppposed to a list of faces/face-attribute-lists)."
  (or (symbolp face)			; face-symbol
      (and (listp face)
           (keywordp (car face))	; (:attr val ...)
           (keywordp (cadr face)))))	; (face-symbol :attr val ...)

(defun .add-to-face-property (face start end)
  "Add FACE to the face text-property of the region from START to END.
Any existing face text-properties are preserved by adding FACE to
the beginning of a list, making the existin value into a list if
necessary first.  It can be removed with `remove-from-face-property'."
  (while (< start end)
    (let ((next (next-char-property-change start end)))
      (let ((cur (get-char-property start 'face)))
        (when (.single-face-p cur)
          (setq cur (list cur)))
        (push face cur)
        (put-text-property start next 'face cur))
      (setq start next))))

(defun .remove-from-face-property (face start end)
  "Remove FACE from the face text-property of the region from START to END.
The assumption is that it was added previously by `add-to-face-property'."
  (while (< start end)
    (let ((next (next-char-property-change start end)))
      (let ((cur (get-char-property start 'face)))
        (when (.single-face-p cur)
          (setq cur (list cur)))
        (when (member face cur)
          (put-text-property start next 'face (remove face cur))))
      (setq start next))))

;;; http://www.emacswiki.org/emacs/AngryFruitSalad
(defun .wash-out-color (color &optional degree)
  "Return a colour string specifying a washed-out version of COLOR."
  (unless degree (setq degree 1))
  (let ((base (color-values (face-attribute 'default :foreground nil t)))
        (old (color-values color))
        new)
    (while old
      (push (/ (/ (+ (pop old)
                     (* degree (pop base)))
                  (1+ degree))
               256)
            new))
    (apply 'format "#%02x%02x%02x" (nreverse new))))

(defun .adjust-color (color &optional base degree)
  "Return a colour string specifying a washed-out version of COLOR."
  (unless degree (setq degree 1))
  (let ((base (color-values
               (or base (face-attribute 'default :foreground nil t))))
        (old (color-values color)) new)
    (while old
      (push (/ (/ (+ (pop old)
                     (* degree (pop base)))
                  (1+ degree))
               256)
            new))
    (apply 'format "#%02x%02x%02x" (nreverse new))))

;;;_ . WWW
;;; cf. goto-addr.el
(defun .buttonize-urls ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (let ((inhibit-read-only t))
      (while (re-search-forward .url-regexp nil t)
        (replace-match
         (propertize
          (match-string 0)
          'fontified t
          'button '(t)
          'url (match-string 0)
          'category 'default-button
          'help-echo "Press me hard!"
          'action (lambda (b)
                    (let ((url (button-get b 'url)))
                      (unless (and current-prefix-arg
                                   (not (y-or-n-p
                                         (format "Title: %s\nBrowse? "
                                                 (.url-title url)))))
                        (browse-url url)))))
         t t)))))

(defun .url-title (url &optional kill)
  "Return the title of the page at URL."
  (interactive (list (.read-string-with-default "URL" nil (.region-or 'url))
                     current-prefix-arg))
  (with-current-buffer (url-retrieve-synchronously url)
    (when (re-search-forward "<title>\\(.*?\\)</title>" nil t)
      (message "%s"
               (prog1 (match-string 1)
                 (unless (and (called-interactively-p 'any)
                              (not kill)
                              (not (y-or-n-p "Kill the web page buffer? ")))
                   (kill-buffer nil)))))))

(defun .wwwhat-changed (url &optional file)
  "Update FILE with contents of URL if newer.
FILE defaults to the decoded tail (non-directory part) of URL."
  (interactive (list (.read-string-with-default
                      "URL" (defvar .wwwhat-changed-url-history nil)
                      (.match-nearest-point .url-regexp))
                     (read-file-name
                      "File: " nil
                      ;; I hate you, Emacs
                      (url-unhex-string (file-name-nondirectory url)))))
  (let ((file (expand-file-name
               (or file (url-unhex-string (file-name-nondirectory url)))))
        old)
    (when (file-exists-p file)
      (setq old (make-temp-file file))
      (copy-file file old t))
    (unwind-protect
         (let ((nbytes (shell-command-to-string
                        (mapconcat 'shell-quote-argument
                                   (list "dlupdate" url file)
                                   " "))))
           (if (zerop (string-to-number nbytes))
               (message "%s not newer than local version" url)
             (diff old file nil t)))
      (when old (delete-file old)))))

;; (defvar .search-zdic-history nil)
;; (defun .search-zdic (s)
;;   (interactive (let ((def (word-at-point)))
;;                  (list (read-string (.prompt-with-default def "Search for")
;;                                     nil .search-zdic-history def t))))
;;   (let ((url (format "http://zdic.net/zd/zi/%s.htm"
;;                      (replace-regexp-in-string "%" "Zdic"
;;                                                (url-hexify-string s)))))
;;     (browse-url-firefox url)))

;; (defun .encode-coding-uri-component (s &optional coding)
;;   (mapconcat (lambda (c) (format "%%%02X" c))
;;              (string-as-unibyte (encode-coding-string s (or coding 'utf-8)))
;;              ""))

;; (defun .urldecode (string)
;;   (let ((case-fold-search t))           ; FIXME
;;     (.replace-regexps-in-string
;;      (("\\+" " ")
;;       ("%\\([0-9A-F][0-9A-F]\\)"
;;        (λ (_) (string (string-to-number (match-string 1) 16)))))
;;      string)))

;; (.url-query-string '("a" . "8,ahoj,haahaha") '("b" . "10"))
;; (let ((a "ahoj,haha")
;;       (b "kuk,prd jup+&"))
;;   (.url-query-string 'a 'b))

;;;_ . SEARCH & REPLACE
(defun .replace-regexps-in-string (items string)
  "Return STRING with replacements done according to the list ITEMS.
Every element of ITEMS is of the form (REGEXP REPLACEMENT
FIXEDCASE LITERAL SUBEXP START), the latter four of which are
optional. For their meaning see `replace-regexp-in-string'."
  (let ((ret string))
    (dolist (item items ret)
      (setq ret
            (apply 'replace-regexp-in-string (car item) (cadr item) ret
                   (cddr item))))))

(defun .multi-occur-all-buffers (text)
  (interactive
   (list
    (.read-string-with-default "Find in all buffers"
                               (defvar .multi-occur-all-buffers-history nil)
                               (.region-or 'Word)
                               t)))
  (multi-occur (buffer-list) text))
;;;_ . EDITING
;; why does this version move point 8-X?
;; (defun .invert-case-region (beg end)
;;   (interactive "r")
;;   (save-excursion
;;     (goto-char beg)
;;     (while (<= (point) end)
;;       (let ((c (char-after)))
;;         (delete-char 1)
;;         (insert (if (eq (upcase c) c) (downcase c) (upcase c)))))))

(defun .cleanup-buffer ()
  "Reindent, delete trailing whitespace, untabify."
  (interactive)
  (whitespace-cleanup)
  (indent-region (point-min) (point-max)))

(defun .comment-or-uncomment-region-or-line ()
  "Comment or uncomment a line or a region."
  (interactive)
  (if (region-active-p)
      (comment-or-uncomment-region (region-beginning) (region-end))
    (comment-or-uncomment-region (line-beginning-position)
                                 (line-end-position))))

(declare-function etags-select-find-tag "etags-select.el" nil)
(defun .tag-jump-smart (&optional override)
  "Jump to a tag, possibly prompting for creating and selecting a tags table.
With OVERRIDE, always ask, even though `tags-table-list' is set."
  (interactive "P")
  (when (or (not tags-table-list) override)
    (let ((file
           (let ((def (.discover-file-upwards "TAGS")))
             (read-file-name (.prompt-with-default def "Tags file")
                             nil def 'confirm))))
      (unless (file-exists-p file)
        (shell-command "ctags -f %s -e -R %s" file
                       (file-name-directory file)))
      (setq tags-table-list (list file))))
  (etags-select-find-tag))

(defun .tabular-transpose (beg end &optional delim)
  "Transpose rows into columns in tabular text delimited by BEG and END.
DELIM is the field (column) separator (rows are separated by newlines)."
  (interactive "r")
  (unless delim
    (setq delim
          (if current-prefix-arg
              (read-string "Field (column) delimiter: " "\t")
            "\t")))
  (insert
   (mapconcat (λ (l) (mapconcat 'identity l (or delim "\t")))
              (apply 'map 'list 'list
                     (mapcar (& (.flip 'split-string) delim)
                             (split-string (delete-and-extract-region beg end)
                                           "\n")))
              "\n")))

;;;_  . KILL RING & SELECTION
(defun .kill-ring-to-primary (&optional arg)
  "Set ARGth kill as current primary X selection."
  (interactive "p")
  (gui-set-selection nil (nth (1- arg) kill-ring)))

(defun .write-region (file &optional append)
  "Write current region to FILE, appending if APPEND.
Interactively, prompt for FILE, APPEND == prefix arg."
  (interactive "FFile:\nP")
  (write-region (region-beginning) (region-end) file append))

(defun .thing-to-selection (thing &optional selection)
  "Copy THING to SELECTION.
THING is passed to `thing-at-point'; SELECTION is either `nil',
`PRIMARY' or `SECONDARY', designating the corresponding X
selections; or anything else, meaning to kill THING and copy it
to the system clipboard."
  (let ((thing (thing-at-point thing)))
    (when thing
      (if (memq selection '(nil PRIMARY SECONDARY))
          (gui-set-selection selection thing)
        (let ((select-enable-clipboard t))
          (kill-new thing))))))

;;;_  . SCRATCH
(defvar .scratch-history nil)
(defun .scratch (&optional mode new)
  (interactive
   (list
    (.completing-read-thing
     major-mode nil "Mode" nil
     (lambda (s) (and (string-match "-mode\\'" (symbol-name s))
                      (not (string-match "minor-mode\\'" (symbol-name s)))))
     '.scratch-history major-mode)))
  (let* ((bname (format "*scratch(%s)*" mode))
         (buf (if new (generate-new-buffer bname)
                (get-buffer-create bname))))
    (with-current-buffer buf (funcall (intern mode)))
    (pop-to-buffer buf)))

;;;_  . TEXT PROPERTIES
(defun .strip-properties-region (beg end)
  "Strip any text properties between BEG and END."
  (interactive "r")
  (with-silent-modifications
    (set-text-properties beg end nil)))

;;;_  . WHITESPACE
(defun .xml-un-pretty-print (string)
  (replace-regexp-in-string ">\\([ \t\n]+?\\)<" "" string nil nil 1))

(defun .xml-prettify-region (beg end)
  (interactive "r")
  (save-excursion
    (goto-char beg)
    (while (search-forward-regexp ">[ \t]*<" nil t)
      (backward-char) (insert "\n"))
    (let ((indent-line-function 'nxml-indent-line))
      (indent-region beg end))))

(defun .batch-indent-files (&optional dir)
  "Batch-indent Elisp files in directory DIR (current by default)."
  (interactive "DDirectory: ")
  (let ((files (.directory-files (or dir default-directory) t nil "\\.el\\'" t))
        indent-tabs-mode)
    (dolist (f files)
      (find-file f)
      (lisp-indent-region (point-min) (point-max))
      (shell-command-on-region (point-min) (point-max) "cat -s" t t)
      (untabify (point-min) (point-max))
      (save-buffer))))

(defun .just-one-space-region (&optional beg end)
  (interactive "r")
  (replace-regexp " +" " " nil beg end)
  (indent-region beg end))

(defun .sudo-edit (&optional arg)
  (interactive "P")
  (if arg
      (find-file (concat "/sudo:root@localhost:" (ido-read-file-name "File: ")))
    (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))

(defun .revert-buffer-with-sudo ()
  "Revert buffer using tramp sudo.
This will also preserve changes already made by a non-root user."
  (interactive)
  (let ((f buffer-file-name))
    (when f
      (let ((content (when (buffer-modified-p) (widen) (buffer-string))))
        (if (file-writable-p f)
            (revert-buffer)
          (kill-buffer (current-buffer))
          (if (file-remote-p f)
              (find-file
               (replace-regexp-in-string "^\\/[^:]+:" "/sudo:" f))
            (find-file (concat "/sudo::" f)))
          (when content
            (let ((buffer-read-only nil))
              (erase-buffer)
              (insert content))))))))

(defvar .emacs-source-dir "/home/stepnem/Hacking/emacs/emacs-roc")
(defun .add-source-dir-to-load-path ()
  (interactive)
  (let ((default-directory (expand-file-name "lisp/" .emacs-source-dir)))
    (normal-top-level-add-subdirs-to-load-path))
  load-path)

(defun .edit-emacs-source (&optional arg)
  "Edit the private file corresponding to the currently opened Emacs source file.
With a prefix argument, ask for the file to edit in the minibuffer."
  (interactive "P")
  (let ((pos (point))
        (default
          (replace-regexp-in-string
           ".gz$" ""
           (replace-regexp-in-string
            "^.+?emacs/[0-9.]+" .emacs-source-dir
            (buffer-file-name)))))
    (find-file
     (if arg (read-file-name "File name:" .emacs-source-dir default t)
       default))
    (goto-char pos)))

;;;_  . ENCODING
(defun .detox-region (&optional beg end encoding)
  (interactive "r\nzCoding system: ")
  (let ((inhibit-read-only t))
    (save-restriction
      (narrow-to-region beg end)
      (goto-char (point-min))
      (while (search-unencodable-char 'utf-8)
        (decode-coding-region (1- (point)) (point) encoding)))))

(defun .unicode-unescape-region (&optional beg end)
  "Transform \\udead escapes to their character equivalents."
  (interactive "r")
  (let ((inhibit-read-only t) (case-fold-search t))
    (save-restriction
      (narrow-to-region beg end)
      (goto-char (point-min))
      (while (search-forward-regexp "\\\\u\\([0-9a-f]\\{4\\}\\)" nil t)
        (replace-match
         (char-to-string (string-to-number (match-string 1) 16)))))))

;;;_  . KEYBINDINGS
(defmacro .careful-define-key (keymap key def &optional force)
  "Bind KEY to DEF in KEYMAP if not already defined."
  (declare (debug t) (indent define-key))
  (let ((orig (make-symbol "orig")))
    `(let ((,orig (lookup-key ,keymap ,key)))
       (if (and ,orig (not ,force))
           (display-warning
            '(emacs dotelib .careful-define-key)
            (format "%S already bound to %S in %S" ,key ,orig ',keymap))
         (define-key ,keymap ,key ,def)))))

;; cf. [[gnus:gmane.emacs.devel#87ipzcqe1z.fsf@fastmail.fm][Email from Tassilo Horn: Re: suggestion for tab keybind]]
(defmacro .define-context-key (keymap key command predicate &optional mode)
  "Bind KEY in KEYMAP to a command which calls COMMAND if PREDICATE is non-nil.

If KEYMAP is nil, bind KEY in `global-map'.

PREDICATE can be a variable, function to call without arguments
or any Elisp form to evaluate.

If PREDICATE doesn't evaluate to non-nil value and KEY is
normally bound in KEYMAP, the corresponding default command will
be executed.

If KEY isn't normally bound in KEYMAP, MODE (defaulting to
s/KEYMAP/-map//) will be disabled temporarily (to prevent
infinite recursion) and the function which is then bound to KEY
will be called.

Example:

  ;; TAB on an outline heading toggles visibility in `outline-minor-mode'
  (.define-context-key outline-minor-mode-map
    (kbd \"TAB\")
    ;; This evals to non-nil, if `point' is on an outline-heading
    'outline-toggle-children
    (save-excursion
      (goto-char (line-beginning-position))
      (looking-at outline-regexp)))

  ;; TAB at end of line inserts a TAB character
  (.define-context-key outline-minor-mode-map
    (kbd \"TAB\")
    'self-insert-command
    eolp)

The context key for KEYMAP and KEY which was given last takes
precedence, so in this example TAB at the end of a line of an
outline heading inserts TAB and doesn't toggle the visibility."
  (declare (debug t) (indent 0))
  (if (null keymap) (setq keymap 'global-map mode nil)
    (unless mode
      (setq mode (intern-soft
                  (replace-regexp-in-string "-map" "" (symbol-name keymap))))))
  `(define-key ,keymap ,key
     (defun ,(intern (replace-regexp-in-string
                      "[^[:graph:]]+" "-"
                      (format "%s-IF-%s" command predicate)))
         ()
       ,(format "Execute `%s' if `%s' returns non-nil." command predicate)
       (interactive)
       (if ,(cond ((custom-variable-p predicate) predicate)
                  ((functionp predicate) `(funcall #',predicate))
                  (t `(eval ,predicate)))
           (call-interactively #',command)
         ,(let ((default-cmd (lookup-key (symbol-value keymap) key)))
            (if (commandp default-cmd)
                `(call-interactively #',default-cmd)
              `(let (,mode)
                 (call-interactively (key-binding ,key)))))))))
;; (.define-context-key nil "k" t nil)
;;;_  . MINIBUFFER

;; (defun .frob-default (default)
;;   (let ((def (cl-case (type-of default)
;;                (function (funcall default))
;;                ((string symbol) default)
;;                (t (eval default)))))
;;     (or def (.region-or 'word))))

;; (defun .complete-with-default (prompt-prefix
;;                                collection &optional hist default require-match
;;                                predicate inherit-input-method initial-input)
;;   (let ((def (if default (.frob-default default)
;;                (when hist (car (symbol-value hist))))))
;;     (completing-read (.prompt-with-default def prompt-prefix)
;;                      collection predicate require-match initial-input hist
;;                      def inherit-input-method)))

;; (defun .read-string-with-default (prompt-prefix
;;                                   &optional hist default inherit-input-method
;;                                   initial-input)
;;   (let ((def (.frob-default default)))
;;     (read-string (.prompt-with-default def prompt-prefix) initial-input hist
;;                  def inherit-input-method)))

(defun .completing-read-thing (thing
                               &optional transformation prompt collection
                                         predicate hist def require-match
                                         initial-input inherit-input-method)
  (let ((def (if (symbolp thing)
                 (funcall (or transformation
                              (when predicate
                                (lambda (def)
                                  (when (funcall predicate def) def)))
                              'identity)
                          (thing-at-point thing))
               thing)))
    (completing-read
     (.prompt-with-default def (or prompt (capitalize (symbol-name thing))))
     (or collection obarray) predicate require-match initial-input hist def
     inherit-input-method)))

(defun .prompt-with-default (default prefix &optional deffmt suffix)
  "Return a prompt suitable for functions like `read-string'.
The outcome depends on whether DEFAULT is actually non-nil. This
saves you from typing boiler-plate all the time when using the
braindead Emacs minibuffer-reading functions. This is more of a
lower-level function, see `.read-string-with-default' and friends
which use it.

Typical simple usage:

\(defun .search-zdic (s)
   (interactive (let ((def (word-at-point)))
                  (list (read-string (.prompt-with-default def \"Search for\")
                                     nil .search-zdic-history def t))))
   (let ((url (format \"http://zdic.net/zd/zi/%s.htm\"
                      (replace-regexp-in-string \"%\" \"Zdic\"
                                                (url-hexify-string s)))))
     (browse-url-firefox url)))"
  (concat prefix
          (and default (format (or deffmt " (%s)") default))
          (or suffix ": ")))

;;;_  . COMPLETION
(defmacro .define-completion-at-point (prefix thing cond-form &rest body)
  "Define a function suitable for `completion-at-point-functions'.
Defines a PREFIX-completion-at-point function, completing THING
if COND-FORM returns true. BODY is used to define a caching
completion generation function called PREFIX-completions, storing
the completions generated in a PREFIX-completions-cache variable.
The cached completions are used unless `current-prefix-arg' is
non-nil."
  (declare (debug t) (indent 3))
  (let ((cachevar (.format-symbol "%s-completions-cache" prefix))
        (genfun (.format-symbol "%s-completions" prefix)))
    `(progn
       (defvar ,cachevar nil
         ,(format "Cache used by `%s'." genfun))
       (defun ,genfun (&optional nocache)
         (if (or nocache (null ,cachevar))
             (setq ,cachevar (progn ,@body))
           ,cachevar))
       (defun ,(.format-symbol "%s-completion-at-point" prefix) ()
         (when ,cond-form
           (let ((bounds (bounds-of-thing-at-point ',thing)))
             (list (or (car bounds) (point))
                   (or (cdr bounds) (point))
                   (,genfun current-prefix-arg))))))))

;;;_  . VARIABLES
(defun .bound-and-true (sym)
  "`bound-and-true-p' done right."
  (when (boundp sym) (symbol-value sym)))

(defun .variable-at-point ()
  "Sanitized version of the utterly braindead `variable-at-point'.
\(The latter returns 0 as the failure value. Very useful,
indeed.)"
  (let ((v (variable-at-point)))
    (unless (eq v 0) v)))

(defun .edit-variable (var)
  "Edit the value of VAR.
When its current value exceeds single line, edit it in a
dedicated temporary buffer."
  (interactive
   (let ((def (.variable-at-point)))
     (list (read (completing-read (.prompt-with-default def "Edit variable")
                                  obarray 'boundp nil nil nil def)))))
  (let ((val (symbol-value var)))
    (if (> (length (prin1-to-string val)) 80)
        (let ((buf (generate-new-buffer "VarEdit"))
              (wincfg (current-window-configuration)))
          (pop-to-buffer buf)
          (prin1 val buf)
          (message "Type C-c C-c when finished")
          (local-set-key "\C-c\C-c" (let ((var var) (wincfg wincfg))
                                      (lambda ()
                                        (interactive)
                                        (set var (progn
                                                   (goto-char (point-min))
                                                   (read (current-buffer))))
                                        (kill-buffer nil)
                                        (set-window-configuration wincfg)))))
      (set var (read-minibuffer "Value: " (prin1-to-string val))))))

;;;_  . HELP
;;(find-node "(elisp)Accessing Documentation" "(defun describe-symbols (pattern)")
(defun .describe-symbols (pattern)
  "Describe the Emacs Lisp symbols matching PATTERN.
All symbols that have PATTERN in their name are described
in the `*Help*' buffer."
  (interactive "sDescribe symbols matching: ")
  (let ((describe-func
         (function
          (lambda (s)
            ;; Print description of symbol.
            (if (fboundp s)             ; It is a function.
                (princ
                 (format "%s\t%s\n%s\n\n" s
                         (if (commandp s)
                             (let ((keys (where-is-internal s)))
                               (if keys
                                   (concat
                                    "Keys: "
                                    (mapconcat 'key-description
                                               keys " "))
                                 "Keys: none"))
                           "Function")
                         (or (documentation s)
                             "not documented"))))

            (if (boundp s)              ; It is a variable.
                (princ
                 (format "%s\t%s\n%s\n\n" s
                         (if (custom-variable-p s)
                             "Option " "Variable")
                         (or (documentation-property
                              s 'variable-documentation)
                             "not documented")))))))
        sym-list)

    ;; Build a list of symbols that match pattern.
    (mapatoms (function
               (lambda (sym)
                 (if (string-match pattern (symbol-name sym))
                     (setq sym-list (cons sym sym-list))))))

    ;; Display the data.
    (help-setup-xref (list 'describe-symbols pattern) (called-interactively-p 'any))
    (with-help-window (help-buffer)
      (mapcar describe-func (sort sym-list 'string<)))))

(defun .symbol-docstring (symbol &optional functionp)
  "Return the docstring for SYMBOL.
When FUNCTIONP is non-nil (interactively, the prefix argument),
inspect SYMBOL's function definition, otherwise inspect the
variable definition. When called interactively, the docstring is
also displayed in a tooltip."
  (interactive
   (list (.complete-with-default (if current-prefix-arg "Function" "Variable")
                                 obarray
                                 (defvar .symbol-docstring-history nil)
                                 (symbol-at-point)
                                 t (if current-prefix-arg 'fboundp 'boundp))
         current-prefix-arg))
  (let ((docstring
         (if functionp (documentation symbol)
           (documentation-property symbol 'variable-documentation))))
    (when (called-interactively-p 'any)
      (tooltip-show (or docstring "No docstring found")))
    docstring))

(defun .where-is-useful (definition
                         &optional keymap firstonly noindirect no-remap)
  "A useful version of `where-is'."
  (interactive
   (list (intern (.completing-read-thing 'symbol nil "Command" nil 'commandp))
         (mapcar
          (lambda (s) (symbol-value (intern s)))
          (let ((trans (lambda (s)
                         (if (string-match "mode" s)
                             (concat (substring s 0 (match-end 0)) "-map")
                           s)))
                list more)
            (while (y-or-n-p (if more "More maps? " "Specify a keymap? "))
              (setq more t)
              (push (.completing-read-thing 'symbol trans
                                            "Keymap" nil '.keymapp)
                    list))
            (nreverse list)))))
  (let ((keys (mapconcat 'key-description (where-is-internal
                                           definition keymap firstonly
                                           noindirect no-remap)
                         ", ")))
    (message "%s" (if (> (length keys) 0) keys "Not bound"))))

(provide 'dotelib-priv)

;;; found in emms-info-libtag.el
;; (defmacro .with-keyword-args (plist args &rest body)
;;   (declare (debug t) (indent 2))
;;   (let ((getter (make-symbol "getter")))
;;     `(let* ((,getter (& 'plist-get (symbol-value ,plist)))
;;             ,@(mapcar
;;                (lambda (a) `(,a ($ ,getter ,(.format-symbol ":%s" a))))
;;                args))
;;        ,@body)))

;; (defmacro .with-keyword-args (plist args &rest body)
;;   (declare (debug t) (indent 2))
;;   (let ((getter (& 'plist-get plist)))
;;     `(let (,@(mapcar
;;               (lambda (a) `(,a ,($ getter (.format-symbol ":%s" a))))
;;               args))
;;        ,@body)))

;; (defun .kwargs-list-p (object)
;;   "Return non-nil if OBJECT is a keyword argument list."
;;   (find-if 'keywordp object))

;; (defun .test (&rest kwargs)
;;   (.with-keyword-args kwargs (prompt collection predicate require-match)
;;     (completing-read prompt collection predicate require-match)))

;; (.with-keyword-args (:ahoj "ahoj" :meh "meh") (ahoj meh))
;; (defmacro .with-process-output (program args &rest kwargs/body)
;;   "Execute BODY in a temporary buffer containing output of PROGRAM."
;;   (declare (debug t) (indent 2))
;;   (let ((body (or (plist-get kwargs/body :body)
;;                   `(progn ,@kwargs/body))))
;;     `(.with-keyword-args ,kwargs/body (infile buffer display)
;;        (with-temp-buffer
;;          (when (zerop (call-process ,program infile
;;                                     (or buffer '(t nil)) display ,args))
;;            ,body)))))

;; (.with-process-output emms-info-libtag-program-name (emms-track-name track)
;;   :infile "hovno"
;;   :body
;;   (progn
;;     (goto-char (point-min))
;;     ;; Crush the trailing whitespace
;;     (while (re-search-forward "[[:space:]]+$" nil t)
;;       (replace-match "" nil nil))
;;     (goto-char (point-min))
;;     (while (looking-at "^\\([^=\n]+\\)=\\(.*\\)$")
;;       (let ((name (intern (match-string 1)))
;;             (value (match-string 2)))
;;         (when (> (length value) 0)
;;           (emms-track-set track name value)))
;;       (forward-line 1))))

;; (defun .pp-macroexpand-last-sexp (arg)
;;   (interactive "P")
;;   (let ((e (pp-last-sexp)))
;;     (cond ((equal arg '(64)) (insert (pp-to-string (macroexpand e))))
;;           ((equal arg '(16))
;;            (pp-display-expression (macroexpand e) "*Pp Macroexpand Output*"))
;;           (arg (insert (pp-to-string (macroexpand-all e))))
;;           (t (pp-display-expression (macroexpand-all e)
;;                                     "*Pp Macroexpand Output*")))))

;;;_ . MISC

;;;###autoload
(defun .drones-update-summary ()
  "Insert a commit message summarising borg drones update."
  (interactive)
  (save-excursion
    (let* ((modules (save-restriction
                      (narrow-to-region
                       (re-search-forward "# Changes to be committed:\n")
                       (re-search-forward "#\n"))
                      (.collect-matches "lib/\\(.*\\)" 1 t)))
           (count (length modules)))
      (goto-char (point-min))
      (insert "Update " (number-to-string count)
              " drone" (if (= count 1) "" "s")
              "\n\n")
      (magit-with-toplevel
        (let ((col-format (format "%%-%is"
                                  (apply 'max (mapcar 'length modules)))))
          (dolist (module (nreverse modules))
            (let ((default-directory
                   (expand-file-name (concat "lib/"
                                             (file-name-as-directory module)))))
              (insert "Updated " (format col-format module)
                      " to " (magit-git-string "describe" "--tags" "--always")
                      ?\n))))))))

;;;_ . PARAPHERNALIA
(ignore-errors
  (let ((keyword-regex
         (concat "(\\("
                 (regexp-opt '(".aif" ".defkeymap" ".deflocalvar" ".defprefix"
                               ".setq-local" ".update-struct"
                               ".with-executing-kbd-macro-nil"
                               ".with-made-symbols" ".with-ephemeral-files"
                               ".with-input-from-file" ".with-open-file"
                               ".with-output-to-file" ".with-struct-accessors"
                               "λ"))
                 "\\)\\>")))
    (font-lock-add-keywords 'emacs-lisp-mode `((,keyword-regex . 1)))))

(defun .snarf-macro-indent-specs ()
  (let ((macros (.collect-matches "^(defmacro\\*? \\([^( ]+\\)" 1 t)))
    (cl-loop for m in macros
          for s = (intern m)
          for lif = (get s 'lisp-indent-function)
          do (put s 'common-lisp-indent-function
                  (if (symbolp lif) (list 'as lif) lif)))))

(.with-input-from-file load-file-name
  (.snarf-macro-indent-specs))

;;;_. fileLocalVariables
;; Local Variables:
;; allout-layout: (* 0 :)
;; End:
