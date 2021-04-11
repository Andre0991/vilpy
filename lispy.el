;;; lispy.el --- vi-like Paredit. -*- lexical-binding: t -*-

;; Author: André Peric Tavares <andre.peric.tavares@gmail.com>
;; URL: https://github.com/Andre0991/lispy-lite
;; Version: 0.1.5
;; Keywords: lisp

;; This file is not part of GNU Emacs

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This is a stripped-down fork of the excellent [lispy](https://github.com/abo-abo/lispy), a paredit-like mode.
;; For more details, please see https://github.com/Andre0991/lispy-lite.
;;
;; Due to the structure of Lisp syntax it's very rare for the
;; programmer to want to insert characters right before "(" or right
;; after ")".  Thus unprefixed printable characters can be used to call
;; commands when the point is at one of these locations, which are
;; further referred to as special.
;;
;; Conveniently, when located at special position it's very clear to
;; which sexp the list-manipulating command will be applied to, what
;; the result be and where the point should end up afterwards.  You
;; can enhance this effect with `show-paren-mode' or similar.
;;
;; Here's an illustration to this effect, with `lispy-clone' ("*"
;; represents the point):
;; |--------------------+-----+--------------------|
;; | before             | key | after              |
;; |--------------------+-----+--------------------|
;; |  (looking-at "(")* |  c  |  (looking-at "(")  |
;; |                    |     |  (looking-at "(")* |
;; |--------------------+-----+--------------------|
;; | *(looking-at "(")  |  c  | *(looking-at "(")  |
;; |                    |     |  (looking-at "(")  |
;; |--------------------+-----+--------------------|
;;
;; When special, the digit keys call `digit-argument', since most
;; `lispy' commands accept a numeric argument.  For instance, "3c" is
;; equivalent to "ccc" (clone sexp 3 times), and "4j" is equivalent to
;; "jjjj" (move point 4 sexps down).  Some useful applications are
;; "9l" and "9h" - they exit list forwards and backwards respectively
;; at most 9 times which makes them effectively equivalent to
;; `end-of-defun' and `beginning-of-defun'.
;;
;; To move the point into a special position, use:
;; "]" - calls `lispy-forward'
;; "[" - calls `lispy-backward'
;; "C-3" - calls `lispy-right' (exit current list forwards)
;; ")" - calls `lispy-right-nostring' (exit current list
;;       forwards, but self-insert in strings and comments)
;;
;; These are the few Lispy commands that don't care whether the point
;; is special or not.  Other such bindings are `DEL', `C-d', `C-k'.
;;
;; To get out of the special position, you can use any of the good-old
;; navigational commands such as `C-f' or `C-n'.
;; Additionally `SPC' will break out of special to get around the
;; situation when you have the point between open parens like this
;; "(|(" and want to start inserting.  `SPC' will change the code to
;; this: "(| (".
;;
;; A lot of Lispy commands come in pairs: one reverses the other.
;; Some examples are:
;; |-----+--------------------------+------------+-------------------|
;; | key | command                  | key        | command           |
;; |-----+--------------------------+------------+-------------------|
;; | j   | `lispy-down'             | k          | `lispy-up'        |
;; | s   | `lispy-move-down'        | w          | `lispy-move-up'   |
;; | >   | `lispy-slurp'            | <          | `lispy-barf'      |
;; | c   | `lispy-clone'            | C-d or DEL |                   |
;; | C   | `lispy-convolute'        | C          | reverses itself   |
;; | d   | `lispy-other'        | d          | reverses itself   |
;; | M-j | `lispy-split'            | +          | `lispy-join'      |
;; | O   | `lispy-oneline'          | M          | `lispy-multiline' |
;; | S   | `lispy-stringify'        | C-u "      | `lispy-quotes'    |
;; | ;   | `lispy-comment'          | C-u ;      | `lispy-comment'   |
;; | xi  | `lispy-to-ifs'           | xc         | `lispy-to-cond'   |
;; | F   | `lispy-follow'           | D          | `pop-tag-mark'    |
;; |-----+--------------------------+------------+-------------------|
;;
;; Here's a list of commands for inserting pairs:
;; |-----+------------------------------------|
;; | key | command                            |
;; |-----+------------------------------------|
;; |  (  | `lispy-parens'                     |
;; |  {  | `lispy-braces'                     |
;; |  }  | `lispy-brackets'                   |
;; |  "  | `lispy-quotes'                     |
;; |-----+------------------------------------|
;;
;; Here's a list of modified insertion commands that handle whitespace
;; in addition to self-inserting:
;; |-----+------------------------------------|
;; | key | command                            |
;; |-----+------------------------------------|
;; | SPC | `lispy-space'                      |
;; |  :  | `lispy-colon'                      |
;; |  ^  | `lispy-hat'                        |
;; |  '  | `lispy-tick'                       |
;; |  `  | `lispy-backtick'                   |
;; | C-m | `lispy-newline-and-indent'         |
;; |-----+------------------------------------|
;;
;; You can see the full list of bound commands with "F1 f lispy-mode".
;;
;; Most special commands will leave the point special after they're
;; done.  This allows to chain them as well as apply them continuously
;; by holding the key.  Some useful holdable keys are "jkf<>cws;".
;; Not so useful, but fun is "/": start it from "|(" position and hold
;; until all your Lisp code is turned into Python :).
;;
;; Some Clojure support depends on `cider'.
;; Some Scheme support depends on `geiser'.
;; Some Common Lisp support depends on `slime' or `sly'.
;; You can get them from MELPA.
;;
;; See http://abo-abo.github.io/lispy/ for a detailed documentation.
;;
;;; Code:

;;* Requires
(eval-when-compile
  (require 'eldoc)
  (require 'ediff)
  (require 'ediff-util))
(require 'mode-local)
(require 'help-fns)
(require 'outline)
(require 'avy)
(require 'newcomment)
(require 'delsel)
(require 'swiper)
(require 'pcase)
(require 'hydra)

;;* Features for suported languages

(defvar lispy--eval-last-sexp-handlers-alist
      '((:emacs-lisp . (lambda () (call-interactively 'eval-last-sexp)))
        (:clojure . lispy--clojure-eval-last-sexp)))

(defvar lispy--eval-defun-handlers-alist
      '((:emacs-lisp . (lambda ()
                         (call-interactively 'eval-defun)))
        (:clojure . lispy--clojure-eval-defun)))

(defvar lispy--eval-region-handlers-alist
      '((:emacs-lisp . (lambda ()
                         (call-interactively 'eval-region)))
        (:clojure . lispy--clojure-eval-region)))

(setq lispy--describe-handlers-alist
  '((:emacs-lisp . lispy--emacs-lisp-describe-symbol)
    (:clojure . lispy--clojure-describe-symbol)))

(defun lispy--lang ()
  "Return the language that is being used, based on the current major and minor mode."
  (cond
   ((or (derived-mode-p 'emacs-lisp-mode)
        (memq major-mode lispy-elisp-modes))
    :emacs-lisp)
   ((or (derived-mode-p 'clojure-mode)
        (memq major-mode lispy-clojure-modes))
    :clojure)))

(defun lispy--clojure-eval-last-sexp ()
  (cond ((bound-and-true-p inf-clojure-minor-mode)
         (call-interactively 'inf-clojure-eval-last-sexp))
        ((bound-and-true-p cider-mode)
         (call-interactively 'cider-eval-last-sexp))))

(defun lispy--clojure-eval-region ()
  (cond ((bound-and-true-p inf-clojure-minor-mode)
         (call-interactively 'inf-clojure-eval-region))
        ((bound-and-true-p cider-mode)
         (call-interactively 'cider-eval-region))))

(defun lispy--clojure-eval-defun ()
  (cond ((bound-and-true-p inf-clojure-minor-mode)
         (call-interactively 'inf-clojure-eval-defun))
        ((bound-and-true-p cider-mode)
         (call-interactively 'cider-eval-defun-at-point))))

(defsubst lispy-looking-back (regexp)
  "Forward to (`looking-back' REGEXP)."
  (looking-back regexp (line-beginning-position)))

(defvar lispy-elisp-modes
  '(emacs-lisp-mode
    lisp-interaction-mode
    minibuffer-inactive-mode)
  "Modes for which emacs-lisp related functions are appropriate.")

(defvar lispy-clojure-modes
  '(clojure-mode
    clojurescript-mode
    clojurec-mode)
  "Modes for which clojure related functions are appropriate.")

(defvar lispy-map-input-overlay nil
  "The input overlay for mapping transformations.")

;; TODO: Should this be suspect to comment-char handling as well?
(defvar-local lispy-outline-header ";;"
  "Store the buffer-local outline start.")

;;* Customization
(defgroup lispy nil
  "List navigation and editing for the Lisp family."
  :group 'bindings
  :prefix "lispy-")

(defvar lispy-left "[([{]"
  "Opening delimiter.")

(defvar lispy-right "[])}]"
  "Closing delimiter.")

(defvar lispy-outline "^;;\\(?:;[^#]\\|\\*+\\)"
  "Outline delimiter.")

(defcustom lispy-no-space nil
  "When non-nil, don't insert a space before parens/brackets/braces/colons."
  :type 'boolean
  :group 'lispy)
(make-variable-buffer-local 'lispy-no-space)

(defcustom lispy-lax-eval t
  "When non-nil, fix \"unbound variable\" error by setting the it to nil.
This is useful when hacking functions with &optional arguments.
So evaling (setq mode (or mode major-mode)) will set mode to nil on
the first eval, and to major-mode on the second eval."
  :type 'boolean
  :group 'lispy)

(defcustom lispy-verbose t
  "If t, lispy will display some messages on error state.
These messages are similar to \"Beginning of buffer\" error for
`backward-char' and can safely be ignored."
  :type 'boolean
  :group 'lispy)

(defcustom lispy-verbose-verbs t
  "If t, verbs produced by `lispy-defverb' will have a hint in the echo area.
The hint will consist of the possible nouns that apply to the verb."
  :type 'boolean
  :group 'lispy)

(defcustom lispy-close-quotes-at-end-p nil
  "If t, when pressing the `\"' at the end of a quoted string, it will move you past the end quote."
  :type 'boolean
  :group 'lispy)

(defcustom lispy-visit-method 'ffip
  "Method to switch to a file in the current project."
  :type '(choice
          (const :tag "Find File in Project" ffip)
          (const :tag "Projectile" projectile)))

(defcustom lispy-avy-style-char 'pre
  "Method of displaying the overlays for a char during visual selection."
  :type '(choice
          (const :tag "Pre" pre)
          (const :tag "At" at)
          (const :tag "At full" at-full)
          (const :tag "Post" post)))

(defcustom lispy-avy-style-paren 'at
  "Method of displaying the overlays for a paren during visual selection."
  :type '(choice
          (const :tag "Pre" pre)
          (const :tag "At" at)
          (const :tag "At full" at-full)
          (const :tag "Post" post)))

(defcustom lispy-avy-style-symbol 'pre
  "Method of displaying the overlays for a symbol during visual selection."
  :type '(choice
          (const :tag "Pre" pre)
          (const :tag "At" at)
          (const :tag "At full" at-full)
          (const :tag "Post" post)))

(defcustom lispy-avy-keys (number-sequence ?a ?z)
  "Keys for jumping."
  :type '(repeat :tag "Keys" (character :tag "char")))

(defface lispy-command-name-face
  '((((class color) (background light))
     :background "#d8d8f7" :inherit font-lock-function-name-face)
    (((class color) (background dark))
     :background "#333333" :inherit font-lock-function-name-face))
  "Face for Elisp commands."
  :group 'lispy-faces)

(defface lispy-cursor-face
  '((((class color) (background light))
     :background "#000000" :foreground "#ffffff")
    (((class color) (background dark))
     :background "#ffffff" :foreground "#000000"))
  "Face for `lispy-view-test'."
  :group 'lispy-faces)

(defface lispy-test-face
  '((t (:inherit lispy-face-hint)))
  "Face for `lispy-view-test'."
  :group 'lispy-faces)

(defvar lispy-mode-map (make-sparse-keymap))

(defvar lispy-known-verbs nil
  "List of registered verbs.")

(defvar lispy-ignore-whitespace nil
  "When set to t, function `lispy-right' will not clean up whitespace.")

(defcustom lispy-compat '()
  "List of package compatibility options.
Enabling them adds overhead, so make sure that you are actually
using those packages."
  :type '(repeat
          (choice
           (const :tag "god-mode" god-mode)
           (const :tag "magit-blame-mode" magit-blame-mode)
           (const :tag "cider" cider)
           (const :tag "macrostep" macrostep))))

(defvar-local lispy-old-outline-settings nil
  "Store the old values of `outline-regexp' and `outline-level'.
`lispy-mode' overrides those while it's on.")

(defcustom lispy-safe-delete nil
  "When non-nil, killing/deleting an active region keeps delimiters balanced.
This applies to `lispy-delete', `lispy-kill-at-point', `lispy-paste', and
`lispy-delete-backward'. This also applies to `lispy-yank' when
`delete-selection-mode' is non-nil."
  :group 'lispy
  :type 'boolean)

(defcustom lispy-safe-copy nil
  "When non-nil, `lispy-new-copy' won't copy unbalanced delimiters in a region."
  :group 'lispy
  :type 'boolean)

(defcustom lispy-safe-paste nil
  "When non-nil, `lispy-paste' and `lispy-yank' will add missing delimiters."
  :group 'lispy
  :type 'boolean)

(defcustom lispy-safe-threshold 1500
  "The max size of an active region that lispy will try to keep balanced.
This only applies when `lispy-safe-delete', `lispy-safe-copy', and/or
`lispy-safe-paste' are non-nil."
  :group 'lispy
  :type 'number)

(defcustom lispy-safe-actions-ignore-strings t
  "When non-nil, don't try to act safely in strings.
Any unmatched delimiters inside of strings will be copied or deleted. This only
applies when `lispy-safe-delete', `lispy-safe-copy', and/or `lispy-safe-paste'
are non-nil."
  :group 'lispy
  :type 'boolean)

(defcustom lispy-safe-actions-ignore-comments t
  "When non-nil, don't try to act safely in comments.
Any unmatched delimiters inside of comments will be copied or deleted. This only
applies when `lispy-safe-delete', `lispy-safe-copy', and/or `lispy-safe-paste'
are non-nil."
  :group 'lispy
  :type 'boolean)

(defcustom lispy-safe-actions-no-pull-delimiters-into-comments nil
  "When non-nil, don't pull unmatched delimiters into comments when deleting.
This prevents the accidental unbalancing of expressions from commenting out
delimiters. This only applies when `lispy-safe-delete', `lispy-safe-copy',
and/or `lispy-safe-paste' are non-nil."
  :group 'lispy
  :type 'boolean)

(defcustom lispy-insert-space-after-wrap t
  "When non-nil, insert a space after the point when wrapping.
This applies to the commands that use `lispy-pair'."
  :group 'lispy
  :type 'boolean)

(defcustom lispy-thread-last-macro "thread-last"
  "Threading macro to use by default in command `lispy-thread-last'."
  :type '(radio
          (const :tag "Elisp" "thread-last")
          (const :tag "Clojure" "->>")
          (string :tag "Custom")))

(defun lispy-comment-char (&optional level postfix)
  "Get the `comment-start' character, or `;' if nil, repeated LEVEL times concated with POSTFIX."
  (concat
   (apply #'concat (make-list (or level 1) (or comment-start ";")))
   (or postfix "")))

;;;###autoload
(define-minor-mode lispy-mode
  "Minor mode for navigating and editing LISP dialects.

When `lispy-mode' is on, most unprefixed keys,
i.e. [a-zA-Z+-./<>], conditionally call commands instead of
self-inserting. The condition (called special further on) is one
of:

- the point is before \"(\"
- the point is after \")\"
- the region is active

For instance, when special, \"j\" moves down one sexp, otherwise
it inserts itself.

When special, [0-9] call `digit-argument'.

When `lispy-mode' is on, \"[\" and \"]\" move forward and
backward through lists, which is useful to move into special.

\\{lispy-mode-map}"
  :keymap lispy-mode-map
  :group 'lispy
  :lighter " LY"
  (if lispy-mode
      (progn
        (require 'eldoc)
        (eldoc-remove-command 'special-lispy-eval)
        (eldoc-remove-command 'special-lispy-x)
        (eldoc-add-command 'lispy-space)
        (setq lispy-old-outline-settings
              (cons outline-regexp outline-level))
        (setq-local outline-level 'lispy-outline-level)
        (cond ((eq major-mode 'latex-mode)
               (setq-local lispy-outline "^\\(?:%\\*+\\|\\\\\\(?:sub\\)?section{\\)")
               (setq lispy-outline-header "%")
               (setq-local outline-regexp "\\(?:%\\*+\\|\\\\\\(?:sub\\)?section{\\)"))
              ((eq major-mode 'python-mode)
               (setq-local lispy-outline "^#\\*+")
               (setq lispy-outline-header "#")
               (setq-local outline-regexp "#\\*+")
               (setq-local outline-heading-end-regexp "\n"))
              (t
               (setq-local outline-regexp (substring lispy-outline 1))))
        (when (called-interactively-p 'any)
          (mapc #'lispy-raise-minor-mode
                (cons 'lispy-mode lispy-known-verbs))))
    (when lispy-old-outline-settings
      (setq outline-regexp (car lispy-old-outline-settings))
      (setq outline-level (cdr lispy-old-outline-settings))
      (setq lispy-old-outline-settings nil))))

(defun lispy-raise-minor-mode (mode)
  "Make MODE the first on `minor-mode-map-alist'."
  (let ((x (assq mode minor-mode-map-alist)))
    (when x
      (setq minor-mode-map-alist
            (cons x (delq mode minor-mode-map-alist))))))

;;* Macros
(defmacro lispy-dotimes (n &rest bodyform)
  "Execute N times the BODYFORM unless an error is signaled.
Return nil if couldn't execute BODYFORM at least once.
Otherwise return the amount of times executed."
  (declare (indent 1)
           (debug (form body)))
  `(let ((i 0))
     (catch 'result
       (condition-case e
           (progn
             (while (<= (cl-incf i) ,n)
               ,@bodyform)
             ,n)
         (error
          (when (eq (car e) 'buffer-read-only)
            (message "Buffer is read-only: %s" (current-buffer)))
          (cl-decf i)
          (and (> i 0) i))))))

(defmacro lispy-save-excursion (&rest body)
  "More intuitive (`save-excursion' BODY)."
  (declare (indent 0))
  `(let ((out (save-excursion
                ,@body)))
     (when (lispy-bolp)
       (back-to-indentation))
     out))

(defmacro lispy-from-left (&rest body)
  "Ensure that BODY is executed from start of list."
  (declare (debug (body)))
  (let ((at-start (cl-gensym "at-start")))
    `(let ((,at-start (lispy--leftp)))
       (unless ,at-start
         (lispy-other))
       (unwind-protect
            (lispy-save-excursion
              ,@body)
         (unless (eq ,at-start (lispy--leftp))
           (lispy-other))))))

(defmacro lispy-flet (binding &rest body)
  "Temporarily override BINDING and execute BODY."
  (declare (indent 1))
  (let* ((name (car binding))
         (old (cl-gensym (symbol-name name))))
    `(let ((,old (symbol-function ',name)))
       (unwind-protect
            (progn
              (fset ',name (lambda ,@(cdr binding)))
              ,@body)
         (fset ',name ,old)))))

(defmacro lispy-multipop (lst n)
  "Remove LST's first N elements and return them."
  `(if (<= (length ,lst) ,n)
       (prog1 ,lst
         (setq ,lst nil))
     (prog1 ,lst
       (setcdr
        (nthcdr (1- ,n) (prog1 ,lst (setq ,lst (nthcdr ,n ,lst))))
        nil))))

;;* Verb related
(defun lispy-disable-verbs-except (verb)
  "Disable all verbs except VERB."
  (mapc
   (lambda (v) (funcall v -1))
   (remq verb lispy-known-verbs)))

(defun lispy-quit ()
  "Remove modifiers."
  (interactive)
  (lispy-disable-verbs-except nil))

(defmacro lispy-defverb (name grammar)
  "Define the verb NAME.
GRAMMAR is a list of nouns that work with this verb."
  (let* ((sym (intern (format "lispy-%s-mode" name)))
         (keymap (intern (format "lispy-%s-mode-map" name)))
         (doc (format "%s verb.\n\n \\{lispy-%s-mode-map}"
                      (capitalize name) name))
         (lighter (format " [%s]" name))
         (verb (intern (format "lispy-%s-verb" name)))
         (msg (format "[%s]: %s" name
                      (mapconcat #'car grammar " "))))
    `(progn
       (defvar ,sym nil
         ,(format "Non-nil if Lispy-%s mode is enabled.
Use the command `%s' to change this variable."
                  (capitalize name)
                  sym))
       (make-variable-buffer-local ',sym)
       (defvar ,keymap (make-sparse-keymap))
       (defun ,sym (&optional arg)
         ,doc
         (interactive (list (or current-prefix-arg 'toggle)))
         (let ((last-message (current-message)))
           (setq ,sym (if (eq arg 'toggle)
                          (not ,sym)
                        (> (prefix-numeric-value arg)
                           0)))
           (cond (,sym (lispy-disable-verbs-except ',sym))
                 (t nil))
           (if (called-interactively-p 'any)
               (unless (and (current-message)
                            (not (equal last-message (current-message))))
                 (if ,sym
                     (when lispy-verbose-verbs
                       (message ,msg))
                   (message "")))))
         (force-mode-line-update))
       (mapc (lambda (x)
               (lispy-define-key
                   ,keymap
                   (car x) (cadr x)
                 :disable ',sym))
             ',grammar)
       (unless (memq ',sym lispy-known-verbs)
         (push ',sym lispy-known-verbs))
       (defun ,verb ()
         (interactive)
         (if (bound-and-true-p ,sym)
             (,sym -1)
           (,sym 1)))
       (with-no-warnings
         (add-minor-mode ',sym ,lighter ,keymap nil nil)))))

;;* Globals: navigation
(defsubst lispy-right-p ()
  "Return t if after variable `lispy-right'."
  (looking-back lispy-right
                (line-beginning-position)))

(defsubst lispy-left-p ()
  "Return t if before variable `lispy-left'."
  (looking-at lispy-left))

(defun lispy-forward (arg)
  "Move forward list ARG times or until error.
Return t if moved at least once,
otherwise call function `lispy-right' and return nil."
  (interactive "p")
  (when (= arg 0)
    (setq arg 2000))
  (lispy--exit-string)
  (let ((bnd (lispy--bounds-comment)))
    (when bnd
      (goto-char (1+ (cdr bnd)))))
  (let ((pt (point))
        (r (lispy-dotimes arg
             (when (= (point) (point-max))
               (error "Reached end of buffer"))
             (forward-list))))
    ;; `forward-list' returns true at and of buffer
    (if (or (null r)
            (= pt (point))
            (and (not (lispy-right-p))
                 (progn
                   (backward-list)
                   (forward-list)
                   (= pt (point)))))
        (prog1 nil
          (lispy--out-forward 1))
      (point))))

(defun lispy-backward (arg)
  "Move backward list ARG times or until error.
If couldn't move backward at least once, move up backward and return nil."
  (interactive "p")
  (when (= arg 0)
    (setq arg 2000))
  (lispy--exit-string)
  (let ((bnd (lispy--bounds-comment)))
    (when bnd
      (goto-char (car bnd))))
  (let ((pt (point))
        (r (lispy-dotimes arg
             (when (= (point) (point-min))
               (error "Reached beginning of buffer"))
             (backward-list))))
    ;; `backward-list' returns true at beginning of buffer
    (if (or (null r)
            (= pt (point))
            (and (not (lispy-left-p))
                 (progn
                   (forward-list)
                   (backward-list)
                   (= pt (point)))))
        (prog1 nil
          (condition-case nil
              (progn
                (lispy--out-forward 1)
                (backward-list))
            (error
             (progn
               (goto-char pt)
               (up-list -1)))))
      (point))))

(defun lispy-right (arg)
  "Move outside list forwards ARG times.
Return nil on failure, t otherwise."
  (interactive "p")
  (lispy--remember)
  (when (bound-and-true-p abbrev-mode)
    (ignore-errors (expand-abbrev)))
  (cond ((region-active-p)
         (lispy-mark-right arg))
        ((looking-at lispy-outline)
         (lispy-outline-right))
        (t
         (lispy--out-forward arg))))

(defun lispy-right-nostring (arg)
  "Call `lispy--out-forward' with ARG unless in string or comment.
Self-insert otherwise."
  (interactive "p")
  (if (or (lispy--in-string-or-comment-p)
          (looking-back "?\\\\"
                        (line-beginning-position)))
      (self-insert-command arg)
    (lispy--out-forward arg)))

(defun lispy-left (arg)
  "Move outside list forwards ARG times.
Return nil on failure, t otherwise."
  (interactive "p")
  (lispy--remember)
  (cond ((region-active-p)
         (lispy-mark-left arg))
        ((looking-at lispy-outline)
         (lispy-outline-left))
        (t
         (or (lispy--out-backward arg)
             (ignore-errors
               (up-list -1))))))

(defun lispy-out-forward-newline (arg)
  "Call `lispy--out-forward', then ARG times `newline-and-indent'."
  (interactive "p")
  (lispy--out-forward 1)
  (lispy-dotimes arg
    (newline-and-indent)))

(defvar lispy-meol-point 1
  "Point where `lispy-move-end-of-line' should go when already at eol.")

(defun lispy-move-end-of-line ()
  "Forward to `move-end-of-line' unless already at end of line.
Then return to the point where it was called last.
If this point is inside string, move outside string."
  (interactive)
  (let ((pt (point))
        bnd)
    (if (eq pt (line-end-position))
        (if (setq bnd (lispy--bounds-string))
            (goto-char (cdr bnd))
          (when (and (< lispy-meol-point pt)
                     (>= lispy-meol-point (line-beginning-position)))
            (goto-char lispy-meol-point)
            (when (setq bnd (lispy--bounds-string))
              (goto-char (cdr bnd)))))
      (setq lispy-meol-point (point))
      (move-end-of-line 1))))

(defun lispy-move-beginning-of-line ()
  "Forward to `move-beginning-of-line'.
Reveal outlines."
  (interactive)
  (lispy--ensure-visible)
  (if (bolp)
      (back-to-indentation)
    (move-beginning-of-line 1)))

(defun lispy--re-search-in-code (regexp direction &optional count)
  "Move to the next REGEXP in DIRECTION, COUNT times.
DIRECTION is either 'forward or 'backward.
Return the amount of successful moves, or nil otherwise."
  (setq count (or count 1))
  (let ((to-move (abs count))
        (advancer
         (if (eq direction 'forward)
             (if (> count 0)
                 #'re-search-forward
               #'re-search-backward)
           (if (> count 0)
               #'re-search-backward
             #'re-search-forward)))
        (pt (point)))
    (if (and (eq direction 'forward) (> count 0))
        (when (looking-at regexp)
          (goto-char (match-end 0))))
    (while (and (> to-move 0)
                (funcall advancer regexp nil t))
      (unless (lispy--in-string-or-comment-p)
        (cl-decf to-move)))
    (if (= to-move (abs count))
        (progn
          (goto-char pt)
          nil)
      (if (eq direction 'forward)
          (goto-char (match-beginning 0)))
      (- count to-move))))

;;* Locals: navigation
(defun lispy-flow (arg)
  "Move inside list ARG times.
Don't enter strings or comments.
Return nil if can't move."
  (interactive "p")
  (lispy--remember)
  (let ((pt (point))
        r)
    (cond
      ((and (lispy-bolp)
            (looking-at (lispy-comment-char)))
       (setq r (lispy--re-search-in-code lispy-left 'forward arg)))
      ((lispy-left-p)
       (setq r (lispy--re-search-in-code lispy-left 'forward arg)))
      ((lispy-right-p)
       (backward-char)
       (when (setq r (lispy--re-search-in-code lispy-right 'backward arg))
         (forward-char))))
    (or r
        (progn
          (goto-char pt)
          nil))))

(defun lispy-down (arg)
  "Move down ARG times inside current list."
  (interactive "p")
  (lispy--remember)
  (cond ((region-active-p)
         (let ((leftp (= (point) (region-beginning))))
           (when leftp
             (exchange-point-and-mark))
           (cond ((save-excursion
                    (skip-chars-forward " \n")
                    (eobp)))
                 ((lispy--symbolp (lispy--string-dwim))
                  (lispy-dotimes arg
                    (when (lispy-slurp 1)
                      (lispy-other)
                      (lispy-barf 1)
                      (lispy-other))))

                 ((looking-at "[\n ]+\\(;\\)")
                  (deactivate-mark)
                  (goto-char (match-beginning 1))
                  (lispy--mark (lispy--bounds-comment)))

                 (t
                  (lispy-dotimes arg
                    (forward-sexp 1)
                    (lispy-other)
                    (if (lispy--in-comment-p)
                        (progn
                          (goto-char (1+ (cdr (lispy--bounds-comment))))
                          (skip-chars-forward "\n"))
                      (forward-sexp 2)
                      (forward-sexp -1))
                    (lispy-other))))
           (when leftp
             (exchange-point-and-mark))))

        ((lispy-left-p)
         (lispy-forward arg)
         (let ((pt (point))
               (lispy-ignore-whitespace t))
           (if (lispy-forward 1)
               (lispy-backward 1)
             (goto-char pt)
             (lispy-other))))

        ((lispy-right-p)
         (let ((pt (point)))
           (unless (lispy-forward arg)
             (goto-char pt))))

        ((or (looking-at lispy-outline)
             (and (bolp) (looking-at (lispy-comment-char))))
         (let ((pt (point))
               (outline-regexp lispy-outline))
           (lispy-dotimes arg
             (outline-next-visible-heading 1)
             (if (looking-at lispy-outline)
                 (setq pt (point))
               (goto-char pt)
               (error "Last outline reached")))))

        (t
         (lispy-forward 1)
         (lispy-backward 1)))
  (lispy--ensure-visible))

(defun lispy-up (arg)
  "Move up ARG times inside current list."
  (interactive "p")
  (lispy--remember)
  (cond ((region-active-p)
         (let ((leftp (= (point) (region-beginning))))
           (unless leftp
             (exchange-point-and-mark))
           (cond ((save-excursion
                    (skip-chars-backward "\n ")
                    (bobp)))
                 ((looking-back "^ *\\(;\\)[^\n]*[\n ]*"
                                (save-excursion
                                  (ignore-errors
                                    (backward-sexp 1))
                                  (point)))
                  (deactivate-mark)
                  (goto-char (match-beginning 1))
                  (lispy--mark (lispy--bounds-comment))
                  (exchange-point-and-mark))
                 ((lispy--symbolp (lispy--string-dwim))
                  (lispy-dotimes arg
                    (when (lispy-slurp 1)
                      (lispy-other)
                      (lispy-barf 1)
                      (lispy-other))))
                 (t
                  (lispy-dotimes arg
                    (backward-sexp 1)
                    (lispy-other)
                    (if (lispy--in-comment-p)
                        (progn
                          (goto-char (1- (car (lispy--bounds-comment))))
                          (skip-chars-backward "\n"))
                      (backward-sexp 2)
                      (backward-sexp -1))
                    (lispy-other))))
           (unless leftp
             (exchange-point-and-mark))))

        ((lispy-left-p)
         (let ((pt (point)))
           (unless (lispy-backward arg)
             (goto-char pt))))

        ((lispy-right-p)
         (lispy-backward arg)
         (let ((pt (point)))
           (if (lispy-backward 1)
               (lispy-forward 1)
             (goto-char pt)
             (lispy-other))))

        ((or (looking-at lispy-outline)
             (and (bolp) (looking-at (lispy-comment-char))))
         (let ((pt (point))
               (outline-regexp lispy-outline))
           (lispy-dotimes arg
             (outline-previous-visible-heading 1)
             (if (looking-at lispy-outline)
                 (setq pt (point))
               (goto-char pt)
               (error "First outline reached")))))
        (t
         (lispy-backward 1)
         (lispy-forward 1)))
  (lispy--ensure-visible))

(defvar lispy-pos-ring (make-ring 100)
  "Ring for point/mark position and restriction history.")

(defun lispy--remember ()
  "Store the current point and mark in history."
  (let* ((emptyp (zerop (ring-length lispy-pos-ring)))
         (top (unless emptyp
                (ring-ref lispy-pos-ring 0)))
         (restriction (when (buffer-narrowed-p)
                        (cons (set-marker (make-marker)
                                          (point-min))
                              (set-marker (make-marker)
                                          (point-max))))))
    (if (region-active-p)
        (let* ((bnd (lispy--bounds-dwim))
               (bnd (cons
                     (move-marker (make-marker) (car bnd))
                     (move-marker (make-marker) (cdr bnd)))))
          (when (or emptyp
                    (not (equal bnd top)))
            (ring-insert lispy-pos-ring (list bnd restriction))))
      (when (or emptyp
                (not (equal (point-marker) top)))
        (ring-insert lispy-pos-ring (list (point-marker) restriction))))))

(defvar lispy-back-restore-restriction t
  "When non-nil, restore buffer restriction on `lispy-back'.")

(defun lispy-back (arg)
  "Move point to ARGth previous position.
If position isn't special, move to previous or error."
  (interactive "p")
  (when (buffer-narrowed-p)
    (widen))
  (lispy-dotimes arg
    (if (zerop (ring-length lispy-pos-ring))
        (lispy--complain "At beginning of point history")
      (let* ((data (ring-remove lispy-pos-ring 0))
             (marker (pop data))
             (restriction (pop data))
             (beg (car restriction))
             (end (cdr restriction)))
        ;; After deleting some text, markers that point to it converge
        ;; to one point
        (while (and (not (zerop (ring-length lispy-pos-ring)))
                    (equal (ring-ref lispy-pos-ring 0)
                           marker))
          (ring-remove lispy-pos-ring 0))
        (if (consp marker)
            (lispy--mark marker)
          (deactivate-mark)
          (switch-to-buffer (marker-buffer marker))
          (goto-char marker))
        (when (and lispy-back-restore-restriction
                   restriction)
          (narrow-to-region beg end)
          (set-marker beg nil)
          (set-marker end nil))))))

(defun lispy-knight-down ()
  "Make a knight-like move: down and right."
  (interactive)
  (cond ((lispy-right-p)
         (lispy-other))
        ((lispy-left-p))
        (t (lispy-backward 1)))
  (let ((pt (point))
        (bnd (save-excursion
               (lispy-beginning-of-defun)
               (lispy--bounds-list))))
    (catch 'done
      (while t
        (forward-line)
        (cond ((>= (point) (cdr bnd))
               (goto-char pt)
               (throw 'done nil))
              ((looking-at (concat "\\s-*" lispy-left))
               (goto-char (1- (match-end 0)))
               (throw 'done t)))))))

(defun lispy-knight-up ()
  "Make a knight-like move: up and right."
  (interactive)
  (cond ((lispy-right-p)
         (lispy-other))
        ((lispy-left-p))
        (t (lispy-backward 1)))
  (let ((pt (point))
        (bnd (save-excursion
               (lispy-beginning-of-defun)
               (lispy--bounds-list))))
    (catch 'done
      (while t
        (beginning-of-line 0)
        (cond ((< (point) (car bnd))
               (goto-char pt)
               (throw 'done nil))
              ((looking-at (concat "\\s-*" lispy-left))
               (goto-char (1- (match-end 0)))
               (throw 'done t)))))))

(defun lispy-other ()
  "Switch to the different side of current sexp."
  (interactive)
  (cond ((and (region-active-p)
              (not (= (region-beginning) (region-end))))
         (exchange-point-and-mark))
        ((lispy-right-p)
         (backward-list))
        ((lispy-left-p)
         (forward-list))
        (t
         (user-error "Unexpected"))))

;;* Globals: kill, yank, delete, mark, copy
(defun lispy-kill ()
  "Kill line, keeping parens consistent."
  (interactive)
  (let (bnd)
    (cond ((or (lispy--in-comment-p)
               (and (looking-at " *;")
                    (save-excursion
                      (goto-char (match-end 0))
                      (lispy--in-comment-p))))
           (kill-line))

          ((and (setq bnd (lispy--bounds-string))
                (or
                 (not (eq (point) (car bnd)))
                 (> (count-lines (car bnd) (cdr bnd)) 1)))
           (if (> (cdr bnd) (line-end-position))
               (if (eq (point) (car bnd))
                   (kill-region (car bnd) (cdr bnd))
                 (kill-line))
             (kill-region (point) (1- (cdr bnd)))))
          ((looking-at " *\n")
           (kill-region
            (match-beginning 0)
            (match-end 0))
           (lispy--indent-for-tab))
          ((and (looking-at lispy-right) (looking-back lispy-left
                                                       (line-beginning-position)))
           (delete-char 1)
           (backward-delete-char 1))
          ((lispy-left-p)
           (if (progn
                 (setq bnd (lispy--bounds-list))
                 (> (count-lines (car bnd) (cdr bnd)) 1))
               (kill-region (car bnd)
                            (cdr bnd))
             (narrow-to-region (car bnd) (line-end-position))
             (let ((pt (point)))
               (while (and (ignore-errors
                             (forward-list))
                           (> (point) pt))
                 (setq pt (point)))
               (when (looking-at "[\t ]*;[^\n]*$")
                 (setq pt (match-end 0)))
               (goto-char (point-min))
               (widen)
               (kill-region (point) pt))))
          (t
           (let ((beg (point))
                 (end (line-end-position))
                 bnd)
             (while (and (< (point) end)
                         (ignore-errors
                           (forward-sexp 1)
                           (skip-chars-forward " ")
                           t))
               (when (setq bnd (lispy--bounds-comment))
                 (goto-char (cdr bnd))))
             (skip-chars-forward " \t")
             (kill-region beg (point)))))))

(defun lispy-kill-word (arg)
  "Kill ARG words, keeping parens consistent."
  (interactive "p")
  (if (< arg 0)
      (lispy-backward-kill-word (- arg))
    (let (bnd)
      (lispy-dotimes arg
        (while (not (or (eobp)
                        (memq (char-syntax (char-after))
                              '(?w ?_))))
          (forward-char 1))
        (when (or (lispy-looking-back (concat lispy-left " +"))
                  (lispy-looking-back (lispy-comment-char 1 " +")))
          (delete-horizontal-space))
        (if (setq bnd (lispy--bounds-string))
            (save-restriction
              (narrow-to-region (1+ (car bnd)) (1- (cdr bnd)))
              (kill-word 1)
              (widen))
          (kill-word 1))))))

(defun lispy-backward-kill-word (arg)
  "Kill ARG words backward, keeping parens consistent."
  (interactive "p")
  (let (bnd
        (pt (point))
        (last-command (if (eq last-command 'lispy-backward-kill-word)
                          'kill-region
                        last-command)))
    (lispy-dotimes arg
      (when (lispy--in-comment-p)
        (skip-chars-backward " \n"))
      (if (memq (char-syntax (char-before))
                '(?w ?_ ?\s))
          (if (lispy-looking-back "\\_<\\s_+")
              (delete-region (match-beginning 0)
                             (match-end 0))
            (backward-kill-word 1)
            (when (and (lispy--in-string-p)
                       (not (lispy-looking-back "\\\\\\\\"))
                       (lispy-looking-back "\\\\"))
              (delete-char -1)))
        (delete-region (point) pt)
        (while (not (or (bobp)
                        (memq (char-syntax (char-before))
                              '(?w ?_))))
          (backward-char 1))
        (if (setq bnd (lispy--bounds-string))
            (progn
              (save-restriction
                (if (and (looking-at "\\s-+\"")
                         (eq (match-end 0) (cdr bnd)))
                    (goto-char (1- (cdr bnd)))
                  (when (and (> pt (car bnd))
                             (< pt (cdr bnd)))
                    (goto-char pt)))
                (narrow-to-region (1+ (car bnd)) (point))
                (kill-region (progn
                               (forward-word -1)
                               (when (and (not (lispy-looking-back "\\\\\\\\"))
                                          (lispy-looking-back "\\\\"))
                                 (backward-char))
                               (point))
                             (point-max))
                (widen)))
          (backward-kill-word 1))))))

(defun lispy-kill-sentence ()
  "Kill until the end of current string or list."
  (interactive)
  (let ((bnd (lispy--bounds-dwim)))
    (if (or (lispy-left-p) (looking-at "\""))
        (kill-region (car bnd) (cdr bnd))
      (setq bnd (or (lispy--bounds-string)
                    (lispy--bounds-list)))
      (kill-region (point) (1- (cdr bnd))))))

(defun lispy-yank ()
  "Like regular `yank', but quotes body when called from \"|\"."
  (interactive)
  (setq this-command 'yank)
  (let* ((text (lispy--maybe-safe-current-kill)))
    (cond
      ((and (region-active-p)
            (bound-and-true-p delete-selection-mode))
       (lispy--maybe-safe-delete-region (region-beginning) (region-end))
       (insert-for-yank text))
      ((and (eq (char-after) ?\")
            (eq (char-before) ?\"))
       (insert-for-yank (replace-regexp-in-string "\"" "\\\\\""
                                                  text)))
      (t
       (push-mark (point))
       (insert-for-yank text)))))

(defvar lispy-delete-sexp-from-within nil
  "When cursor is adjacent to an opening or closing pair,
`lispy-delete' or `lispy-delete-backward' toward the delimiter
will kill the whole sexp (string or list).")

(defun lispy-delete (arg)
  "Delete ARG sexps."
  (interactive "p")
  (let (bnd)
    (cond ((< arg 0)
           (lispy-delete-backward (- arg)))

          ((region-active-p)
           (lispy--maybe-safe-delete-region (region-beginning) (region-end)))

          ((setq bnd (lispy--bounds-string))
           (cond ((eq (1+ (point)) (cdr bnd))
                  (goto-char (car bnd))
                  (when lispy-delete-sexp-from-within
                    (lispy-delete arg)))
                 ((looking-at "\\\\\"")
                  (if (eq (+ (point) 2) (cdr bnd))
                      (goto-char (car bnd))
                    (delete-char 2)))
                 ((and (looking-at "\"")
                       (lispy-looking-back "\\\\"))
                  (backward-char 1)
                  (delete-char 2))
                 ((lispy--delete-pair-in-string "\\\\\\\\(" "\\\\\\\\)"))
                 ((looking-at "\\\\\\\\")
                  (delete-char 2))
                 ((and (looking-at "\\\\")
                       (lispy-looking-back "\\\\"))
                  (backward-char 1)
                  (delete-char 2))
                 ((eq (point) (car bnd))
                  (delete-region (car bnd)
                                 (cdr bnd))
                  (let ((pt (point)))
                    (skip-chars-forward " ")
                    (delete-region pt (point))))
                 ((save-excursion
                    (forward-char 1)
                    (lispy--in-string-or-comment-p))
                  (delete-char arg))
                 (t
                  (lispy--exit-string))))

          ((lispy--in-comment-p)
           (if (lispy-bolp)
               (let ((bnd (lispy--bounds-comment)))
                 (delete-region (car bnd) (cdr bnd)))
             (delete-char arg)))

          ((looking-at lispy-right)
           (lispy-left 1)
           (when lispy-delete-sexp-from-within
             (lispy-delete arg)))

          ((lispy-left-p)
           (lispy--delete-leading-garbage)
           (lispy-dotimes arg
             (lispy--delete)))

          ((eolp)
           (delete-char 1)
           (let ((pt (point)))
             (skip-chars-forward " ")
             (delete-region pt (point))
             (unless (or (eolp)
                         (bolp)
                         (lispy-bolp)
                         (eq (char-before) ?\ ))
               (insert " "))))

          (t
           (delete-char arg)))))

(defun lispy--delete-leading-garbage ()
  "Delete any syntax before an opening delimiter such as '.
Delete backwards to the closest whitespace char or opening delimiter or to the
beginning of the line."
  (let ((pt (point))
        (end (save-excursion (re-search-backward ")" nil t))))
    (re-search-backward
     (concat "[[:space:]]" "\\|" lispy-left "\\|" "^")
     end t)
    (goto-char (match-end 0))
    (delete-region (point) pt)))

(defun lispy--delete-whitespace-backward ()
  "Delete spaces backward."
  (let ((pt (point)))
    (skip-chars-backward " ")
    (delete-region (point) pt)))

(defvar lispy-delete-backward-recenter -20
  "When cursor is near top of screen when calling
  `lispy-delete-backward', recenter cursor with arg.")

(defun lispy-delete-backward (arg)
  "From \")|\", delete ARG sexps backwards.
Otherwise (`backward-delete-char-untabify' ARG)."
  (interactive "p")
  (let (bnd)
    (cond ((< arg 0)
           (lispy-delete (- arg)))

          ((use-region-p)
           (lispy--maybe-safe-delete-region (region-beginning)
                                            (region-end)))
          ((bobp))

          ((and (setq bnd (lispy--bounds-string))
                (not (eq (point) (car bnd))))
           (cond ((eq (- (point) (car bnd)) 1)
                  (goto-char (cdr bnd))
                  (if lispy-delete-sexp-from-within
                      (lispy-delete-backward arg)))
                 ((or (looking-back "\\\\\\\\(" (car bnd))
                      (looking-back "\\\\\\\\)" (car bnd)))
                  (let ((pt (point)))
                    (goto-char (match-beginning 0))
                    (unless (lispy--delete-pair-in-string
                             "\\\\\\\\(" "\\\\\\\\)")
                      (goto-char pt)
                      (backward-delete-char-untabify arg))))
                 ((looking-back "[^\\]\\\\[^\\]" (car bnd))
                  (backward-delete-char 2))
                 (t
                  (backward-delete-char-untabify arg))))

          ((looking-at lispy-outline)
           (if (lispy-looking-back (concat lispy-outline ".*\n"))
               (delete-region
                (match-beginning 0)
                (match-end 0))
             (delete-char -1)))

          ((lispy--in-comment-p)
           (cond ((lispy-looking-back "^ +")
                  (delete-region (max (1- (match-beginning 0))
                                      (point-min))
                                 (match-end 0))
                  (lispy--indent-for-tab))
                 ((and (looking-at "$") (lispy-looking-back (lispy-comment-char 1 " +")))
                  (let ((pt (point)))
                    (skip-chars-backward " ;")
                    (delete-region (point) pt)
                    (if (lispy-looking-back "^")
                        (lispy--indent-for-tab)
                      (let ((p (point)))
                        (lispy--out-forward 1)
                        (lispy--prettify-1)
                        (goto-char p)))))
                 (t
                  (backward-delete-char-untabify arg))))

          ((lispy-looking-back "\\\\.")
           (backward-delete-char-untabify arg))

          ((and (lispy-looking-back (concat lispy-right " "))
                (looking-at " *$"))
           (backward-delete-char-untabify arg))

          ((or (and (lispy-right-p)
                    (or (memq major-mode lispy-clojure-modes)
                        (not (lispy-looking-back "[\\?]."))))
               (and (lispy-looking-back (concat lispy-right " "))
                    (or (lispy-left-p) (looking-at "\""))))
           (let ((pt (point)))
             (lispy-backward arg)
             (unless (lispy-right-p)
               (lispy--skip-delimiter-preceding-syntax-backward))
             (skip-chars-backward " \t")
             (while (plist-get (text-properties-at (point)) 'read-only)
               (forward-char))
             (delete-region (point) pt)
             (unless (or (looking-at " ")
                         (lispy-bolp)
                         (and (lispy-right-p)
                              (not (or (lispy-left-p)
                                       (looking-at "\""))))
                         (lispy-looking-back lispy-left)
                         ;; REPL prompt, e.g. `ielm'
                         (lispy-after-string-p "> "))
               (just-one-space))
             (setq pt (point))
             (if (and
                  (not (lispy-bolp))
                  (not (lispy-left-p))
                  (progn
                    (skip-chars-backward " \t\n")
                    (lispy-right-p)))
                 (delete-region (point) pt)
               (goto-char pt)
               (lispy--indent-for-tab))))

          ((and (lispy-looking-back lispy-left)
                (not (lispy-looking-back "[\\?].")))
           (lispy--out-forward 1)
           (lispy-delete-backward 1))

          ((eq (char-before) ?\")
           (backward-char 1)
           (let ((bnd (lispy--bounds-string)))
             (delete-region (car bnd)
                            (cdr bnd))
             (lispy--delete-whitespace-backward)
             (unless (looking-at " ")
               (insert " "))
             (lispy--indent-for-tab)))

          ((and (lispy-after-string-p "\" ")
                (not (looking-at lispy-right)))
           (let ((pt (point)))
             (backward-char 2)
             (delete-region (car (lispy--bounds-string)) pt))
           (lispy--delete-whitespace-backward)
           (unless (lispy-looking-back lispy-left)
             (just-one-space))
           (lispy--indent-for-tab))

          ((lispy-bolp)
           (delete-region
            (line-beginning-position)
            (point))
           (unless (bobp)
             (if (and (not (eolp))
                      (save-excursion
                        (backward-char 1)
                        (lispy--in-comment-p)))
                 (progn
                   (backward-char 1)
                   (let ((bnd (lispy--bounds-comment)))
                     (delete-region (car bnd) (cdr bnd)))
                   (delete-char 1))
               (backward-delete-char 1)
               (unless (or (eolp)
                           (looking-at lispy-right)
                           (lispy-looking-back lispy-left))
                 (just-one-space)))
             (lispy--indent-for-tab)))

          ((lispy-looking-back "[^ ]  +")
           (delete-region (+ (match-beginning 0) 2) (point)))

          (t
           (backward-delete-char-untabify arg))))
  (when (and (buffer-file-name)
             (< (- (line-number-at-pos (point))
                   (line-number-at-pos (window-start)))
                5)
             lispy-delete-backward-recenter)
    (ignore-errors
      (recenter lispy-delete-backward-recenter)))
  (when (and (lispy-left-p)
             (not (lispy--in-string-or-comment-p)))
    (indent-sexp)))

(defun lispy-mark ()
  "Mark the quoted string or the list that includes the point.
Extend region when it's aleardy active."
  (interactive)
  (let ((bounds (or (lispy--bounds-comment)
                    (lispy--bounds-string)
                    (lispy--bounds-list))))
    (when bounds
      (lispy--mark bounds))))

(defun lispy-mark-list (arg)
  "Mark list from special position.
When ARG is more than 1, mark ARGth element."
  (interactive "p")
  (when (called-interactively-p 'interactive)
    (lispy--remember))
  (cond ((> arg 1)
         (lispy-mark-car)
         (lispy-down (1- arg)))
        ((= arg 0)
         (let ((bnd (lispy--bounds-dwim)))
           (lispy--mark
            (cons (+ (car bnd) (if (eq (char-after (car bnd)) ?\#) 2 1))
                  (1- (cdr bnd))))))
        ((region-active-p)
         (deactivate-mark)
         (if (lispy--in-comment-p)
             (progn
               (beginning-of-line)
               (skip-chars-forward " "))
           (skip-chars-forward ",@'`")))
        ((lispy-left-p)
         (lispy--mark
          (lispy--bounds-dwim)))
        ((lispy-right-p)
         (lispy--mark
          (lispy--bounds-dwim))
         (lispy-other))
        ((and (lispy-bolp) (looking-at (lispy-comment-char)))
         (lispy--mark (lispy--bounds-comment))))
  (setq this-command 'lispy-mark-list))

(defvar-local lispy-bind-var-in-progress nil)

(defun lispy-mark-symbol ()
  "Mark current symbol."
  (interactive)
  (let (bnd)
    (cond (lispy-bind-var-in-progress
           (lispy-map-done)
           (setq lispy-bind-var-in-progress nil)
           (forward-sexp 2)
           (lispy-mark-symbol))

          ((lispy--in-comment-p)
           (if (and (looking-at "\\(?:\\w\\|\\s_\\)*'")
                    (setq bnd (match-end 0))
                    (looking-back "`\\(?:\\w\\|\\s_\\)*"
                                  (line-beginning-position)))
               (progn
                 (goto-char (match-beginning 0))
                 (set-mark (point))
                 (goto-char bnd))
             (lispy--mark (lispy--bounds-comment))))

          ((and
            (not (region-active-p))
            (setq bnd (lispy--bounds-string))
            (= (1+ (point))
               (cdr bnd)))
           (lispy--mark bnd))

          ((and (lispy-after-string-p "\"")
                (not (lispy--in-string-or-comment-p)))
           (set-mark-command nil)
           (forward-sexp -1)
           (exchange-point-and-mark))

          ((looking-at " *[[({]")
           (if (and (lispy-looking-back "\\sw\\|\\s_")
                    (not (region-active-p)))
               (progn
                 (set-mark-command nil)
                 (forward-sexp -1)
                 (exchange-point-and-mark))
             (let ((pt (point)))
               (skip-chars-forward "(){}[] \"\n")
               (set-mark-command nil)
               (if (looking-at "\\sw\\|\\s_")
                   (forward-sexp)
                 (condition-case nil
                     (progn
                       (re-search-forward "[][(){} \n]")
                       (while (lispy--in-string-or-comment-p)
                         (re-search-forward "[() \n]"))
                       (backward-char 1))
                   (error
                    (message "No further symbols found")
                    (deactivate-mark)
                    (goto-char pt)))))))

          ((region-active-p)
           (let ((bnd (lispy--bounds-string)))
             (condition-case nil
                 (progn
                   (forward-sexp)
                   (when (and bnd (> (point) (cdr bnd)))
                     (goto-char (cdr bnd))
                     (error "`forward-sexp' went through string bounds")))
               (error
                (deactivate-mark)
                (re-search-forward "\\sw\\|\\s_")
                (forward-char -1)
                (set-mark-command nil)
                (forward-sexp)))))

          ((lispy-right-p)
           (skip-chars-backward "}]) \n")
           (set-mark-command nil)
           (re-search-backward "[][{}() \n]")
           (while (lispy--in-string-or-comment-p)
             (re-search-backward "[() \n]"))
           (forward-char 1))

          ((looking-at lispy-right)
           (lispy--mark
            (save-excursion
              (backward-char 1)
              (lispy--bounds-dwim))))

          (t
           (lispy--mark (lispy--bounds-dwim))))))

(defun lispy-kill-at-point ()
  "Kill the quoted string or the list that includes the point."
  (interactive)
  (cond ((region-active-p)
         (lispy--maybe-safe-kill-region (region-beginning)
                                        (region-end)))
        ((derived-mode-p 'text-mode)
         (let ((beg (save-excursion
                      (1+ (re-search-backward "[ \t\n]" nil t))))
               (end (save-excursion
                      (1- (re-search-forward "[ \t\n]" nil t)))))
           (kill-region beg end)))
        (t
         (let ((bounds (or (lispy--bounds-comment)
                           (lispy--bounds-string)
                           (lispy--bounds-list))))
           (if buffer-read-only
               (kill-new (buffer-substring
                          (car bounds) (cdr bounds)))
             (kill-region (car bounds) (cdr bounds)))))))

(defun lispy-new-copy ()
  "Copy marked region or sexp to kill ring."
  (interactive)
  (let ((str (if (region-active-p)
                 (lispy--maybe-safe-region (region-beginning)
                                           (region-end))
               (lispy--string-dwim))))
    (unless (equal str (ignore-errors
                         (current-kill 0)))
      (kill-new str))))

;;* Globals: pairs
(defvar lispy-parens-only-left-in-string-or-comment t
  "Whether \"(\" should insert only the left paren in strings and comments.")

(defun lispy-pair (left right preceding-syntax-alist)
  "Return (lambda (arg)(interactive \"P\")...) using LEFT RIGHT.
PRECEDING-SYNTAX-ALIST should be an alist of `major-mode' to a list of regexps.
The regexps correspond to valid syntax that can precede LEFT in each major mode.
When this function is called:
- with region active:
  Wrap region with LEFT RIGHT.
- with region active and arg 1:
  Wrap region with LEFT RIGHT and put the point after LEFT followed by a space.
- with arg nil:
  Insert LEFT RIGHT.
- with arg negative:
  Wrap as many sexps as possible until the end of the line with LEFT RIGHT.
- with arg 0:
  Wrap as many sexps as possible with LEFT RIGHT.
- with the universal arg:
  Wrap one sexp with LEFT RIGHT.
- with arg positive:
  Wrap that number of sexps with LEFT RIGHT or as many as possible."
  `(lambda (arg)
     (interactive "P")
     (cond ((not arg))
           ((listp arg)
            (setq arg 1))
           (t
            (setq arg (prefix-numeric-value arg))))
     (cond ((region-active-p)
            (lispy--surround-region ,left ,right)
            (when (and (lispy-looking-back lispy-left)
                       (or (lispy-left-p)
                           (> (or arg 0) 0)))
              (insert " "))
            (backward-char 1))
           ((and (lispy--in-string-p)
                 (lispy-looking-back "\\\\\\\\"))
            (insert ,left "\\\\" ,right)
            (backward-char 3))
           ((lispy--in-string-or-comment-p)
            (if (and lispy-parens-only-left-in-string-or-comment
                     (string= ,left "(")
                     (= ?\( (aref (this-command-keys-vector) 0)))
                (insert "(")
              (insert ,left ,right)
              (backward-char 1)))
           ((lispy-after-string-p "?\\")
            (insert ,left))
           ((not arg)
            (lispy--indent-for-tab)
            (lispy--delimiter-space-unless ,preceding-syntax-alist)
            (insert ,left ,right)
            (unless (or (eolp)
                        (lispy--in-string-p)
                        (looking-at "\n\\|)\\|}\\|\\]"))
              (just-one-space)
              (backward-char 1))
            (when (looking-at ,(regexp-quote left))
              (insert " ")
              (backward-char))
            (backward-char))
           (t
            ;; don't jump backwards or out of a list when not at a sexp
            (unless (or (lispy--not-at-sexp-p ,preceding-syntax-alist)
                        (and (memq major-mode lispy-clojure-modes)
                             (looking-at lispy-left)
                             (lispy-after-string-p "#")))
              (when (lispy--bounds-dwim)
                (goto-char (car (lispy--bounds-dwim)))))
            (lispy--indent-for-tab)
            (insert ,left ,right)
            (save-excursion
              (lispy-slurp arg))
            (when (or (looking-at lispy-right)
                      (and (eolp)
                           (looking-back lispy-right (1- (point)))))
              ;; failed to wrap anything
              (backward-char))
            (when (and lispy-insert-space-after-wrap
                       (not (lispy--in-empty-list-p ,preceding-syntax-alist))
                       (not (eolp)))
              (just-one-space)
              (backward-char))))))

(defvar lispy-parens-preceding-syntax-alist
  '((lisp-mode . ("[#`',.@]+" "#[0-9]*" "#[.,Ss+-]" "#[0-9]+[=Aa]"))
    (emacs-lisp-mode . ("[#`',@]+" "#s" "#[0-9]+="))
    (clojure-mode . ("[`'~@]+" "#" "#\\?@?"))
    (clojurescript-mode . ("[`'~@]+" "#" "#\\?@?"))
    (clojurec-mode . ("[`'~@]+" "#" "#\\?@?"))
    (cider-repl-mode . ("[`'~@]+" "#" "#\\?@?"))
    (cider-clojure-interaction-mode . ("[`'~@]+" "#" "#\\?@?"))
    (janet-mode . ("[@;]"))
    (scheme-mode . ("[#`',@]+" "#hash"))
    (t . ("[`',@]+")))
  "An alist of `major-mode' to a list of regexps.
Each regexp describes valid syntax that can precede an opening paren in that
major mode. These regexps are used to determine whether to insert a space for
`lispy-parens'.")

(defvar lispy-brackets-preceding-syntax-alist
  '((clojure-mode . ("[`']" "#[A-z.]*"))
    (clojurescript-mode . ("[`']" "#[A-z.]*"))
    (clojurec-mode . ("[`']" "#[A-z.]*"))
    (cider-repl-mode . ("[`']" "#[A-z.]*"))
    (cider-clojure-interaction-mode . ("[`']" "#[A-z.]*"))
    (janet-mode . ("[@;]"))
    (scheme-mode . ("[#`',@]+" "#hash"))
    (t . nil))
  "An alist of `major-mode' to a list of regexps.
Each regexp describes valid syntax that can precede an opening bracket in that
major mode. These regexps are used to determine whether to insert a space for
`lispy-brackets'.")

(defvar lispy-braces-preceding-syntax-alist
  '((clojure-mode . ("[`'^]" "#[:]*[A-z.:]*"))
    (clojurescript-mode . ("[`'^]" "#[:]*[A-z.:]*"))
    (clojurec-mode . ("[`'^]" "#[:]*[A-z.:]*"))
    (cider-repl-mode . ("[`'^]" "#[:]*[A-z.:]*"))
    (cider-clojure-interaction-mode . ("[`'^]" "#[:]*[A-z.:]*"))
    (janet-mode . ("[@;]"))
    (t . nil))
  "An alist of `major-mode' to a list of regexps.
Each regexp describes valid syntax that can precede an opening brace in that
major mode. These regexps are used to determine whether to insert a space for
`lispy-braces'.")

(defalias 'lispy-parens
    (lispy-pair "(" ")" 'lispy-parens-preceding-syntax-alist)
  "`lispy-pair' with ().")

(defalias 'lispy-brackets
    (lispy-pair "[" "]" 'lispy-brackets-preceding-syntax-alist)
  "`lispy-pair' with [].")

(defalias 'lispy-braces
    (lispy-pair "{" "}" 'lispy-braces-preceding-syntax-alist)
  "`lispy-pair' with {}.")

(defun lispy-quotes (arg)
  "Insert a pair of quotes around the point.

When the region is active, wrap it in quotes instead.
When inside string, if ARG is nil quotes are quoted,
otherwise the whole string is unquoted."
  (interactive "P")
  (let (bnd)
    (cond ((region-active-p)
           (if arg
               (lispy-unstringify)
             (lispy-stringify)))
          ((and (setq bnd (lispy--bounds-string))
                (not (= (point) (car bnd))))
           (if arg
               (lispy-unstringify)
             (if (and lispy-close-quotes-at-end-p (looking-at "\""))
                 (forward-char 1)
                 (progn (insert "\\\"\\\""))
               (backward-char 2))))

          (arg
           (lispy-stringify))

          ((lispy-after-string-p "?\\")
           (self-insert-command 1))

          (t
           (lispy--space-unless "^\\|\\s-\\|\\s(\\|[#]")
           (insert "\"\"")
           (unless (looking-at "\n\\|)\\|}\\|\\]\\|$")
             (just-one-space)
             (backward-char 1))
           (backward-char)))))

(defun lispy-parens-down ()
  "Exit the current sexp, and start a new sexp below."
  (interactive)
  (condition-case nil
      (progn
        (lispy--out-forward 1)
        (if (looking-at "\n *\\()\\)")
            (progn
              (goto-char (match-beginning 1))
              (insert "()")
              (lispy--indent-for-tab)
              (backward-char))

          (insert "\n()")
          (lispy--indent-for-tab)
          (backward-char)))
    (error (indent-new-comment-line))))

;;* Globals: insertion
(defun lispy-space (arg)
  "Insert one space, with position depending on ARG.
If ARG is 2, amend the current list with a space from current side.
If ARG is 3, switch to the different side beforehand.
If jammed between parens, \"(|(\" unjam: \"(| (\". If after an opening delimiter
and before a space (after wrapping a sexp, for example), do the opposite and
delete the extra space, \"(| foo)\" to \"(|foo)\"."
  (interactive "p")
  (cond ((region-active-p)
         (goto-char (region-end))
         (deactivate-mark)
         (insert " "))
        ((lispy--in-string-or-comment-p)
         (call-interactively 'self-insert-command))
        ((eq arg 4)
         (when (lispy--leftp)
           (lispy-other))
         (backward-char)
         (unless (lispy-bolp)
           (newline-and-indent)))
        ((or (eq arg 2)
             (when (eq arg 3)
               (lispy-other)
               t))

         (if (lispy-left-p)
             (progn
               (forward-char)
               (just-one-space)
               (backward-char))
           (backward-char)
           (just-one-space)))
        ((and (lispy-looking-back lispy-left)
              (not (eq ?\\ (char-before (match-beginning 0)))))
         (if (looking-at "[[:space:]]")
             (delete-region (point)
                            (progn
                              (skip-chars-forward "[:space:]")
                              (point)))
           (call-interactively 'self-insert-command)
           (backward-char)))
        (t
         (call-interactively 'self-insert-command)
         (when (and (lispy-left-p)
                    (lispy-looking-back "[[({] "))
           (backward-char)))))

(defvar lispy-colon-p t
  "If true (the default), then add a space before inserting a
colon following `lispy-colon-no-space-regex'. To disable this
behavior, set this variable to nil.")

(defvar lispy-colon-no-space-regex
  '((lisp-mode . "\\s-\\|[:^?#]\\|ql\\|\\(?:\\s([[:word:]-]*\\)"))
  "Overrides REGEX that `lispy-colon' will consider for `major-mode'.
`lispy-colon' will insert \" :\" instead of \":\" unless
`lispy-no-space' is t or `looking-back' REGEX.")

(defun lispy-colon ()
  "Insert :."
  (interactive)
  (when lispy-colon-p
    (lispy--space-unless
     (or (cdr (assoc major-mode lispy-colon-no-space-regex))
         "\\s-\\|\\s(\\|[#:^?]")))
  (insert ":"))

(defun lispy-hat ()
  "Insert ^."
  (interactive)
  (lispy--space-unless "\\s-\\|\\s(\\|[:?]\\|\\\\")
  (insert "^"))

(defun lispy-at ()
  (interactive)
  (lispy--space-unless "\\s-\\|\\s(\\|[:?]\\|\\\\\\|~\\|,")
  (insert "@"))

(defun lispy-tick (arg)
  "Insert ' ARG times.
When the region is active and marks a string, unquote it.
Otherwise, when the region is active, toggle ' at the start of the region."
  (interactive "p")
  (cond ((lispy--string-markedp)
         (lispy-unstringify))
        ((region-active-p)
         (lispy-toggle-char ?\'))
        (t
         (lispy--space-unless "\\s-\\|\\s(\\|[~#:?'`]\\|\\\\")
         (self-insert-command arg))))

(defun lispy-underscore (&optional arg)
  "Insert _ ARG times.
For Clojure modes, toggle #_ sexp comment."
  (interactive "p")
  (setq arg (or arg 1))
  (if (memq major-mode lispy-clojure-modes)
      (let ((leftp (lispy--leftp)))
        (unless leftp
          (lispy-other))
        (if (lispy-after-string-p "#_")
            (delete-char -2)
          (insert "#_"))
        (unless leftp
          (lispy-other)))
    (self-insert-command arg)))

(defun lispy-backtick ()
  "Insert `."
  (interactive)
  (if (region-active-p)
      (lispy--surround-region "`" "'")
    (lispy--space-unless "\\s-\\|\\s(\\|[:?`']\\|\\\\")
    (insert "`")))

(defun lispy-tilde (arg)
  "Insert ~ ARG times.
When the region is active, toggle a ~ at the start of the region."
  (interactive "p")
  (if (region-active-p)
      (lispy-toggle-char ?~)
    (self-insert-command arg)))

(defun lispy-toggle-char (char)
  "Toggle CHAR at the start of the region."
  (let ((bnd (lispy--bounds-dwim))
        deactivate-mark)
    (save-excursion
      (goto-char (car bnd))
      (if (eq (char-after) char)
          (delete-char 1)
        (insert char)))))

(defun lispy-hash ()
  "Insert #."
  (interactive)
  (if (and (or (memq major-mode lispy-clojure-modes)
               (memq major-mode '(nrepl-repl-mode
                                  cider-clojure-interaction-mode)))
           (lispy-looking-back "\\sw #"))
      (progn
        (backward-delete-char 2)
        (insert "#"))
    (lispy--space-unless "\\s-\\|\\s(\\|[#:?'`,]\\\\?")
    (insert "#")))

(declare-function cider-eval-print-last-sexp "ext:cider-eval")
(declare-function cider-repl-newline-and-indent "ext:cider-repl")
(declare-function ielm-return "ielm")
(declare-function mode-local-bind "mode-local")

(defun lispy-newline-and-indent ()
  "Insert newline."
  (interactive)
  (cond ((eq major-mode 'lisp-interaction-mode)
         (setq this-command 'eval-last-sexp)
         (eval-print-last-sexp))
        ((eq major-mode 'cider-clojure-interaction-mode)
         (setq this-command 'cider-eval-print-last-sexp)
         (cider-eval-print-last-sexp))
        ((eq major-mode 'cider-repl-mode)
         (setq this-command 'cider-repl-newline-and-indent)
         (cider-repl-newline-and-indent))
        ((eq major-mode 'inferior-emacs-lisp-mode)
         (lispy-newline-and-indent-plain))
        ((lispy-left-p)
         (skip-chars-backward ",@'`#")
         (newline-and-indent)
         (skip-chars-forward ",@'`#")
         (indent-sexp))
        (t
         (lispy-newline-and-indent-plain))))

(declare-function cider-repl-return "ext:cider-repl")
(declare-function slime-repl-return "ext:slime-repl")
(declare-function sly-mrepl-return "ext:sly-mrepl")
(declare-function racket-repl-submit "ext:racket-repl")
(defun lispy-newline-and-indent-plain ()
  "When in minibuffer, exit it.  Otherwise forward to `newline-and-indent'."
  (interactive)
  (if (minibufferp)
      (exit-minibuffer)
    (cl-case major-mode
      (cider-repl-mode
       (cider-repl-return))
      (slime-repl-mode
       (slime-repl-return))
      (sly-mrepl-mode
       (sly-mrepl-return))
      (comint-mode
       (comint-send-input))
      (python-mode
       (newline-and-indent))
      (inferior-emacs-lisp-mode
       (setq this-command 'ielm-return)
       (ielm-return))
      (racket-repl-mode
       (racket-repl-submit))
      (t
       (if (and (not (lispy--in-string-or-comment-p))
                (if (memq major-mode lispy-clojure-modes)
                    (lispy-looking-back "[^#`'@~][#`'@~]+")
                  (lispy-looking-back "[^#`',@|][#`',@]+")))
           (save-excursion
             (goto-char (match-beginning 0))
             (newline-and-indent))
         (newline-and-indent))
       (let ((lispy-ignore-whitespace t))
         (save-excursion
           (lispy--out-backward 1)
           (unless (< 50000
                      (- (save-excursion (forward-list 1))
                         (point)))
             (indent-sexp))))))))

(defun lispy-open-line (arg)
  "Add ARG lines after the current expression.
When ARG is nagative, add them above instead"
  (interactive "p")
  (save-excursion
    (cond ((lispy-left-p)
           (forward-list))
          ((lispy-right-p))
          (t
           (lispy--out-forward 1)))
    (if (> arg 0)
        (newline arg)
      (forward-list -1)
      (newline (- arg))
      (lispy--indent-for-tab))))

(defun lispy-meta-return ()
  "Insert a new heading."
  (interactive)
  (let ((pt (point)))
    (cond ((lispy--in-comment-p)
           (end-of-line)
           (newline))
          ((and (lispy-bolp)
                (looking-at " *$"))
           (delete-region
            (line-beginning-position)
            (line-end-position)))
          (t
           (lispy-beginning-of-defun)
           (if (save-excursion
                 (forward-list 1)
                 (= (point) pt))
               (progn
                 (forward-list 1)
                 (newline))
             (newline)
             (backward-char 1)))))
  (insert lispy-outline-header
          (make-string (max (lispy-outline-level) 1)
                       ?\*)
          " ")
  (beginning-of-line))

(defun lispy-alt-line (&optional N)
  "Do a context-aware exit, then `newline-and-indent', N times.

Exit branches:

- When in the minibuffer, exit the minibuffer.
- When in a string, exit the string.
- When \")|\", do nothing.
- When \" |)\", exit the list and normalize it.
- When \"|(\", move to the other side of the list.
- When there's a \")\" on the current line before the point, move there.
- Otherwise, move to the end of the line.

This should generally be useful when generating new code.
If you find yourself with:

    (foo (bar (baz 1 2 \"3|\")))

calling this function consecutively, you will get a chance to add arguments
to all the functions, while maintaining the parens in a pretty state."
  (interactive "p")
  (setq N (or N 1))
  (when (bound-and-true-p abbrev-mode)
    (expand-abbrev))
  (let (bnd)
    (lispy-dotimes N
      (cond ((> (minibuffer-depth) 0)
             (exit-minibuffer))
            ((when (setq bnd (lispy--bounds-string))
               (if (> (cdr bnd) (line-end-position))
                   (goto-char (cdr bnd))
                 (goto-char (cdr bnd))
                 nil)))
            ((lispy-right-p))
            ((looking-at lispy-right)
             (when (or (eq (char-before) ?\ )
                       (bolp))
               (lispy-right 1)))
            ((lispy-left-p)
             (lispy-other))
            ((lispy-looking-back "^ +")
             (if (re-search-forward lispy-right (line-end-position) t)
                 (backward-char 1)
               (move-end-of-line 1)))
            ((lispy--in-comment-p))
            (t
             (when bnd
               (goto-char (cdr bnd)))
             (let ((end (min (line-end-position)
                             (cdr (lispy--bounds-list)))))
               (while (< (point) (1- end))
                 (forward-sexp)))))
      (newline-and-indent))))

;;* Globals: miscellanea
(defun lispy-string-oneline ()
  "Convert current string to one line."
  (interactive)
  (when (eq (char-before) ?\")
    (backward-char 1))
  (let (bnd str)
    (setq str (lispy--string-dwim (setq bnd (lispy--bounds-string))))
    (delete-region (car bnd) (cdr bnd))
    (insert (replace-regexp-in-string "\n" "\\\\n" str))))

;;* Locals: navigation
;;** Occur
(defcustom lispy-occur-backend 'ivy
  "Method to navigate to a line with `lispy-occur'."
  :type '(choice
          (const :tag "Ivy" ivy)
          (const :tag "Helm" helm)))

(defvar lispy--occur-beg 1
  "Start position of the top level sexp during `lispy-occur'.")

(defvar lispy--occur-end 1
  "End position of the top level sexp during `lispy-occur'.")

(defun lispy--occur-candidates (&optional bnd)
  "Return the candidates for `lispy-occur'."
  (setq bnd (or bnd (save-excursion
                      (unless (and (bolp)
                                   (lispy-left-p))
                        (beginning-of-defun))
                      (lispy--bounds-dwim))))
  (let ((line-number -1)
        candidates)
    (setq lispy--occur-beg (car bnd))
    (setq lispy--occur-end (cdr bnd))
    (save-excursion
      (goto-char lispy--occur-beg)
      (while (< (point) lispy--occur-end)
        (push (format "%-3d %s"
                      (cl-incf line-number)
                      (buffer-substring
                       (line-beginning-position)
                       (line-end-position)))
              candidates)
        (forward-line 1)))
    (nreverse candidates)))

(defun lispy--occur-preselect ()
  "Initial candidate regex for `lispy-occur'."
  (format "^%d"
          (-
           (line-number-at-pos (point))
           (line-number-at-pos lispy--occur-beg))))

(defvar helm-input)
(declare-function helm "ext:helm")

(defun lispy-occur-action-goto-paren (x)
  "Goto line X for `lispy-occur'."
  (setq x (read x))
  (goto-char lispy--occur-beg)
  (let ((input (if (eq lispy-occur-backend 'helm)
                   helm-input
                 ivy-text))
        str-or-comment)
    (cond ((string= input "")
           (forward-line x)
           (back-to-indentation)
           (when (re-search-forward lispy-left (line-end-position) t)
             (goto-char (match-beginning 0))))

          ((setq str-or-comment
                 (progn
                   (forward-line x)
                   (re-search-forward (ivy--regex input)
                                      (line-end-position) t)
                   (lispy--in-string-or-comment-p)))
           (goto-char str-or-comment))

          ((re-search-backward lispy-left (line-beginning-position) t)
           (goto-char (match-beginning 0)))

          ((re-search-forward lispy-left (line-end-position) t)
           (goto-char (match-beginning 0)))

          (t
           (back-to-indentation)))))

(defun lispy-occur-action-goto-end (x)
  "Goto line X for `lispy-occur'."
  (setq x (read x))
  (goto-char lispy--occur-beg)
  (forward-line x)
  (re-search-forward (ivy--regex ivy-text) (line-end-position) t))

(defun lispy-occur-action-goto-beg (x)
  "Goto line X for `lispy-occur'."
  (when (lispy-occur-action-goto-end x)
    (goto-char (match-beginning 0))))

(defun lispy-occur-action-mc (_x)
  "Make a fake cursor for each `lispy-occur' candidate."
  (let ((cands (nreverse ivy--old-cands))
        cand)
    (while (setq cand (pop cands))
      (goto-char lispy--occur-beg)
      (forward-line (read cand))
      (re-search-forward (ivy--regex ivy-text) (line-end-position) t)
      (when cands
        (mc/create-fake-cursor-at-point))))
  (multiple-cursors-mode 1))

(ivy-set-actions
 'lispy-occur
 '(("m" lispy-occur-action-mc "multiple-cursors")
   ("j" lispy-occur-action-goto-beg "goto start")
   ("k" lispy-occur-action-goto-end "goto end")))

(defvar ivy-last)
(declare-function ivy-state-window "ext:ivy")

(defun lispy-occur ()
  "Select a line within current top level sexp.
See `lispy-occur-backend' for the selection back end."
  (interactive)
  (swiper--init)
  (cond ((eq lispy-occur-backend 'helm)
         (require 'helm)
         (add-hook 'helm-move-selection-after-hook
                   #'lispy--occur-update-input-helm)
         (add-hook 'helm-update-hook
                   #'lispy--occur-update-input-helm)
         (unwind-protect
              (helm :sources
                    `((name . "this defun")
                      (candidates . ,(lispy--occur-candidates))
                      (action . lispy-occur-action-goto-paren)
                      (match-strict .
                                    (lambda (x)
                                      (ignore-errors
                                        (string-match
                                         (ivy--regex helm-input) x)))))
                    :preselect (lispy--occur-preselect)
                    :buffer "*lispy-occur*")
           (swiper--cleanup)
           (remove-hook 'helm-move-selection-after-hook
                        #'lispy--occur-update-input-helm)
           (remove-hook 'helm-update-hook
                        #'lispy--occur-update-input-helm)))
        ((eq lispy-occur-backend 'ivy)
         (unwind-protect
              (ivy-read "pattern: "
                        (lispy--occur-candidates)
                        :preselect (lispy--occur-preselect)
                        :require-match t
                        :update-fn (lambda ()
                                     (lispy--occur-update-input
                                      ivy-text
                                      (ivy-state-current ivy-last)))
                        :action #'lispy-occur-action-goto-paren
                        :caller 'lispy-occur)
           (swiper--cleanup)
           (when (null ivy-exit)
             (goto-char swiper--opoint))))
        (t
         (error "Bad `lispy-occur-backend': %S" lispy-occur-backend))))

(defun lispy--occur-update-input-helm ()
  "Update selection for `lispy-occur' using `helm' back end."
  (lispy--occur-update-input
   helm-input
   (buffer-substring-no-properties
    (line-beginning-position)
    (line-end-position))))

(defun lispy--occur-update-input (input str)
  "Update selection for `ivy-occur'.
INPUT is the current input text.
STR is the full current candidate."
  (swiper--cleanup)
  (let ((re (ivy--regex input))
        (num (if (string-match "^[0-9]+" str)
                 (string-to-number (match-string 0 str))
               0)))
    (with-selected-window (ivy-state-window ivy-last)
      (goto-char lispy--occur-beg)
      (when (cl-plusp num)
        (forward-line num)
        (unless (<= (point) lispy--occur-end)
          (recenter)))
      (let ((ov (make-overlay (line-beginning-position)
                              (1+ (line-end-position)))))
        (overlay-put ov 'face 'swiper-line-face)
        (overlay-put ov 'window (ivy-state-window ivy-last))
        (push ov swiper--overlays))
      (re-search-forward re (line-end-position) t)
      (swiper--add-overlays
       re
       lispy--occur-beg
       lispy--occur-end))))

;;* Locals: Paredit transformations
(defun lispy--sub-slurp-forward (arg)
  "Grow current marked symbol by ARG words forwards.
Return the amount of successful grow steps, nil instead of zero."
  (when (looking-at "\\s_")
    (let ((end (cdr (bounds-of-thing-at-point 'symbol)))
          prev)
      (lispy-dotimes arg
        (setq prev (point))
        (forward-word 1)
        (when (> (point) end)
          (goto-char prev)
          (throw 'result (1- i)))))))

(defun lispy--sub-slurp-backward (arg)
  "Grow current marked symbol by ARG backwards.
Return the amount of successful grow steps, nil instead of zero."
  (when (lispy-looking-back "\\s_")
    (let ((beg (car (bounds-of-thing-at-point 'symbol)))
          prev)
      (lispy-dotimes arg
        (setq prev (point))
        (backward-word 1)
        (when (< (point) beg)
          (goto-char prev)
          (throw 'result (1- i)))))))

(defun lispy-slurp (arg)
  "Grow current sexp by ARG sexps.
If ARG is zero, grow as far as possible. If ARG is -1, grow until the end or
beginning of the line. If it is not possible to slurp to the end of the line,
slurp as far as possible within the line. If before a multi-line list, slurp to
the end of the line where that list ends."
  (interactive "p")
  (if (region-active-p)
      (if (= (point) (region-end))
          (cond ((= arg 0)
                 (while (and (lispy-dotimes 1 (forward-sexp 1))
                             (not (looking-at "\\'")))))
                ((= arg -1)
                 (while (and (not (looking-at (concat lispy-right "*$")))
                             (lispy-dotimes 1 (forward-sexp 1)))))
                ((or (looking-at "\\s_")
                     (save-excursion
                       (goto-char (region-beginning))
                       (and (not (lispy-left-p))
                            (lispy-looking-back "\\s_"))))
                 (lispy--sub-slurp-forward arg))
                ((looking-at "[\n ]+;")
                 (goto-char (match-end 0))
                 (goto-char (cdr (lispy--bounds-comment))))
                (t
                 (lispy-dotimes arg
                   (forward-sexp 1))))
        (cond ((= arg 0)
               (while (and (lispy-dotimes 1 (forward-sexp -1))
                           (not (looking-at "\\`")))))
              ((= arg -1)
               (while (and (not (lispy-looking-back "^[[:space:]]*"))
                           (lispy-dotimes 1 (forward-sexp -1)))))
              ((or (and (not (lispy-left-p))
                        (lispy-looking-back "\\s_"))
                   (save-excursion
                     (goto-char (region-end))
                     (looking-at "\\s_")))
               (lispy--sub-slurp-backward arg))
              ((save-excursion
                 (skip-chars-backward " \n")
                 (lispy--in-comment-p))
               (skip-chars-backward " \n")
               (goto-char (car (lispy--bounds-comment))))
              (t
               (lispy-dotimes arg
                 (forward-sexp -1)))))
    (if (lispy-right-p)
        (cond ((= arg 0)
               (let ((last-pos (point)))
                 (while (and (lispy-dotimes 1
                               (lispy--slurp-forward)
                               (lispy--reindent))
                             (not (= (point) last-pos)))
                   (setq last-pos (point)))))
              ((= arg -1)
               (while (and (not (looking-at (concat "\\("
                                                    lispy-right
                                                    "\\|$\\)")))
                           (lispy-dotimes 1
                             (lispy--slurp-forward)))))
              (t
               (lispy-dotimes arg
                 (lispy--slurp-forward))))
      (if (lispy-left-p)
          (cond ((= arg 0)
                 ;; lispy--slurp-backward errors when reaching another delimiter
                 (while (and (lispy-dotimes 1
                               (lispy--slurp-backward))
                             (not (lispy-looking-back "\\`")))))
                ((= arg -1)
                 (while (and (not (lispy-looking-back "^[[:space:]]*"))
                             (lispy-dotimes 1
                               (lispy--slurp-backward)))))
                (t
                 (lispy-dotimes arg
                   (lispy--slurp-backward))))))
    (lispy--reindent)))

(defun lispy-down-slurp ()
  "Move current sexp or region into the next sexp."
  (interactive)
  (let ((bnd (lispy--bounds-dwim))
        (leftp (lispy--leftp))
        (regionp (region-active-p))
        (bolp (bolp))
        deactivate-mark)
    (when (lispy-left-p)
      (forward-sexp))
    (let ((pt (save-excursion
                (when (lispy-forward 1)
                  (lispy-backward 1)
                  (point)))))
      (when pt
        (goto-char pt)
        (lispy--teleport (car bnd) (cdr bnd) (not leftp) regionp)
        (save-excursion
          (backward-char 1)
          (when (lispy-looking-back (concat lispy-right " +"))
            (just-one-space))
          (when (and bolp (lispy-looking-back "^ +"))
            (delete-region (match-beginning 0)
                           (match-end 0)))
          (indent-sexp))))))

(defun lispy-up-slurp ()
  "Move current sexp or region into the previous sexp.
If the point is by itself on a line or followed only by right delimiters, slurp
the point into the previous list. This can be of thought as indenting the code
to the next level and adjusting the parentheses accordingly."
  (interactive)
  (let* ((empty-line-p (lispy--empty-line-p))
         (list-start (when (eq empty-line-p 'right)
                       (save-excursion
                         (re-search-forward lispy-right)
                         (lispy-other)
                         (point))))
         (failp (when list-start
                  (= list-start
                     (save-excursion
                       (re-search-backward lispy-left)
                       (point)))))
         (bnd (if empty-line-p
                  (cons (point) (point))
                (lispy--bounds-dwim)))
         (regionp (region-active-p))
         (endp (or (lispy-right-p)
                   (and (region-active-p) (= (point) (region-end)))))
         p-beg p-end
         (deactivate-mark nil)
         bsize)
    (deactivate-mark)
    (goto-char (car bnd))
    (if (or failp
            (not (lispy-backward 1)))
        (progn
          (lispy--complain "No list above to slurp into")
          (if regionp
              (lispy--mark bnd)
            (goto-char
             (if endp
                 (cdr bnd)
               (car bnd)))))
      (setq p-beg (point))
      (forward-list)
      (setq p-end (point))
      (goto-char (car bnd))
      (setq bsize (buffer-size))
      (lispy-save-excursion
        (goto-char (cdr bnd))
        (insert (char-before p-end))
        (goto-char p-end)
        (backward-delete-char 1)
        (goto-char p-beg)
        (indent-sexp))
      (setq bnd (cons (point)
                      (+ (point)
                         (- (cdr bnd) (car bnd))
                         (- (buffer-size)
                            bsize
                            (- (point) (car bnd))
                            1))))
      (when regionp
        (lispy--mark bnd))
      (if endp
          (goto-char (cdr bnd))
        (if (region-active-p)
            (lispy-other)
          (goto-char (car bnd)))))))

(defun lispy-indent-adjust-parens (arg)
  "Indent the line if it is incorrectly indented or act as `lispy-up-slurp'.
If indenting does not adjust indentation or move the point, call
`lispy-up-slurp' ARG times."
  (interactive "p")
  (let ((tick (buffer-chars-modified-tick))
        (pt (point))
        (bnd (when (region-active-p)
               (cons (region-beginning)
                     (region-end)))))
    (indent-for-tab-command)
    (when (and (= tick (buffer-chars-modified-tick))
               (= pt (point)))
      (if bnd
          (lispy--mark bnd)
        (unless (lispy--empty-line-p)
          (set-mark (point))
          (lispy-slurp -1)))
      (dotimes (_ arg)
        (lispy-up-slurp))
      (when (and (not bnd)
                 (region-active-p))
        (ignore-errors (lispy-other))
        (deactivate-mark)))))

(defun lispy--backward-sexp-or-comment ()
  "When in comment, move to the comment start.
Otherwise, move to the previous sexp."
  (if (lispy--in-comment-p)
      (goto-char (car (lispy--bounds-comment)))
    (forward-sexp -1))
  (skip-chars-backward " \n"))

(defun lispy--forward-sexp-or-comment ()
  "When before comment, move to the comment end.
Otherwise, move to the next sexp."
  (if (save-excursion
        (skip-chars-forward " \n")
        (lispy--in-comment-p))
      (progn
        (skip-chars-forward " \n")
        (goto-char (cdr (lispy--bounds-comment))))
    (forward-sexp 1)))

(defun lispy-barf (arg)
  "Shrink current sexp or region by ARG sexps."
  (interactive "p")
  (cond ((region-active-p)
         (let* ((bnd (lispy--bounds-dwim))
                (str (lispy--string-dwim bnd))
                (one-symbolp (lispy--symbolp str)))
           (if (= (point) (region-end))
               (cond (one-symbolp
                      (lispy-dotimes arg
                        (if (re-search-backward "\\sw\\s_+" (region-beginning) t)
                            (forward-char 1)
                          (throw 'result i))))
                     ((lispy--in-comment-p)
                      (goto-char (car (lispy--bounds-comment)))
                      (if (= (point) (region-beginning))
                          (goto-char (cdr (lispy--bounds-comment)))
                        (skip-chars-backward " \n")))
                     (t
                      (cl-incf arg)
                      (lispy-dotimes arg
                        (lispy--backward-sexp-or-comment))
                      (when (< (point) (car bnd))
                        (goto-char (car bnd)))
                      (lispy--forward-sexp-or-comment)))
             (cond (one-symbolp
                    (lispy-dotimes arg
                      (if (re-search-forward "\\s_+\\sw" (region-end) t)
                          (backward-char 1)
                        (throw 'result i))))
                   ((lispy--in-comment-p)
                    (goto-char (cdr (lispy--bounds-comment)))
                    (if (= (region-beginning) (region-end))
                        (goto-char (car bnd))
                      (skip-chars-forward " \n")))
                   (t
                    (save-restriction
                      (narrow-to-region (point-min)
                                        (region-end))
                      (cl-incf arg)
                      (lispy-dotimes arg
                        (lispy--forward-sexp-or-comment))
                      (if (lispy--in-comment-p)
                          (goto-char (car (lispy--bounds-comment)))
                        (forward-sexp -1))
                      (widen)))))))

        ((looking-at "()"))

        ((lispy-right-p)
         (lispy-dotimes arg
           (lispy--barf-backward)))

        ((lispy-left-p)
         (lispy-dotimes arg
           (lispy--barf-forward)))))

(defun lispy-splice (arg)
  "Splice ARG sexps into containing list."
  (interactive "p")
  (lispy-dotimes arg
    (let ((bnd (lispy--bounds-dwim))
          (deactivate-mark nil))
      (cond ((region-active-p)
             (save-excursion
               (goto-char (cdr bnd))
               (re-search-backward lispy-right)
               (delete-region (point) (cdr bnd)))
             (save-excursion
               (goto-char (car bnd))
               (re-search-forward lispy-left)
               (delete-region (car bnd) (point))))
            ((lispy-splice-let))

            ((lispy-left-p)
             (save-excursion
               (goto-char (cdr bnd))
               (delete-char -1))
             (lispy--delete-leading-garbage)
             (delete-char 1)
             (lispy-forward 1)
             (lispy-backward 1))

            ((lispy-right-p)
             (setq bnd (lispy--bounds-dwim))
             (delete-char -1)
             (goto-char (car bnd))
             (let ((pt (point)))
               (re-search-forward lispy-left nil t)
               (delete-region pt (point)))
             (lispy-backward 1)
             (forward-list))

            (t
             (setq bnd (lispy--bounds-list))
             (save-excursion
               (goto-char (cdr bnd))
               (delete-char -1))
             (save-excursion
               (goto-char (car bnd))
               (delete-char 1)))))))

(defun lispy-find (item tree)
  (cond ((null tree)
         nil)
        ((consp tree)
         (or (lispy-find item (car tree))
             (lispy-find item (cdr tree))))
        (t
         (eq item tree))))

(defun lispy-splice-let ()
  "Join the current `let' into the parent `let'."
  (when (save-excursion
          (and (looking-at "(let")
               (lispy--out-backward 1)
               (looking-at "(let")))
    (if (memq major-mode lispy-clojure-modes)
        (lispy-splice-let-clojure)
      (let ((child-binds (save-excursion
                           (lispy-flow 1)
                           (read (lispy--string-dwim))))
            (parent-binds
             (mapcar (lambda (x) (if (consp x) (car x) x))
                     (save-excursion
                       (lispy-up 1)
                       (read (lispy--string-dwim)))))
            (end (save-excursion
                   (lispy-flow 2)
                   (point)))
            (beg (save-excursion
                   (lispy-up 1)
                   (lispy-other)
                   (1- (point)))))
        (save-excursion
          (forward-list)
          (delete-char -1))
        (delete-region beg end)
        (when parent-binds
          (newline-and-indent))
        (lispy-left 2)
        (when (cl-find-if (lambda (v) (lispy-find v child-binds))
                          parent-binds)
          (cond
            ((looking-at "(let\\*"))
            ((looking-at "(\\(let\\)")
             (replace-match "(let*")
             (lispy--out-backward 1)
             (indent-sexp))
            (t
             (error "unexpected"))))
        (lispy--prettify-1)
        (lispy-flow 2)
        (when parent-binds
          (lispy-down (length parent-binds))))
      t)))

(defun lispy-splice-let-clojure ()
  "Join the current Clojure `let' form into the parent `let'."
  (let ((end (save-excursion
               (lispy-flow 1)
               (1+ (point))))
        (beg (save-excursion
               (lispy-up 1)
               (lispy-other)
               (1- (point)))))
    (save-excursion
      (forward-list)
      (delete-char -1))
    (delete-region beg end)
    (insert "\n")
    (lispy--out-backward 2)
    (lispy--prettify-1)
    t))

(defun lispy-barf-to-point (arg)
  "Barf to the closest sexp before the point.
When ARG is non-nil, barf from the left."
  (interactive "P")
  (if (and (not arg)
           (looking-at lispy-right))
      (forward-char)
    (unless (or (not (cadr (syntax-ppss)))
                (let ((str (lispy--bounds-string)))
                  (and str
                       (not (= (car str) (point))))))
      (let ((line-number (line-number-at-pos))
            split-moved-point-down)
        (lispy-split)
        (when (and arg
                   (not (= (line-number-at-pos) line-number)))
          (setq split-moved-point-down t))
        (lispy--prettify-1)
        (cond (arg
               (save-excursion
                 (lispy-up 1)
                 (lispy-splice 1))
               (when split-moved-point-down
                 (lispy-delete-backward 1)))
              (t
               (save-excursion
                 (lispy-splice 1))
               (join-line)
               (when (looking-at " $")
                 (delete-char 1))))
        (lispy--reindent 1)))))

(defun lispy-raise (arg)
  "Use current sexp or region as replacement for its parent.
Do so ARG times."
  (interactive "p")
  (lispy-dotimes arg
    (let ((regionp (region-active-p))
          (leftp (lispy--leftp))
          (deactivate-mark nil)
          bnd1 bnd2)
      ;; re-indent first
      (lispy-save-excursion (lispy--out-forward 1))
      (unless leftp
        (lispy-other))
      (setq bnd1 (lispy--bounds-dwim))
      (deactivate-mark)
      (lispy--out-forward 1)
      (setq bnd2 (lispy--bounds-dwim))
      (delete-region (cdr bnd2) (cdr bnd1))
      (delete-region (car bnd2) (car bnd1))
      (if regionp
          (progn
            (indent-region (car bnd2) (point))
            (lispy--mark (cons (car bnd2) (point))))
        (lispy-from-left
         (indent-sexp)))
      (unless (eq leftp (lispy--leftp))
        (lispy-other)))))

(defun lispy-raise-some ()
  "Use current sexps as replacement for their parent.
The outcome when ahead of sexps is different from when behind."
  (interactive)
  (let ((pt (point)))
    (cond ((region-active-p))

          ((lispy-left-p)
           (if (null (lispy--out-forward 1))
               (progn
                 (goto-char pt)
                 (lispy--complain "Not enough depth to raise"))
             (backward-char 1)
             (set-mark (point))
             (goto-char pt)))

          ((lispy-right-p)
           (if (null (lispy--out-forward 1))
               (progn
                 (goto-char pt)
                 (lispy--complain "Not enough depth to raise"))
             (backward-list)
             (forward-char 1)
             (set-mark (point))
             (goto-char pt)))

          (t
           (error "Unexpected")))
    (lispy-raise 1)
    (deactivate-mark)))

(defun lispy-convolute (arg)
  "Replace (...(,,,|( with (,,,(...|( where ... and ,,, is arbitrary code.
When ARG is more than 1, pull ARGth expression to enclose current sexp.
When ARG is nil, convolute only the part above sexp."
  (interactive "p")
  (let ((deactivate-mark nil)
        (only-upper nil))
    (when (= arg 0)
      (setq only-upper t)
      (setq arg 1))
    (if (and (save-excursion
               (lispy--out-forward (1+ arg)))
             (save-excursion
               (lispy--out-backward (1+ arg))))
        (let (beg end)
          (lispy-from-left
           (setq beg (point))
           (setq end (lispy--out-backward arg))
           (lispy--out-backward 1)
           (lispy--swap-regions (cons beg end)
                                (cons (point) (point)))
           (lispy--reindent arg))
          (unless only-upper
            (lispy-from-left
             (lispy-other)
             (setq beg (point))
             (setq end (lispy--out-forward arg))
             (lispy--out-forward 1)
             (lispy--swap-regions (cons beg end)
                                  (cons (point) (point)))
             (ignore-errors
               (lispy-other))
             (lispy--reindent (1+ arg)))))
      (error "Not enough depth to convolute"))))

(defun lispy-convolute-left ()
  "Convolute and move left.
Useful for propagating `let' bindings."
  (interactive)
  (if (region-active-p)
      (progn
        (lispy-convolute 1)
        (lispy-left 1))
    (user-error "region must be active")))

(defvar lispy-repeat--command nil
  "Command to use with `lispy-repeat'.")

(defvar lispy-repeat--prefix-arg nil
  "Prefix arg to use with `lispy-repeat'.")

(defun lispy-repeat ()
  "Repeat last command with last prefix arg."
  (interactive)
  (unless (memq last-command
                '(special-lispy-repeat lispy-repeat))
    (setq lispy-repeat--command last-command)
    (setq lispy-repeat--prefix-arg
          (or last-prefix-arg 1)))
  (setq current-prefix-arg lispy-repeat--prefix-arg)
  (funcall lispy-repeat--command))

(defun lispy-join ()
  "Join sexps."
  (interactive)
  (let ((pt (point))
        bnd)
    (cond ((lispy-right-p)
           (when (lispy-forward 1)
             (backward-list)
             (delete-char 1)
             (goto-char pt)
             (backward-delete-char 1)
             (lispy--out-forward 1)
             (lispy--reindent 1)))
          ((lispy-left-p)
           (when (lispy-backward 1)
             (forward-list)
             (backward-delete-char 1)
             (goto-char (1- pt))
             (delete-char 1)
             (lispy-save-excursion
               (forward-char 1)
               (lispy-left 2)
               (lispy--prettify-1))))
          ((and (setq bnd (lispy--bounds-string))
                (or (save-excursion
                      (goto-char (car bnd))
                      (skip-chars-backward " \t\n")
                      (when (eq (char-before) ?\")
                        (delete-region (1- (point))
                                       (1+ (car bnd)))
                        t))
                    (save-excursion
                      (goto-char (cdr bnd))
                      (skip-chars-forward " \t\n")
                      (when (looking-at "\"")
                        (delete-region (1- (cdr bnd))
                                       (1+ (point)))
                        t))))))))

(defun lispy-split ()
  "Split sexps."
  (interactive)
  (let (bnd
        char-left
        char-right)
    (cond ((lispy--in-comment-p)
           (indent-new-comment-line))
          ((and (setq bnd (lispy--bounds-string))
                (not (= (point) (car bnd))))
           (insert "\"\"")
           (when (eolp)
             (delete-char 1))
           (backward-char)
           (newline-and-indent))
          ((lispy-split-let-binding))
          (t
           (when (save-excursion
                   (prog1 (lispy--out-forward 1)
                     (setq char-right (char-before))
                     (forward-list -1)
                     (setq char-left (char-after))))
             (insert (string char-right char-left))
             (backward-char 2)
             (lispy-right 1))
           (newline-and-indent)
           (when (lispy-left-p)
             (indent-sexp))))))

(defun lispy-split-let-binding ()
  (when (and
         (or (lispy-left-p)
             (lispy-right-p))
         (save-excursion
           (lispy--out-backward 2)
           (looking-at "(let")))
    (save-excursion
      (lispy--out-forward 2)
      (insert ")"))
    (save-excursion
      (insert ")\n(let (")
      (lispy--out-backward 3)
      (lispy--prettify-1))
    (lispy-flow 1)
    (lispy-down 1)
    t))

;;* Locals: more transformations
(defun lispy-move-up (arg)
  "Move current expression up ARG times.  Don't exit parent list.
Also works from inside the list."
  (interactive "p")
  (if (or (lispy-left-p)
          (lispy-right-p)
          (region-active-p)
          (looking-at lispy-outline))
      (lispy--move-up-special arg)
    (let ((offset (-
                   (point)
                   (progn
                     (lispy--out-backward 1)
                     (point)))))
      (lispy--move-up-special arg)
      (forward-char offset))))

(defun lispy-move-down (arg)
  "Move current expression down ARG times.  Don't exit parent list.
Also works from inside the list."
  (interactive "p")
  (if (or (lispy-left-p)
          (lispy-right-p)
          (region-active-p)
          (looking-at lispy-outline))
      (lispy--move-down-special arg)
    (let ((offset (-
                   (point)
                   (progn
                     (lispy--out-backward 1)
                     (point)))))
      (lispy--move-down-special arg)
      (forward-char offset))))

(defun lispy--move-up-region (arg)
  "Swap the marked region ARG positions up.
Precondition: the region is active and the point is at `region-beginning'."
  (cond
    ((and (looking-at "\\_<")
          (save-excursion
            (goto-char (region-end))
            (looking-at "-"))))
    ((lispy-after-string-p "-")
     (let ((bnd1 (lispy--bounds-dwim))
           bnd2)
       (lispy-up arg)
       (setq bnd2 (lispy--bounds-dwim))
       (lispy--swap-regions bnd1 bnd2)
       (setq deactivate-mark nil)
       (set-mark (point))
       (forward-char (- (cdr bnd1) (car bnd1)))))
    ((= arg 1)
     (let ((bnd1 (lispy--bounds-dwim))
           (bnd0 (save-excursion
                   (deactivate-mark)
                   (if (ignore-errors (up-list) t)
                       (lispy--bounds-dwim)
                     (cons (point-min) (point-max)))))
           bnd2)
       (goto-char (car bnd1))
       (if (re-search-backward "[^ \t\n`'#({[]" (car bnd0) t)
           (progn
             (deactivate-mark)
             (if (lispy--in-comment-p)
                 (setq bnd2 (lispy--bounds-comment))
               (when (eq (char-after) ?\")
                 (forward-char)
                 (backward-sexp))
               (when (memq (char-after) '(?\) ?\] ?\}))
                 (forward-char))
               (setq bnd2 (lispy--bounds-dwim)))
             (lispy--swap-regions bnd1 bnd2)
             (setq deactivate-mark nil)
             (goto-char (car bnd2))
             (set-mark (point))
             (forward-char (- (cdr bnd1) (car bnd1))))
         (setq deactivate-mark nil)
         (lispy--mark bnd1)))
     (exchange-point-and-mark))
    (t
     (let ((bnd1 (lispy--bounds-dwim)))
       (lispy-up arg)
       (lispy--mark
        (car
         (lispy--swap-regions
          bnd1 (lispy--bounds-dwim)))))
     (exchange-point-and-mark))))

(defun lispy--move-up-special (arg)
  "Move current expression up ARG times.  Don't exit parent list."
  (let ((at-start (lispy--leftp)))
    (unless (or at-start (looking-at lispy-outline))
      (lispy-other))
    (cond ((region-active-p)
           (lispy--move-up-region arg))
          (t
           (lispy--mark (lispy--bounds-dwim))
           (lispy-move-up arg)
           (deactivate-mark)
           (lispy-other)))
    (unless at-start (lispy-other))))

(defun lispy--move-down-region (arg)
  "Swap the marked region ARG positions down.
Precondition: the region is active and the point is at `region-beginning'."
  (cond
    ((and (lispy-after-string-p "-")
          (save-excursion
            (goto-char (region-end))
            (looking-at "\\_>"))))
    ((save-excursion
       (goto-char (region-end))
       (looking-at "-"))
     (let ((bnd1 (lispy--bounds-dwim))
           bnd2)
       (lispy-down arg)
       (setq bnd2 (lispy--bounds-dwim))
       (lispy--swap-regions bnd1 bnd2)
       (goto-char (cdr bnd2))
       (setq deactivate-mark nil)
       (set-mark (point))
       (forward-char (- (car bnd1) (cdr bnd1)))))
    ((= arg 1)
     (let ((bnd1 (lispy--bounds-dwim))
           (bnd0 (save-excursion
                   (deactivate-mark)
                   (if (ignore-errors (up-list) t)
                       (lispy--bounds-dwim)
                     (cons (point-min) (point-max)))))
           bnd2)
       (goto-char (cdr bnd1))
       (if (re-search-forward "[^ \t\n]" (max (1- (cdr bnd0))
                                              (point)) t)
           (progn
             (deactivate-mark)
             (if (lispy--in-comment-p)
                 (setq bnd2 (lispy--bounds-comment))
               (when (memq (char-before) '(?\( ?\" ?\[ ?\{))
                 (backward-char))
               (setq bnd2 (lispy--bounds-dwim)))
             (lispy--swap-regions bnd1 bnd2)
             (setq deactivate-mark nil)
             (goto-char (cdr bnd2))
             (set-mark (point))
             (backward-char (- (cdr bnd1) (car bnd1))))
         (lispy--mark bnd1)
         (exchange-point-and-mark))))
    (t
     (let ((bnd1 (lispy--bounds-dwim)))
       (lispy-down arg)
       (lispy--mark
        (cdr
         (lispy--swap-regions
          bnd1 (lispy--bounds-dwim))))
       (lispy-other)))))

(defun lispy--move-down-special (arg)
  "Move current expression down ARG times.  Don't exit parent list."
  (let ((at-start (lispy--leftp)))
    (unless (or at-start (looking-at lispy-outline))
      (lispy-other))
    (cond ((region-active-p)
           (lispy--move-down-region arg))
          ((looking-at lispy-outline)
           (lispy-dotimes arg
             (let ((bnd1 (lispy--bounds-outline))
                   bnd2)
               (goto-char (1+ (cdr bnd1)))
               (if (and (setq bnd2 (lispy--bounds-outline))
                        (not (equal bnd1 bnd2)))
                   (progn
                     (lispy--swap-regions bnd1 bnd2)
                     (forward-char (1+ (- (cdr bnd2) (car bnd2)))))
                 (goto-char (car bnd1))))))
          (t
           (lispy--mark (lispy--bounds-dwim))
           (lispy-move-down arg)
           (deactivate-mark)
           (lispy-other)))
    (unless at-start (lispy-other))))

(defun lispy-move-left (arg)
  "Move region left ARG times."
  (interactive "p")
  (lispy-dotimes arg
    (when (save-excursion (ignore-errors (up-list) t))
      (let* ((regionp (region-active-p))
             (leftp (lispy--leftp))
             (bnd (lispy--bounds-dwim))
             (str (lispy--string-dwim bnd))
             pt)
        (delete-region (car bnd) (cdr bnd))
        (cond ((looking-at " *;"))
              ((and (looking-at "\n")
                    (lispy-bolp))
               (delete-region
                (line-beginning-position)
                (1+ (point))))
              ((looking-at "\\([\n ]+\\)[^\n ;]")
               (delete-region (match-beginning 1)
                              (match-end 1))))
        (deactivate-mark)
        (lispy--out-backward 1)
        (setq pt (point))
        (insert str)
        (newline-and-indent)
        (skip-chars-backward " \n")
        (indent-region pt (point))
        (if regionp
            (progn
              (setq deactivate-mark nil)
              (set-mark pt)
              (when leftp
                (exchange-point-and-mark)))
          (when leftp
            (lispy-other)))))))

(defun lispy-move-right (arg)
  "Move region right ARG times."
  (interactive "p")
  (lispy-dotimes arg
    (when (save-excursion (ignore-errors (up-list) t))
      (let* ((regionp (region-active-p))
             (leftp (lispy--leftp))
             (bnd (lispy--bounds-dwim))
             (str (lispy--string-dwim bnd))
             pt)
        (delete-region (car bnd) (cdr bnd))
        (cond ((looking-at " *;"))
              ((and (looking-at "\n")
                    (lispy-bolp))
               (delete-region
                (line-beginning-position)
                (1+ (point))))
              ((looking-at "\\([\n ]+\\)[^\n ;]")
               (delete-region (match-beginning 1)
                              (match-end 1))))
        (lispy--out-backward 1)
        (deactivate-mark)
        (lispy-other)
        (newline-and-indent)
        (setq pt (point))
        (insert str)
        (indent-region pt (point))
        (if regionp
            (progn
              (setq deactivate-mark nil)
              (set-mark pt)
              (when leftp
                (exchange-point-and-mark)))
          (when leftp
            (lispy-other)))))))

(defun lispy-dedent-adjust-parens (arg)
  "Move region or all the following sexps in the current list right.
This can be of thought as dedenting the code to the previous level and adjusting
the parentheses accordingly."
  (interactive "p")
  (let ((line-type (lispy--empty-line-p)))
    (cond ((eq line-type 'right)
           (unless (looking-at lispy-right)
             (re-search-forward lispy-right)
             (backward-char))
           (lispy-dotimes arg
             (when (looking-at "$")
               (error "No longer in sexp"))
             (unless (save-excursion
                       (forward-line -1)
                       (end-of-line)
                       (lispy--in-comment-p))
               (lispy-delete-backward 1))
             (forward-char)
             (newline-and-indent)))
          ((region-active-p)
           (lispy-move-right arg))
          ((not line-type)
           (set-mark (point))
           (lispy-slurp 0)
           (lispy-move-right arg)
           (lispy-other)
           (deactivate-mark)))))

(defun lispy-clone (arg)
  "Clone sexp ARG times.
When the sexp is top level, insert an additional newline."
  (interactive "p")
  (let* ((bnd (lispy--bounds-dwim))
         (str (lispy--string-dwim bnd))
         (pt (point)))
    (cond ((region-active-p)
           (lispy-dotimes arg
             (cl-labels
                 ((doit ()
                    (let (deactivate-mark)
                      (save-excursion
                        (newline)
                        (insert str)
                        (lispy--indent-for-tab)))))
               (if (= (point) (region-end))
                   (doit)
                 (exchange-point-and-mark)
                 (doit)
                 (exchange-point-and-mark)))))
          ((lispy-left-p)
           (goto-char (car bnd))
           (cond ((and (bolp) (looking-at "(defun"))
                  (lispy-dotimes arg
                    (insert str)
                    (newline)
                    (newline))
                  (goto-char pt))
                 ((and (bolp)
                       (save-excursion
                         (goto-char (cdr bnd))
                         (looking-at "\n;; =>")))
                  (lispy-dotimes arg
                    (insert str)
                    (newline-and-indent)
                    (lispy-move-down 1)))
                 (t
                  (lispy-dotimes arg
                    (insert str)
                    (newline-and-indent))
                  (goto-char pt))))
          ((lispy-right-p)
           (if (save-excursion
                 (backward-list)
                 (and (bolp) (looking-at "(defun")))
               (lispy-dotimes arg
                 (newline)
                 (newline-and-indent)
                 (insert str))
             (lispy-dotimes arg
               (newline-and-indent)
               (insert str))))
          (t
           (error "Unexpected")))))

(defvar lispy--oneline-comments nil
  "Collect comments for `lispy--oneline'.")

(defun lispy-mapcan-tree (func expr)
  "Reduce with FUNC all lists in EXPR."
  (cond ((null expr)
         nil)
        ((and (vectorp expr) (> (length expr) 0))
         (apply #'vector
                (funcall func
                         (lispy-mapcan-tree func (aref expr 0))
                         (lispy-mapcan-tree
                          func
                          (cdr
                           (mapcar #'identity expr))))))
        ((listp expr)
         (funcall func
                  (lispy-mapcan-tree func (car expr))
                  (lispy-mapcan-tree func (cdr expr))))
        (t
         expr)))

(defun lispy--oneline (expr &optional ignore-comments)
  "Remove newlines from EXPR.
When IGNORE-COMMENTS is not nil, don't remove comments.
Instead keep them, with a newline after each comment."
  (lispy-mapcan-tree
   (lambda (x y)
     (cond ((equal x '(ly-raw newline))
            y)
           ((lispy--raw-comment-p x)
            (if (null ignore-comments)
                (progn
                  (push x lispy--oneline-comments)
                  y)
              (if (equal (car y) '(ly-raw newline))
                  (cons x y)
                `(,x (ly-raw newline) ,@y))))
           ((and (lispy--raw-string-p x)
                 (null ignore-comments))
            (cons `(ly-raw string ,(replace-regexp-in-string "\n" "\\\\n" (cl-caddr x)))
                  y))
           (t
            (cons x y))))
   expr))

(defun lispy-oneline ()
  "Squeeze current sexp into one line.
Comments will be moved ahead of sexp."
  (interactive)
  (cond ((lispy--in-comment-p)
         (let* ((bnd (lispy--bounds-comment))
                (str (lispy--string-dwim bnd)))
           (delete-region (car bnd) (cdr bnd))
           (insert (lispy-comment-char 2 " ")
                   (mapconcat #'identity
                              (split-string str "[ \n]*;;[ \n]*" t)
                              " "))
           (beginning-of-line)
           (back-to-indentation)))
        ((and (region-active-p)
              (= (char-after (region-beginning)) ?\")
              (= (char-before (region-end)) ?\"))
         (lispy-string-oneline))
        (t
         (unless (or (lispy-left-p)
                     (lispy-right-p))
           (lispy--out-backward 1))
         (let* ((bnd (lispy--bounds-dwim))
                (str (lispy--string-dwim bnd))
                (from-left (lispy-left-p))
                expr)
           (delete-region (car bnd) (cdr bnd))
           (when (region-active-p)
             (deactivate-mark))
           (setq lispy--oneline-comments nil)
           (if (setq expr (ignore-errors
                            (lispy--oneline
                             (lispy--read str))))
               (progn
                 (mapc (lambda (x)
                         (lispy--insert x)
                         (newline))
                       lispy--oneline-comments)
                 (lispy--insert expr))
             (let ((no-comment "")
                   comments)
               (cl-loop for s in (split-string str "\n" t)
                        do (if (string-match "^ *\\(;\\)" s)
                               (push (substring s (match-beginning 1)) comments)
                             (setq no-comment (concat no-comment "\n" s))))
               (when comments
                 (insert (mapconcat #'identity comments "\n") "\n"))
               (insert (substring
                        (replace-regexp-in-string "\n *" " " no-comment) 1))))
           (when from-left
             (backward-list))))))

(defun lispy-multiline (&optional arg)
  "Spread current sexp over multiple lines.
When ARG is `fill', do nothing for short expressions."
  (interactive "p")
  (unless (or (lispy-left-p)
              (lispy-right-p))
    (lispy--out-backward 1))
  (lispy-from-left
   (let* ((bnd (lispy--bounds-list))
          (str (lispy--string-dwim bnd))
          (plain-expr (read str))
          (expr (lispy--read str))
          res)
     (unless (and (eq arg 'fill)
                  (< (length str) 80))
       (unless (listp plain-expr)
         (setq plain-expr nil))
       (if (or (cl-some #'listp plain-expr)
               (member '(ly-raw newline) expr))
           (let ((pt (point)))
             (lispy-forward 1)
             (while (and (lispy-flow 1) (> (point) pt))
               (unless (looking-at "\]\\|)\\|\n")
                 (when (looking-at " *")
                   (replace-match "\n")
                   (backward-char 1))))
             (goto-char pt)
             (indent-sexp))
         (delete-region (car bnd) (cdr bnd))
         (setq res
               (butlast
                (cl-mapcan (lambda (y)
                             (if (memq y '(ly-raw clojure-map clojure-set))
                                 (list y)
                               (list y '(ly-raw newline))))
                           (lispy--read str))))
         (when (vectorp expr)
           (setq res (apply #'vector res)))
         (lispy--insert res))))))

(defvar-local lispy--multiline-take-3
    '(defvar defun defmacro defcustom defgroup defvar-local declare-function
      define-key nth throw define-error defadvice defhydra defsubst)
  "List of constructs for which the first 3 elements are on the first line.")

(setq-mode-local
 clojure-mode
 lispy--multiline-take-3 '())

(defvar lispy--multiline-take-3-arg
  '(defun defmacro declare-function define-error defadvice defhydra defsubst)
  "List of constructs for which the first 3 elements are on the first line.
The third one is assumed to be the arglist and will not be changed.")

(defvar-local lispy--multiline-take-2
    '(defface define-minor-mode
      condition-case while incf car
      cdr > >= < <= /= = eq equal incf
      decf cl-incf cl-decf catch
      require provide setq cons when
      if unless interactive assq delq
      assoc declare lambda remq
      make-variable-buffer-local
      bound-and-true-p
      called-interactively-p
      lispy-dotimes cond case cl-case
      defalias 1+ 1- dotimes dolist boundp fboundp macrop
      null consp oddp zerop plusp minusp kbd
      not pop listp or and)
  "List of constructs for which the first 2 elements are on the first line.")

(setq-mode-local
 clojure-mode
 lispy--multiline-take-2 '(loop recur for fn def defn ns if -> ->>
                           + +' - -' * *' / > >= < <= = ==
                           or and not
                           assoc! assoc assoc-in concat))

(defvar lispy--multiline-take-2-arg '(declare lambda
                                      make-variable-buffer-local
                                      bound-and-true-p
                                      called-interactively-p
                                      lispy-dotimes dotimes)
  "List of constructs for which the first 2 elements are on the first line.
The second one will not be changed.")

(defun lispy-interleave (x lst &optional step)
  "Insert X in between each element of LST.
Don't insert X when it's already there.
When STEP is non-nil, insert in between each STEP elements instead."
  (setq step (or step 1))
  (let ((res (nreverse (lispy-multipop lst step)))
        item)
    (while lst
      (unless (equal (car res) x)
        (push x res))
      (unless (equal (car res)
                     (car (setq item (lispy-multipop lst step))))
        (setq res (nconc (nreverse item) res))))
    (nreverse res)))

(defcustom lispy-multiline-threshold 32
  "Don't multiline expresssions shorter than this when printed as a string."
  :type 'integer)

(defun lispy--translate-newlines (str)
  "Replace quoted newlines with real ones in STR."
  (with-temp-buffer
    (insert str)
    (goto-char (point-min))
    (while (re-search-forward "\\\\n" nil t)
      (unless (= ?\\
                 (char-before (- (point) 2)))
        (replace-match "\n" nil t)))
    (buffer-string)))

(defun lispy--multiline-1 (expr &optional quoted)
  "Transform a one-line EXPR into a multi-line.
When QUOTED is not nil, assume that EXPR is quoted and ignore some rules."
  (cond ((vectorp expr)
         (apply #'vector
                (lispy--multiline-1
                 (mapcar #'identity expr))))
        ((not (listp expr))
         expr)
        ((and lispy-multiline-threshold
              (< (length (lispy--prin1-to-string
                          expr 0 'emacs-lisp-mode))
                 lispy-multiline-threshold))
         expr)
        ((and (eq (car-safe expr) 'ly-raw)
              (memq (cadr expr) '(clojure-map clojure-set)))
         (list 'ly-raw (cadr expr)
               (lispy-interleave '(ly-raw newline)
                                 (mapcar #'lispy--multiline-1 (cl-caddr expr))
                                 2)))
        ((and (eq (car-safe expr) 'ly-raw)
              (eq (nth 1 expr) 'splice))
         (list 'ly-raw (nth 1 expr) (nth 2 expr) (nth 3 expr)
               (lispy-interleave '(ly-raw newline)
                                 (mapcar #'lispy--multiline-1 (car (nthcdr 4 expr)))
                                 2)))
        (t
         (let ((res nil)
               elt)
           (while expr
             (setq elt (pop expr))
             (when (equal elt '(ly-raw clojure-symbol "let"))
               (setq elt 'let))
             (cond
               ((eq elt 'ly-raw)
                (cl-case (car expr)
                  (empty
                   (setq res '(ly-raw empty)))
                  (raw
                   (setq res (cons elt expr)))
                  (dot
                   (setq res (cons elt expr)))
                  (newline
                   (setq res '(ly-raw newline)))
                  (comment
                   (setq res (cons elt expr)))
                  (string
                   (setq res
                         `(ly-raw string
                                  ,(lispy--translate-newlines
                                    (cadr expr)))))
                  (t (unless (= (length expr) 2)
                       (error "Unexpected expr: %S" expr))
                     (unless (null res)
                       (error "Stray ly-raw in %S" expr))
                     (setq res (list 'ly-raw (car expr)
                                     (lispy--multiline-1
                                      (cadr expr)
                                      (car (memq (car expr) '(quote \` clojure-lambda))))))))
                (setq expr nil))
               ((vectorp elt)
                (push
                 (apply #'vector
                        (lispy--multiline-1
                         (mapcar #'identity elt)))
                 res)
                (push '(ly-raw newline) res))
               ((equal elt '(ly-raw dot))
                (when (equal (car res) '(ly-raw newline))
                  (pop res))
                (push elt res))
               ((equal elt '(ly-raw clojure-comma))
                ;; two sexps without newlines, then a comma with a newline
                (when (equal (car res) '(ly-raw newline))
                  (pop res))
                (when (equal (cadr res) '(ly-raw newline))
                  (setq res
                        (cons (car res)
                              (cddr res))))
                (push elt res)
                (push '(ly-raw newline) res))
               ((and (not quoted) (memq elt lispy--multiline-take-3))
                (push elt res)
                ;; name
                (when expr
                  (push (pop expr) res))
                ;; value
                (when expr
                  (if (memq elt lispy--multiline-take-3-arg)
                      (push (pop expr) res)
                    (push (car (lispy--multiline-1 (list (pop expr)))) res)))
                (push '(ly-raw newline) res))
               ((and (not quoted) (memq elt lispy--multiline-take-2))
                (push elt res)
                (when (memq elt lispy--multiline-take-2-arg)
                  (push (pop expr) res)
                  (push '(ly-raw newline) res)))
               ((and (memq elt '(let let*))
                     expr
                     (or (memq major-mode lispy-clojure-modes)
                         (and
                          (listp (car expr))
                          (listp (cdar expr)))))
                (push elt res)
                (let ((body (pop expr)))
                  (push
                   (if (memq major-mode lispy-clojure-modes)
                       (apply #'vector
                              (lispy-interleave '(ly-raw newline)
                                                (mapcar #'lispy--multiline-1 body) 2))
                     (lispy-interleave
                      '(ly-raw newline)
                      (mapcar
                       (lambda (x)
                         (if (and (listp x)
                                  (not (eq (car x) 'ly-raw)))
                             (cons (car x)
                                   (lispy--multiline-1 (cdr x)))
                           x))
                       body)))
                   res))
                (push '(ly-raw newline) res))
               ((keywordp elt)
                (push elt res))
               ((not (listp elt))
                (push elt res)
                (unless (and (numberp elt) (eq quoted 'clojure-lambda))
                  (push '(ly-raw newline) res)))
               (t
                (setq elt (lispy--multiline-1 elt))
                (if (equal elt '(ly-raw newline))
                    (unless (equal elt (car res))
                      (push elt res))
                  (push elt res)
                  (push '(ly-raw newline) res)))))
           (cond ((equal (car res) 'ly-raw)
                  res)
                 ((equal (car res) '(ly-raw newline))
                  (if (and (cdr res)
                           (lispy--raw-comment-p (cadr res)))
                      (nreverse res)
                    (nreverse (cdr res))))
                 (t
                  (nreverse res)))))))

(defun lispy-alt-multiline (&optional silent)
  "Spread current sexp over multiple lines.
When SILENT is non-nil, don't issue messages."
  (interactive)
  (unless (or (lispy-left-p)
              (lispy-right-p))
    (lispy--out-backward 1))
  (let* ((bnd (lispy--bounds-dwim))
         (str (lispy--string-dwim bnd))
         (expr (lispy--read str))
         (expr-o (lispy--oneline expr t))
         (expr-m (lispy--multiline-1 expr-o))
         (leftp (lispy--leftp))
         (print-circle nil))
    (cond ((equal expr expr-m)
           (unless silent
             (message "No change")))
          ((and (memq major-mode lispy-elisp-modes)
                (not
                 (condition-case nil
                     (equal (read str)
                            (read (lispy--prin1-to-string
                                   expr-m 0 major-mode)))
                   (error
                    (lispy--complain "Got an unreadable expr (probably overlay)")
                    t))))
           (error "Got a bad transform: %S" expr-m))
          (t
           (delete-region (car bnd) (cdr bnd))
           (lispy--insert expr-m)
           (when leftp
             (backward-list))))))

(defvar lispy-do-fill nil
  "If t, `lispy-insert-1' will try to fill.")

(defun lispy-fill ()
  "Fill current expression."
  (interactive)
  (if (or (lispy-left-p)
          (lispy-right-p))
      (let ((lispy-do-fill t))
        (lispy--prettify-1))
    (fill-paragraph)))

(defcustom lispy-move-after-commenting t
  "When non-nil, adjust point to next sexp after commenting out a
  sexp. If at last sexp in list, move out and backwards to
  enclosing sexp."
  :type 'boolean
  :group 'lispy)

(defcustom lispy-comment-use-single-semicolon nil
  "When non-nil, prefer single semicolons for comments at the
  right of the source code (after lispy-right or at eol)."
  :type 'boolean
  :group 'lispy)

(defun lispy-comment (&optional arg)
  "Comment ARG sexps."
  (interactive "p")
  (setq arg (or arg 1))
  (if (and (> arg 1) (lispy--in-comment-p))
      (let ((bnd (lispy--bounds-comment)))
        (uncomment-region (car bnd) (cdr bnd)))
    (lispy-dotimes arg
      (let (bnd)
        (cond ((region-active-p)
               (comment-dwim nil)
               (when (lispy--in-string-or-comment-p)
                 (lispy--out-backward 1)))
              ((lispy--in-string-or-comment-p)
               (cond ((and (eq major-mode 'emacs-lisp-mode)
                           (lispy-after-string-p ";;; "))
                      (delete-char -1)
                      (insert "###autoload")
                      (forward-char 1))
                     ((lispy-after-string-p (lispy-comment-char 2 " "))
                      (backward-char 1)
                      (insert (lispy-comment-char))
                      (forward-char 1))
                     ((and lispy-comment-use-single-semicolon
                           (lispy-after-string-p (lispy-comment-char 1 " ")))
                      (delete-region
                       (point)
                       (progn
                         (skip-chars-backward (lispy-comment-char 1 " \n"))
                         (point)))
                      (insert (concat " " (lispy-comment-char 2 " "))))
                     (t
                      (self-insert-command 1))))
              ((memq (char-before) '(?\\ ?\#))
               (self-insert-command 1))
              ((lispy-left-p)
               (setq bnd (lispy--bounds-dwim))
               (when lispy-move-after-commenting
                 (lispy-down 1))
               (comment-region (car bnd) (cdr bnd))
               (when lispy-move-after-commenting
                 (when (or (lispy--in-string-or-comment-p)
                           (looking-at (lispy-comment-char)))
                   (lispy--out-backward 1))))
              ((lispy-right-p)
               (if lispy-comment-use-single-semicolon
                   (progn
                     (unless (eolp)
                       (newline-and-indent)
                       (skip-chars-backward "\n\t "))
                     (comment-dwim nil)
                     (just-one-space))
                 (progn
                   (newline-and-indent)
                   (insert (lispy-comment-char 2 " "))
                   (unless (eolp)
                     (newline)
                     (lispy--reindent 1)
                     (skip-chars-backward "\n\t ")
                     (forward-char 1)))))
              ((eolp)
               (comment-dwim nil)
               (when lispy-comment-use-single-semicolon
                 (just-one-space)))
              ((looking-at " *[])}]")
               (if lispy-comment-use-single-semicolon
                   (if (lispy-bolp)
                       (insert (lispy-comment-char 2 "\n"))
                     (insert (lispy-comment-char 1 "\n")))
                 (progn
                   (unless (lispy-bolp)
                     (insert "\n"))
                   (insert (lispy-comment-char 2 "\n"))))
               (when (lispy--out-forward 1)
                 (lispy--prettify-1))
               (move-end-of-line 0)
               (insert " "))
              ((lispy-bolp)
               (let ((bnd (lispy--bounds-list)))
                 (cond ((null bnd)
                        (comment-region (point) (line-end-position)))
                       ((<= (cdr bnd) (line-end-position))
                        (comment-region (point)
                                        (1- (cdr bnd))))
                       (t
                        (let ((beg (point))
                              (ln-start (line-number-at-pos)))
                          (forward-sexp)
                          (while (and (= (line-number-at-pos) ln-start)
                                      (not (eolp)))
                            (forward-sexp))
                          (comment-region beg (point))
                          (goto-char beg))))
                 (skip-chars-forward " ")))
              ((setq bnd (save-excursion
                           (and (lispy--out-forward 1)
                                (point))))
               (let ((pt (point)))
                 (if (re-search-forward "\n" bnd t)
                     (if (= (count-matches lispy-left pt (point))
                            (count-matches lispy-right pt (point)))
                         (progn (comment-region pt (point))
                                (lispy-forward 1)
                                (lispy-backward 1))
                       (goto-char pt)
                       (re-search-forward lispy-left bnd t)
                       (backward-char 1)
                       (forward-list 1)
                       (comment-region pt (point))
                       (lispy-forward 1)
                       (lispy-backward 1))
                   (comment-region (point) (1- bnd))
                   (lispy--out-backward 1))))
              (t
               (self-insert-command 1)))))))

(defun lispy--quote-string (str &optional quote-newlines)
  "Quote the quotes and backslashes in STR.
Quote the newlines if QUOTE-NEWLINES is t."
  (setq str (replace-regexp-in-string "\\\\" "\\\\\\\\" str))
  (setq str (replace-regexp-in-string "\"" "\\\\\"" str))
  (if quote-newlines
      (replace-regexp-in-string "\n" "\\\\n" str)
    str))

(defun lispy-stringify (&optional arg)
  "Transform current sexp into a string.
Quote newlines if ARG isn't 1."
  (interactive "p")
  (setq arg (or arg 1))
  (let* ((bnd (lispy--bounds-dwim))
         (pt (point))
         (str-1 (buffer-substring-no-properties (car bnd) pt))
         (str-2 (buffer-substring-no-properties pt (cdr bnd)))
         (regionp (region-active-p))
         (leftp (lispy--leftp))
         deactivate-mark)
    (when (and regionp leftp)
      (exchange-point-and-mark))
    (if (lispy--in-string-p)
        (if regionp
            (progn
              (insert "\\\"")
              (exchange-point-and-mark)
              (insert "\\\"")
              (backward-char 2)
              (unless leftp
                (exchange-point-and-mark)))
          (lispy--complain "can't do anything useful here"))
      (deactivate-mark)
      (setq str-1 (lispy--quote-string str-1 (/= arg 1)))
      (setq str-2 (lispy--quote-string str-2 (/= arg 1)))
      (delete-region (car bnd) (cdr bnd))
      (insert "\"" str-1)
      (save-excursion (insert str-2 "\""))
      (when regionp
        (unless (looking-at "\"")
          (backward-char 1))
        (lispy-mark-symbol)
        (if (and leftp (= (point) (region-end)))
            (exchange-point-and-mark))))))

(defun lispy-unstringify ()
  "Unquote string at point."
  (interactive)
  (if (region-active-p)
      (if (lispy--string-markedp)
          (let (deactivate-mark
                (str (lispy--string-dwim))
                (leftp (lispy--leftp)))
            (delete-active-region)
            (set-mark (point))
            (insert (read str))
            (when leftp
              (lispy-other)))
        (lispy--complain "the current region isn't a string"))
    (let* ((bnd (lispy--bounds-string))
           (str (lispy--string-dwim bnd))
           (str-1 (concat (substring str 0 (- (point) (car bnd))) "\""))
           (offset (length (read str-1))))
      (delete-region (car bnd) (cdr bnd))
      (save-excursion (insert (read str)))
      (forward-char offset))))

(defvar lispy-teleport-global nil
  "When non-nil, `lispy-teleport' will consider all open parens in window.
Otherwise, only parens within the current defun are considered.
When you press \"t\" in `lispy-teleport', this will be bound to t temporarily.")

(defmacro lispy-quit-and-run (&rest body)
  "Quit the minibuffer and run BODY afterwards."
  `(progn
     (put 'quit 'error-message "")
     (run-at-time nil nil
                  (lambda ()
                    (put 'quit 'error-message "Quit")
                    ,@body))
     (minibuffer-keyboard-quit)))

(defun lispy-teleport (arg)
  "Move ARG sexps into a sexp determined by `lispy-ace-paren'."
  (interactive "p")
  (let ((beg (save-excursion
               (skip-chars-backward "'")
               (point)))
        end endp regionp
        deactivate-mark)
    (cond ((region-active-p)
           (if (= (point) (region-end))
               (progn
                 (setq end (region-beginning))
                 (setq endp t))
             (setq end (region-end)))
           (setq regionp t))
          ((lispy-left-p)
           (save-excursion
             (unless (lispy-dotimes arg
                       (forward-list 1))
               (error "Unexpected"))
             (setq end (point))))
          ((lispy-right-p)
           (save-excursion
             (setq endp t)
             (unless (lispy-dotimes arg
                       (backward-list arg))
               (error "Unexpected"))
             (setq end (point))))
          (t
           (error "Unexpected")))
    (let* ((lispy-avy-keys (delete ?t lispy-avy-keys))
           (avy-handler-function
            (lambda (x)
              (if (eq x ?t)
                  (progn
                    (avy--done)
                    (lispy-quit-and-run
                     (let ((lispy-teleport-global t))
                       (when regionp
                         (activate-mark))
                       (lispy-teleport arg))))
                (avy-handler-default x))))
           (res (lispy-ace-paren
                 (when lispy-teleport-global
                   2))))
      (cond ((memq res '(t nil))
             (when regionp
               (lispy--mark (cons end beg))))
            (t
             (forward-char 1)
             (unless (looking-at "(")
               (ignore-errors
                 (forward-sexp)))
             (backward-char 1)
             (lispy--teleport beg end endp regionp))))))

(defun lispy-goto ()
  "Invoke `imenu."
  (interactive "p")
  (call-interactively #'imenu))

;;* Locals: dialect-related
(defcustom lispy-eval-display-style 'message
  "Choose a function to display the eval result."
  :type '(choice
          (const :tag "message" message)
          (const :tag "overlay" overlay)))

(defvar lispy-eval-alist
  `((,lispy-elisp-modes lispy lispy--eval-elisp)
    ((,@lispy-clojure-modes nrepl-repl-mode cider-clojure-interaction-mode)
     le-clojure lispy-eval-clojure)
    (python-mode
     le-python lispy--eval-python lispy-eval-python-str lispy-eval-python-bnd)
    (julia-mode
     le-julia lispy-eval-julia lispy-eval-julia-str)
    (racket-mode
     le-racket lispy--eval-racket)
    (scheme-mode
     le-scheme lispy--eval-scheme)
    (lisp-mode
     le-lisp lispy--eval-lisp)
    (hy-mode
     le-hy lispy--eval-hy)))

(defvar lispy-eval-output nil
  "The eval function may set this when there's output.")

(declare-function cider--display-interactive-eval-result "ext:cider-overlays")
(declare-function eros--eval-overlay "ext:eros")

(define-error 'eval-error "Eval error")

(defun lispy--get-eval-last-sexp-or-region-handler ()
  "Gets the most appropriate evaluation handler, depending on the region and the current major/minor modes."
  (assoc-default (lispy--lang)
                 (if (region-active-p)
                     lispy--eval-region-handlers-alist
                   lispy--eval-last-sexp-handlers-alist)))

(defun lispy--get-eval-defun-handler ()
  "Gets the most appropriate evaluation handler, depending on the region and the current major/minor modes."
  (assoc-default (lispy--lang)
                 lispy--eval-defun-handlers-alist))

(defun lispy-eval (arg)
  "Eval the current sexp and display the result."
  (interactive "p")
  ;; if point is at the end of the sexp.
  ;; TODO: region is active, point is at the begging of sexp
  (setq lispy-eval-output nil)
  (when-let ((handler (lispy--get-eval-last-sexp-or-region-handler)))
    (cond
     ((or (region-active-p)
          (lispy-right-p))
      (funcall handler))
     ((lispy-left-p)
      (save-excursion
        (lispy-forward 1)
        (funcall handler))))))

(defun lispy-eval-defun ()
  "Evaluate the top level form."
  (interactive)
  (when-let ((handler (lispy--get-eval-defun-handler)))
    (funcall handler)))

(defvar lispy-message-limit 4000
  "String length limit for `lispy-message' to pop up a window.
For smaller strings `message' is used.")

(defun lispy-message (str &optional popup)
  "Display STR in the echo area.
If STR is too large, pop it to a buffer instead."
  (if (or
       popup
       (> (length str) lispy-message-limit)
       (> (cl-count ?\n str)
          (or
           14
           (* (window-height (frame-root-window)) max-mini-window-height))))
      (with-current-buffer (pop-to-buffer "*lispy-message*")
        (special-mode)
        (let ((inhibit-read-only t))
          (delete-region (point-min) (point-max))
          (insert str)
          (ignore-errors (pp-buffer))
          (goto-char (point-min)))
        str)
    (condition-case nil
        (message str)
      (error (message (replace-regexp-in-string "%" "%%" str))))))

(defun lispy-show-top-level ()
  "Show first line of top-level form containing point."
  (interactive)
  (save-excursion
    (beginning-of-defun)
    (message "%s"
             (buffer-substring (point-at-bol) (point-at-eol)))))


;;; Describe
;;;

(defun lispy-describe ()
  "Display documentation for `lispy--current-function'."
  (interactive)
  (if-let ((handler (lispy--get-describe-handler)))
      (funcall handler)
    (lispy--complain-not-supported)))

(defun lispy--emacs-lisp-describe-symbol ()
  (message "in describe")
  (let ((symbol (intern-soft (lispy--current-function))))
    (cond ((fboundp symbol)
           (describe-function symbol))
          ((boundp symbol)
           (describe-variable symbol)))))

(defun lispy--get-describe-handler ()
  "Gets the most appropriate function for describing the thing at point, depending on the region and the current major/minor modes."
  (assoc-default (lispy--lang)
                 lispy--describe-handlers-alist))

(declare-function cider-doc-lookup "ext:cider-doc")
(defun lispy--clojure-describe-symbol ()
  (cond ((bound-and-true-p cider-mode)
         (require 'cider-doc)
         (cider-doc-lookup (lispy--current-function)))
        ('t
         (lispy--complain-not-supported))))

(defvar lispy--pams (make-hash-table))

(defun lispy-pam-store (sym)
  "Store point and mark to SYM."
  (if (region-active-p)
      (progn
        (puthash sym (cons (point) (mark)) lispy--pams)
        (deactivate-mark))
    (puthash sym (point) lispy--pams)))

(defun lispy-pam-restore (sym)
  "Restore point and mark from FROM."
  (let ((val (gethash sym lispy--pams)))
    (cond ((consp val)
           (goto-char (car val))
           (set-mark (cdr val)))
          ((numberp val)
           (goto-char val)))))

(defun lispy-beginning-of-defun (&optional arg)
  "Forward to `beginning-of-defun' with ARG.  Deactivate region.
When called twice in a row, restore point and mark."
  (interactive "p")
  (cond ((and (called-interactively-p 'any)
              (looking-at "^(")
              (let* ((lispy-bof-last-point (gethash 'lispy-bof-last-point lispy--pams))
                     (pt (if (consp lispy-bof-last-point)
                             (car lispy-bof-last-point)
                           lispy-bof-last-point)))
                (and
                 (> pt (point))
                 (<= pt (save-excursion (forward-list) (point))))))
         (lispy-pam-restore 'lispy-bof-last-point))
        ((looking-at "^("))
        (t
         (lispy-pam-store 'lispy-bof-last-point)
         (beginning-of-defun arg))))

;;* Locals: avy-jump
(declare-function avy--regex-candidates "avy")
(declare-function avy-process "avy")
(declare-function avy--overlay-post "avy")

(defun lispy-ace-char ()
  "Visually select a char within the current defun."
  (interactive)
  (let ((avy-keys lispy-avy-keys))
    (avy-with lispy-ace-char
      (lispy--avy-do
       (string (read-char "Char: "))
       (save-excursion
         ;; `beginning-of-defun' won't work, since it can change sexp
         (lispy--out-backward 50)
         (lispy--bounds-dwim))
       (lambda () t)
       lispy-avy-style-char))))

(defun lispy-ace-paren (&optional arg)
  "Jump to an open paren within the current defun.
ARG can extend the bounds beyond the current defun."
  (interactive "p")
  (setq arg (or arg 1))
  (lispy--remember)
  (deactivate-mark)
  (let ((avy-keys lispy-avy-keys)
        (bnd (if (eq arg 1)
                 (save-excursion
                   (lispy--out-backward 50)
                   (lispy--bounds-dwim))
               (cons (window-start)
                     (window-end nil t)))))
    (avy-with lispy-ace-paren
      (lispy--avy-do
       lispy-left
       bnd
       (lambda () (not (lispy--in-string-or-comment-p)))
       lispy-avy-style-paren))))

(defun lispy-ace-symbol (arg)
  "Jump to a symbol within the current sexp and mark it.
Sexp is obtained by exiting the list ARG times."
  (interactive "p")
  (lispy--out-forward
   (if (region-active-p)
       (progn (deactivate-mark) arg)
     (1- arg)))
  (let ((avy-keys lispy-avy-keys)
        res)
    (avy-with lispy-ace-symbol
      (let ((avy--overlay-offset (if (eq lispy-avy-style-symbol 'at) -1 0)))
        (setq res (lispy--avy-do
                   "[([{ ]\\(?:\\sw\\|\\s_\\|[\"'`#~,@]\\)"
                   (lispy--bounds-dwim)
                   (lambda ()
                     (not (save-excursion
                            (forward-char -1)
                            (lispy--in-string-or-comment-p))))
                   lispy-avy-style-symbol))))
    (unless (memq res '(t nil))
      (unless (or (eq (char-after) ?\")
                  (looking-at ". "))
        (forward-char 1))
      (lispy-mark-symbol))))

(defun lispy-ace-symbol-beginning-of-defun ()
  (interactive)
  (lispy-ace-symbol 99))

(defun lispy-ace-subword (arg)
  "Mark sub-word within a sexp.
Sexp is obtained by exiting list ARG times."
  (interactive "p")
  (if (and (region-active-p)
           (string-match "\\`\\(\\sw+\\)\\s_"
                         (lispy--string-dwim)))
      (lispy--mark (cons (region-beginning)
                         (+ (region-beginning) (match-end 1))))
    (lispy--out-forward
     (if (region-active-p)
         (progn (deactivate-mark) arg)
       (1- arg)))
    (let* ((avy-keys lispy-avy-keys)
           (res (avy-with 'lispy-ace-subword
                  (lispy--avy-do
                   "[([{ -/]\\(?:\\sw\\|\\s_\\|\\s(\\|[\"'`#]\\)"
                   (lispy--bounds-dwim)
                   (lambda () (or (not (lispy--in-string-or-comment-p))
                                  (lispy-looking-back ".\"")))
                   lispy-avy-style-symbol))))
      (unless (memq res '(t nil))
        (skip-chars-forward "-([{ `'#")
        (mark-word)))))

(defun lispy--avy-do (regex bnd filter style &optional group)
  "Visually select a match to REGEX within BND.
Filter out the matches that don't match FILTER.
Use STYLE function to update the overlays."
  (lispy--recenter-bounds bnd)
  (let* ((avy-all-windows nil)
         (cands (avy--regex-candidates
                 regex
                 (car bnd) (cdr bnd)
                 filter
                 group)))
    (dolist (x cands)
      (when (> (- (cdar x) (caar x)) 1)
        (cl-incf (caar x))))
    (avy-process
     cands
     (cl-case style
       (pre #'avy--overlay-pre)
       (at #'avy--overlay-at)
       (at-full #'avy--overlay-at-full)
       (post #'avy--overlay-post)))))

(declare-function ediff-regions-internal "ediff")

(defun lispy-tab ()
  "Indent code and hide/show outlines.
When region is active, call `lispy-mark-car'."
  (interactive)
  (if (region-active-p)
      (lispy-mark-car)
    (lispy--prettify-1)))

;;* Locals: refactoring
(defun lispy-to-lambda ()
  "Turn the current function definition into a lambda."
  (interactive)
  (when (save-excursion (lispy--out-backward 1))
    (beginning-of-defun))
  (forward-char 1)
  (let ((beg (point)))
    (when (re-search-forward "(" (save-excursion (forward-list)) t)
      (delete-region beg (- (point) 2))
      (goto-char beg)
      (insert "lambda")
      (goto-char (1- beg)))))

(defun lispy-extract-defun ()
  "Extract the marked block as a defun.
For the defun to have arguments, capture them with `lispy-bind-variable'."
  (interactive)
  (let* ((bnd (lispy--bounds-dwim))
         (str (lispy--string-dwim bnd))
         (expr (lispy--read (format "(progn %s)" str)))
         (name
          (make-symbol
           (read-string "Function name: ")))
         vars
         expr-without-let
         expr-defun
         expr-funcall)
    (setq vars nil)
    (setq expr-without-let
          (lispy-mapcan-tree
           (lambda (x y)
             (if (eq (car-safe x) 'let)
                 (let* ((var-conses (cadr x))
                        (first-var-cons (car var-conses))
                        (var-name (car first-var-cons))
                        (let-body (cddr x)))
                   (if (equal (list var-name)
                              (delete '(ly-raw newline) let-body))
                       (progn
                         (push (cons var-name (cdr first-var-cons)) vars)
                         (cons var-name y))
                     (cons x y)))
               (cons x y)))
           expr))
    (setq expr-defun
          `(defun ,name ,(or (mapcar 'car vars) '(ly-raw empty))
             (ly-raw newline)
             ,@(cdr expr-without-let)))
    (setq expr-funcall
          `(,name ,@(mapcar 'cadr vars)))
    (delete-region (car bnd) (cdr bnd))
    (lispy--insert expr-funcall)
    (save-excursion
      (lispy-beginning-of-defun)
      (lispy--insert expr-defun)
      (insert "\n\n"))))

(declare-function lispy-flatten--clojure "le-clojure")
(declare-function lispy-flatten--lisp "le-lisp")
(defun lispy-flatten (arg)
  "Inline a function at the point of its call.
Pass the ARG along."
  (interactive "P")
  (cond ((memq major-mode lispy-elisp-modes)
         (lispy-flatten--elisp arg))

        ((or (memq major-mode lispy-clojure-modes)
             (memq major-mode '(nrepl-repl-mode
                                cider-clojure-interaction-mode)))
         (require 'le-clojure)
         (lispy-flatten--clojure arg))

        ((eq major-mode 'lisp-mode)
         (lispy-flatten--lisp))

        (t
         (lispy--complain
          (format "%S isn't currently supported" major-mode)))))

(defun lispy-let-flatten ()
  "Inline a function at the point of its call using `let'."
  (interactive)
  (let* ((begp (if (lispy-left-p)
                   t
                 (if (lispy-right-p)
                     (progn (backward-list) nil)
                   (lispy-left 1))))
         (bnd (lispy--bounds-list))
         (str (lispy--string-dwim bnd))
         (expr (lispy--read str))
         (fstr (condition-case e
                   (lispy--function-str
                    (car expr))
                 (unsupported-mode-error
                  (lispy--complain
                   (format
                    "Can't flatten: symbol `%s' is defined in `%s'"
                    (lispy--prin1-fancy (car expr))
                    (lispy--prin1-fancy (cdr e))))
                  nil))))
    (when fstr
      (goto-char (car bnd))
      (delete-region
       (car bnd)
       (cdr bnd))
      (if (macrop (car expr))
          (error "macros not yet supported")
        (let* ((e-args (cl-remove-if
                        #'lispy--whitespacep
                        (cdr expr)))
               (p-body (lispy--function-parse fstr))
               (f-args (car p-body))
               (body (cadr p-body))
               (print-quoted t)
               (body
                (cond (e-args
                       `(let ,(cl-mapcar #'list f-args e-args)
                          (ly-raw newline)
                          ,@body))
                      ((= 1 (length body))
                       (car body))
                      (t
                       (cons 'progn body)))))
          (lispy--insert body)))
      (lispy-multiline)
      (when begp
        (goto-char (car bnd))))))

(defun lispy-flatten--elisp (arg)
  "Inline an Elisp function at the point of its call.
The function body is obtained from `find-function-noselect'.
With ARG, use the contents of `lispy-store-region-and-buffer' instead."
  (let* ((begp (if (lispy-left-p)
                   t
                 (if (lispy-right-p)
                     (progn (backward-list)
                            nil)
                   (lispy-left 1))))
         (bnd (lispy--bounds-list))
         (str (lispy--string-dwim bnd))
         (expr (lispy--read str))
         (fstr (if arg
                   (with-current-buffer (get 'lispy-store-bounds 'buffer)
                     (lispy--string-dwim (get 'lispy-store-bounds 'region)))
                 (condition-case e
                     (lispy--function-str (car expr))
                   (unsupported-mode-error
                    (lispy--complain
                     (format "Can't flatten: symbol `%s' is defined in `%s'"
                             (lispy--prin1-fancy (car expr))
                             (lispy--prin1-fancy (cdr e))))
                    nil))))
         (res (if (macrop (car expr))
                  (macroexpand (read str))
                (lispy--flatten-function
                 fstr
                 (cl-remove-if #'lispy--whitespacep (cdr expr))))))
    (when fstr
      (goto-char (car bnd))
      (delete-region (car bnd) (cdr bnd))
      (if (macrop (car expr))
          (progn
            (save-excursion
              (insert (pp-to-string res))
              (when (bolp)
                (delete-char -1)))
            (indent-sexp))
        (let* ((print-quoted t))
          (lispy--insert res)))
      (lispy-alt-multiline)
      (when begp
        (goto-char (car bnd))))))

(defun lispy-to-ifs ()
  "Transform current `cond' expression to equivalent `if' expressions."
  (interactive)
  (lispy-from-left
   (let* ((bnd (lispy--bounds-dwim))
          (expr (lispy--read (lispy--string-dwim bnd))))
     (unless (eq (car expr) 'cond)
       (error "%s isn't cond" (car expr)))
     (delete-region (car bnd) (cdr bnd))
     (lispy--fast-insert
      (car
       (lispy--whitespace-trim
        (lispy--cases->ifs (cdr expr)))))))
  (lispy-from-left
   (indent-sexp)))

(defun lispy-toggle-thread-last ()
  "Toggle current expression between last-threaded/unthreaded forms.
Macro used may be customized in `lispy-thread-last-macro', which see."
  (interactive)
  (lispy-from-left
   (if (looking-at (concat "(" lispy-thread-last-macro))
       (lispy-unthread-last)
     (lispy-thread-last))))

(defun lispy-thread-last ()
  "Transform current expression to equivalent threaded-last expression."
  (lispy-from-left
   (insert "(" lispy-thread-last-macro ")")
   (lispy-slurp 1)
   (lispy-flow 1)
   (while (and (lispy-right-p)
               (save-excursion (backward-char) (lispy-right-p)))
     (lispy-barf 1)
     (lispy-move-down 1)
     (lispy-up 1))
   (lispy-left 1)))

(defun lispy-unthread-last ()
  "Transform current last-threaded expression to equivalent unthreaded expression."
  (lispy-from-left
   (lispy-flow 1)
   (lispy-other)
   (while (lispy-forward 1)
     (lispy-move-up 1)
     (lispy-slurp 1))
   (lispy-other)
   (lispy-flow 1)
   (lispy-raise 1)))

;;* Locals: multiple cursors
(declare-function mc/create-fake-cursor-at-point "ext:multiple-cursors-core")
(declare-function multiple-cursors-mode "ext:multiple-cursors-core")
(declare-function mc/all-fake-cursors "ext:multiple-cursors-core")
(declare-function mc/maybe-multiple-cursors-mode "ext:multiple-cursors-core")
(declare-function mc/mark-lines "ext:mc-mark-more")
(declare-function mc/remove-fake-cursors "ext:multiple-cursors-core")

(defun lispy-cursor-down (arg)
  "Add ARG cursors using `lispy-down'."
  (interactive "p")
  (require 'multiple-cursors)
  (if (and (mc/all-fake-cursors)
           (not (eq last-command
                    'lispy-cursor-down)))
      (progn
        (deactivate-mark)
        (mc/remove-fake-cursors))
    (if (lispy-left-p)
        (lispy-dotimes arg
          (mc/create-fake-cursor-at-point)
          (cl-loop do (lispy-down 1)
             while (mc/all-fake-cursors (point) (1+ (point)))))
      (mc/mark-lines arg 'forwards))
    (mc/maybe-multiple-cursors-mode)))

(eval-after-load 'multiple-cursors
  '(defadvice mc/execute-command-for-all-fake-cursors
       (around lispy-other-mode-mc (cmd) activate)
     ;; TODO: (Andre0991) lispy-other-mode was removed in favor of simply reading a char
     ;; is the first (and) condition still relevant?
     ;; Related issue: https://github.com/abo-abo/lispy/issues/112
     (unless (and ;; (eq cmd 'special-lispy-other-mode)
              (or (lispy-left-p)
                  (lispy-right-p)
                  (region-active-p)))
       ad-do-it)))

(defun lispy-cursor-ace ()
  "Add a cursor at a visually selected paren.
Currently, only one cursor can be added with local binding.
Any amount can be added with a global binding."
  (interactive)
  (require 'multiple-cursors)
  (mc/create-fake-cursor-at-point)
  (lispy--avy-do
   "("
   (cons (window-start) (window-end))
   (lambda () (not (lispy--in-string-or-comment-p)))
   lispy-avy-style-paren)
  (mc/maybe-multiple-cursors-mode))

;;* Locals: ediff
(defun lispy-store-region-and-buffer ()
  "Store current buffer and `lispy--bounds-dwim'."
  (interactive)
  (put 'lispy-store-bounds 'buffer (current-buffer))
  (put 'lispy-store-bounds 'region (lispy--bounds-dwim)))

(defun lispy--vertical-splitp ()
  "Return nil if the frame isn't two vertical windows.
In case it is, return the left window."
  (let ((windows (window-list)))
    (when (= (length windows) 2)
      (let ((wnd1 (car windows))
            (wnd2 (cadr windows)))
        (when (= (window-pixel-top wnd1)
                 (window-pixel-top wnd2))
          (if (< (window-pixel-left wnd1)
                 (window-pixel-left wnd2))
              wnd1
            wnd2))))))

(defun lispy--ediff-regions (bnd1 bnd2 &optional buf1 buf2 desc1 desc2)
  (interactive)
  (let ((wnd (current-window-configuration))
        (e1 (lispy--make-ediff-buffer
             (or buf1 (current-buffer)) (or desc1 "-A-")
             bnd1))
        (e2 (lispy--make-ediff-buffer
             (or buf2 (current-buffer)) (or desc2 "-B-")
             bnd2)))
    (require 'ediff)
    (apply #'ediff-regions-internal
           `(,@(if (equal (selected-window)
                          (lispy--vertical-splitp))
                   (append e1 e2)
                 (append e2 e1))
               nil ediff-regions-linewise nil nil))
    (add-hook 'ediff-after-quit-hook-internal
              `(lambda ()
                 (setq ediff-after-quit-hook-internal nil)
                 (set-window-configuration ,wnd)))))

(defun lispy-ediff-regions ()
  "Comparable to `ediff-regions-linewise'.
First region and buffer come from `lispy-store-region-and-buffer'
Second region and buffer are the current ones."
  (interactive)
  (if (null (get 'lispy-store-bounds 'buffer))
      (error "No bounds stored: call `lispy-store-region-and-buffer' for this")
    (lispy--ediff-regions
     (lispy--bounds-dwim)
     (get 'lispy-store-bounds 'region)
     (current-buffer)
     (get 'lispy-store-bounds 'buffer))))

;;* Locals: marking
(defun lispy-mark-right (arg)
  "Go right ARG times and mark."
  (interactive "p")
  (let* ((pt (point))
         (mk (mark))
         (lispy-ignore-whitespace t)
         (r (lispy--out-forward arg)))
    (deactivate-mark)
    (if (or (= pt (point))
            (= mk (point))
            (and (region-active-p)
                 (= (region-beginning)
                    (region-end))))
        (progn
          (lispy--complain "can't go any further")
          (if (> mk pt)
              (lispy--mark (cons pt mk))
            (lispy--mark (cons mk pt)))
          nil)
      (lispy--mark
       (lispy--bounds-dwim))
      r)))

(defun lispy-mark-left (arg)
  "Go left ARG times and mark."
  (interactive "p")
  (if (lispy-mark-right arg)
      (lispy-other)
    (when (= (point) (region-end))
      (exchange-point-and-mark))))

(defun lispy-mark-car ()
  "Mark the car of current thing."
  (interactive)
  (lispy--remember)
  (let ((bnd-1 (lispy--bounds-dwim))
        bnd-2)
    (cond ((and (eq (char-after (car bnd-1)) ?\")
                (eq (char-before (cdr bnd-1)) ?\")
                (eq 1 (length (read (format "(%s)" (lispy--string-dwim))))))
           (lispy--mark (cons (1+ (car bnd-1))
                              (1- (cdr bnd-1)))))

          ((and (eq (char-after (car bnd-1)) ?\`)
                (eq (char-before (cdr bnd-1)) ?\'))
           (lispy--mark (cons (1+ (car bnd-1))
                              (1- (cdr bnd-1)))))

          ((save-excursion
             (goto-char (car bnd-1))
             (looking-at "\\(['`,@]+\\)\\w"))
           (set-mark (match-end 1))
           (goto-char (cdr bnd-1)))

          ((and (region-active-p)
                (or (and (= (point) (region-end))
                         (looking-at "\\_>"))
                    (and (= (point) (region-beginning))
                         (looking-at "\\_<")))))
          (t
           (goto-char (car bnd-1))
           (while (and (equal bnd-1 (setq bnd-2 (bounds-of-thing-at-point 'sexp)))
                       (< (point) (cdr bnd-1)))
             (forward-char)
             (skip-chars-forward " "))
           (if bnd-2
               (lispy--mark bnd-2)
             (lispy--complain "can't descend further"))))))

;;* Locals: miscellanea
(declare-function lispy--eval-python "le-python")

(defun lispy-cd ()
  "Change the current REPL working directory."
  (interactive)
  (if (eq major-mode 'python-mode)
      (let* ((pwd
              (lispy--eval-python "import os; print(os.getcwd())"))
             (cwd (read-directory-name "cd: " pwd)))
        (lispy--eval-python (format "os.chdir('%s')" cwd))
        (message cwd))
    (user-error "Unimplemented for %S" major-mode)))

(defhydra hydra-lispy-x (:exit t
                         :hint nil
                         :columns 3)
  "x"
  ;; ("a" nil)
  ("b" lispy-bind-variable "bind variable")
  ("C" lispy-cleanup "cleanup")
  ("D" lispy-extract-defun "extract defun")
  ("f" lispy-flatten "flatten")
  ("F" lispy-let-flatten "let-flatten")
  ;; ("g" nil)
  ("h" lispy-describe "describe")
  ("i" lispy-to-ifs "to ifs")
  ("k" lispy-extract-block "extract block")
  ("l" lispy-to-lambda "to lambda")
  ("m" lispy-cursor-ace "multi cursor")
  ("n" lispy-cd)
  ;; ("o" nil)
  ("p" lispy-set-python-process "process")
  ;; ("q" nil)
  ;; ("r" nil)
  ("s" save-buffer)
  ("t" lispy-view-test "view test")
  ("v" lispy-eval-expression "eval")
  ("w" lispy-show-top-level "where")
  ;; ("x" nil)
  ;; ("y" nil)
  ;; ("z" nil)
  ("B" lispy-store-region-and-buffer "store list bounds")
  ("R" lispy-reverse "reverse")
  ("T" lispy-ert "ert")
  (">" lispy-toggle-thread-last "toggle last-threaded form")
  ("" lispy-x-more-verbosity :exit nil)
  ("?" lispy-x-more-verbosity "help" :exit nil))

(defun lispy-cleanup ()
  (interactive)
  (save-excursion
    (while (re-search-forward "^;; =>" nil t)
      (let ((bnd (lispy--bounds-comment)))
        (delete-region (car bnd) (1+ (cdr bnd))))))
  (save-buffer))

(defvar lispy-x--old-hint "")

(defun lispy-x-more-verbosity ()
  (interactive)
  (let ((cv (hydra-get-property 'hydra-lispy-x :verbosity)))
    (cl-case cv
      (0
       (setq lispy-x--old-hint hydra-lispy-x/hint)
       (setq hydra-lispy-x/hint
             (hydra--format 'hydra-lispy-x '(nil nil :exit t :hint none)
                            (concat
                             "\n_b_nd _c_nd _d_ef _e_de _f_la "
                             "_h_elp _i_f _j_mp bl_k_ _l_mb _m_ul "
                             "_r_ep _s_av _u_nb _v_t _B_nd _R_ev er_T_ _?_")
                            hydra-lispy-x/heads))
       (hydra-set-property 'hydra-lispy-x :verbosity 1))
      (1
       (setq hydra-lispy-x/hint lispy-x--old-hint)
       (hydra-set-property 'hydra-lispy-x :verbosity 2)))))

(defcustom lispy-x-default-verbosity 0
  "Default verbosity of `lispy-x'."
  :type '(radio (const 0) (const 1)))

(defun lispy-x ()
  "Forward to `hydra-lispy-x/body'"
  (interactive)
  (hydra-set-property 'hydra-lispy-x :verbosity lispy-x-default-verbosity)
  (hydra-lispy-x/body))

(defun lispy-ert ()
  "Call (`ert' t)."
  (interactive)
  (ert t))

(defun lispy-undo ()
  "Deactivate region and `undo'."
  (interactive)
  (when (region-active-p)
    (deactivate-mark t))
  (undo))

(defun lispy-view ()
  "Recenter current sexp to first screen line, accounting for scroll-margin.
If already there, return it to previous position."
  (interactive)
  (lispy-from-left
   (let ((window-line (count-lines (window-start) (point))))
     (if (or (= window-line scroll-margin)
             (and (not (bolp)) (= window-line (1+ scroll-margin))))
         (recenter (or (get 'lispy-recenter :line) 0))
       (put 'lispy-recenter :line window-line)
       (recenter 0)))))

(unless (fboundp 'macrop)
  (defun macrop (object)
    "Non-nil if and only if OBJECT is a macro."
    (let ((def (indirect-function object)))
      (when (consp def)
        (or (eq 'macro (car def))
            (and (autoloadp def) (memq (nth 4 def) '(macro t))))))))

(defalias 'lispy--preceding-sexp
    (if (fboundp 'elisp--preceding-sexp)
        'elisp--preceding-sexp
      'preceding-sexp))

(declare-function projectile-find-file "ext:projectile")
(declare-function projectile-find-file-other-window "ext:projectile")
(declare-function projectile-mode "ext:projectile")
(declare-function projectile-project-root "ext:projectile")
(defvar projectile-mode)
(declare-function find-file-in-project "ext:find-file-in-project")

(defun lispy-visit (arg)
  "Forward to find file in project depending on ARG."
  (interactive "p")
  (if (eq lispy-visit-method 'ffip)
      (find-file-in-project)
    (unless projectile-mode
      (projectile-mode 1))
    (cond ((= arg 1)
           (projectile-find-file nil))
          ((= arg 2)
           (projectile-find-file-other-window))
          (t
           (projectile-find-file arg)))))

(defun lispy-narrow (arg)
  "Narrow ARG sexps or region."
  (interactive "p")
  (cond ((region-active-p)
         (narrow-to-region (region-beginning) (region-end)))
        ((lispy-left-p)
         (narrow-to-region (point)
                           (save-excursion
                             (lispy-forward arg)
                             (point))))
        ((lispy-right-p)
         (narrow-to-region (point)
                           (save-excursion
                             (lispy-backward arg)
                             (point))))))

(defun lispy-widen ()
  "Forward to `widen'."
  (interactive)
  (widen))

(defun lispy-paste (arg)
  "Forward to `yank'.
If the region is active, replace instead of yanking.
When ARG is given, paste at that place in the current list."
  (interactive "p")
  (cond ((region-active-p)
         (let ((bnd (lispy--bounds-dwim)))
           (deactivate-mark)
           (lispy--maybe-safe-delete-region (car bnd)
                                            (cdr bnd))
           (insert (lispy--maybe-safe-current-kill))))
        ((> arg 1)
         (lispy-mark-car)
         (lispy-down (- arg 2))
         (deactivate-mark)
         (just-one-space)
         (insert (lispy--maybe-safe-current-kill))
         (unless (or (eolp) (looking-at lispy-right))
           (just-one-space)
           (forward-char -1)))
        ((lispy-right-p)
         (newline-and-indent)
         (insert (lispy--maybe-safe-current-kill)))
        ((lispy-left-p)
         (newline-and-indent)
         (forward-line -1)
         (lispy--indent-for-tab)
         (insert (lispy--maybe-safe-current-kill)))
        (t
         (insert (lispy--maybe-safe-current-kill)))))

(defalias 'lispy-font-lock-ensure
  (if (fboundp 'font-lock-ensure)
      'font-lock-ensure
    'font-lock-fontify-buffer))

(defun lispy--fontify (str mode)
  "Return STR fontified in MODE."
  (with-temp-buffer
    (funcall mode)
    (show-paren-mode)
    (insert str)
    (lispy-font-lock-ensure)
    (let ((color-paren (face-attribute 'show-paren-match :background))
          (color-cursor-fg (face-attribute 'lispy-cursor-face :foreground))
          (color-cursor-bg (face-attribute 'lispy-cursor-face :background))
          pt mk p1 p2)
      (goto-char (point-min))
      (when (search-forward "|" nil t)
        (backward-delete-char 1)
        (setq pt (point))
        (when (< (- (line-end-position) pt) 2)
          (end-of-line)
          (insert "  ")))
      (goto-char (point-min))
      (when (search-forward "~" nil t)
        (backward-delete-char 1)
        (setq mk (point))
        (when (< mk pt)
          (cl-decf pt)))
      (if pt
          (progn
            (goto-char pt)
            (cond ((lispy-right-p)
                   (setq p2 (1- (point)))
                   (lispy-other)
                   (setq p1 (point)))
                  ((lispy-left-p)
                   (setq p1 (point))
                   (lispy-other)
                   (setq p2 (1- (point)))))
            (when p2
              (save-excursion
                (goto-char p2)
                (when (< (- (line-end-position) p2) 2)
                  (end-of-line)
                  (insert " "))))
            (setq str (buffer-string))
            (add-face-text-property 0 (length str) '(face 'lispy-test-face) t str)
            (when mk
              (if (< mk pt)
                  (progn
                    (add-text-properties (1- mk) (1- pt) '(face region) str)
                    (set-text-properties (1- pt) pt '(face cursor) str))
                (add-text-properties (1- (min pt mk)) (1- (max pt mk)) '(face region) str)
                (set-text-properties (1- pt) pt '(face cursor) str)))
            (when p1
              (add-text-properties
               (1- p1) p1
               `(face (:background
                       ,color-paren
                       :foreground
                       ,(if (and mk
                                 (>= p1 (min pt mk))
                                 (<= p1 (max pt mk)))
                            color-cursor-fg
                          color-cursor-bg))) str))
            (when p2
              (add-text-properties
               (1- p2) p2
               `(face (:background
                       ,color-paren
                       :foreground
                       ,(if (and mk
                                 (>= p2 (min pt mk))
                                 (<= p2 (max pt mk)))
                            color-cursor-fg
                          color-cursor-bg)))
               str))
            (add-text-properties
             (1- pt) pt
             `(face (:background
                     ,color-cursor-bg
                     :foreground
                     ,(if (eq pt p1)
                          color-paren
                        color-cursor-fg)))
             str)
            str)
        str))))

(defun lispy-view-test ()
  "View better the test at point."
  (interactive)
  (cond ((and (overlayp lispy-overlay)
              (eq (point) (get 'lispy-overlay 'last-point)))
         (delete-overlay lispy-overlay)
         (setq lispy-overlay nil))

        ((looking-at "(should (\\(?:string=\\|equal\\)")
         (setq lispy-hint-pos (point))
         (let* ((expr (cadr (read (lispy--string-dwim))))
                (str1 (cadr (cadr expr)))
                (str2 (cl-caddr expr))
                (keys (cl-cddadr expr))
                (keys (if (and (= (length keys) 1)
                               (consp (car keys))
                               (eq (caar keys) 'execute-kbd-macro))
                          (cl-cadar keys)
                        keys))
                (sep (make-string (- (window-width)
                                     (current-column)) ?-))
                (mode (if (looking-at "[^\n]*(lispy-with clojure")
                          'clojure-mode
                        'emacs-lisp-mode)))
           (lispy--show
            (concat "\n"
                    (lispy--fontify str1 mode)
                    "\n" sep "\n"
                    (substring (prin1-to-string keys) 1 -1)
                    "\n" sep "\n"
                    (lispy--fontify (if (stringp str2)
                                        str2
                                      (prin1-to-string str2)) mode)
                    "\n"))))

        (t
         (lispy--complain "should position point before (should (string="))))

(defun lispy-map-done ()
  (interactive)
  (lispy-map-delete-overlay)
  (setq lispy-bind-var-in-progress nil)
  (lispy-backward 1))

(defun lispy-map-delete-overlay ()
  "Delete `lispy-map-input-overlay'."
  (when (overlayp lispy-map-input-overlay)
    (delete-overlay lispy-map-input-overlay)))

;;* Predicates
(defun lispy--in-string-p ()
  "Test if point is inside a string.
Return start of string it is."
  (let ((syn (syntax-ppss)))
    (or (and (nth 3 syn)
             (nth 8 syn))
        (and (eq (char-after) ?\")
             (not (eq ?\\ (char-before)))
             (point)))))

(defun lispy--in-comment-p ()
  "Test if point is inside a comment."
  (or
   (save-excursion
     (unless (eolp)
       (forward-char 1))
     (nth 4 (syntax-ppss)))
   (and (bolp) (looking-at lispy-outline-header))))

(defun lispy--in-string-or-comment-p ()
  "Test if point is inside a string or a comment."
  (let* ((sp (syntax-ppss))
         (beg (nth 8 sp)))
    (when (or (eq (char-after beg) ?\")
              (nth 4 sp))
      beg)))

(defun lispy--raw-comment-p (expr)
  "Return t if EXPR is a raw comment."
  (and (listp expr)
       (eq (car expr) 'ly-raw)
       (consp (cdr expr))
       (eq (cadr expr) 'comment)))

(defun lispy--raw-string-p (expr)
  "Return t if EXPR is a raw comment."
  (and (listp expr)
       (eq (car expr) 'ly-raw)
       (consp (cdr expr))
       (eq (cadr expr) 'string)))

(defun lispy--leftp ()
  "Return t if at region beginning, or at start of the list."
  (if (region-active-p)
      (= (point) (region-beginning))
    (or (lispy-left-p)
        (looking-at lispy-outline))))

(defun lispy--symbolp (str)
  "Return t if STR is a symbol."
  (string-match "\\`\\(?:\\sw\\|\\s_\\)+\\'" str))

(defun lispy--string-markedp ()
  "Return t if the current active region is a string."
  (and (region-active-p)
       (eq ?\" (char-after (region-beginning)))
       (eq ?\" (char-before (region-end)))))

(defun lispy-bolp ()
  "Return t if point is at beginning of line, after optional spaces."
  (save-excursion
    (skip-chars-backward " \t")
    (bolp)))

(defun lispy-after-string-p (str)
  "Return t if the string before point is STR."
  (string=
   (buffer-substring
    (max
     (- (point) (length str))
     (point-min))
    (point))
   str))

(defun lispy--empty-line-p ()
  "Test whether the point is on an \"empty\" line.
Return t if the point is by itself on a line with optional whitespace.
Return 'right if the point is on a line with only right delimiters and
whitespace."
  (if (and (looking-at (concat "[[:space:]]*" lispy-right "*$"))
           (lispy-looking-back "^[[:space:]]*"))
      (if (looking-at (concat "[[:space:]]*" lispy-right))
          'right
        t)
    nil))

(defun lispy--preceding-syntax (preceding-syntax-alist &optional before after)
  "Return a regexp corresponding to valid syntax that can precede delimiters.
This is done by checking PRECEDING-SYNTAX-ALIST for the current major mode.
Return nil if there is no entry for the current major mode. When there is an
entry, prepend BEFORE and append AFTER to the regexp when they are specified."
  (let ((regexps (or (cdr (assoc major-mode preceding-syntax-alist))
                     (cdr (assoc t preceding-syntax-alist)))))
    (when regexps
      (concat before
              "\\(?:"
              (apply #'concat
                     (lispy-interleave
                      "\\|"
                      regexps))
              "\\)"
              after))))

(defun lispy--in-empty-list-p (preceding-syntax-alist)
  "Test whether the point is in a list with no sexps.
A list with only characters that can precede a delimiter (e.g. \"`(,)\") is
consider an empty list."
  (and (lispy-looking-back
        (concat lispy-left
                "[[:space:]]*"
                (lispy--preceding-syntax preceding-syntax-alist nil "*")))
       (looking-at (concat "[[:space:]]*" lispy-right))))

(defun lispy--not-at-sexp-p (preceding-syntax-alist)
  "Test whether the point is at a \"free\" spot and not at a wrappable sexp.
PRECEDING-SYNTAX-ALIST should be an alist of `major-mode' to a list of regexps.
The regexps correspond to valid syntax that can precede an opening delimiter in
each major mode."
  (let* ((space "[[:space:]]")
         (space-or-eol (concat "\\(" space "+\\|" space "*$\\)"))
         (right-or-eol (concat "\\(" lispy-right "+\\|" space "*$\\)"))
         (special-syntax (lispy--preceding-syntax preceding-syntax-alist))
         (line (buffer-substring-no-properties
                (line-beginning-position)
                (line-end-position))))
    (or (lispy--in-empty-list-p preceding-syntax-alist)
        ;; empty line
        (string-match (concat "^" space "*" special-syntax "*" space "*$")
                      line)
        ;; empty position at end of list or line
        (and (looking-at right-or-eol)
             (lispy-looking-back (concat space "+" special-syntax "*")))
        ;; empty position at beginning of list
        (and (looking-at space-or-eol)
             (lispy-looking-back (concat lispy-left special-syntax "*")))
        ;; empty position in middle
        (and (looking-at (concat space "+"))
             (lispy-looking-back (concat space "+" special-syntax "*"))))))

;;* Pure
(declare-function lispy-bounds-python-block "le-python")

(defun lispy--bounds-dwim ()
  "Return a cons of region bounds if it's active.
Otherwise return cons of current string, symbol or list bounds."
  (let (bnd)
    (cond ((region-active-p)
           (cons (region-beginning)
                 (region-end)))
          ((and (setq bnd (lispy--bounds-string))
                (or (eq (point) (car bnd))
                    (eq (point) (1- (cdr bnd)))))
           bnd)
          ((looking-at lispy-outline)
           (save-excursion
             (cons
              (progn
                (outline-end-of-heading)
                (1+ (point)))
              (progn
                (outline-end-of-subtree)
                (skip-chars-backward "\n")
                (when (setq bnd (lispy--bounds-comment))
                  (goto-char (1- (car bnd))))
                (point)))))
          ((save-excursion
             (when (lispy-right-p)
               (backward-list))
             (and (or (looking-at (concat "[^[:space:]\n]*" lispy-left))
                      (looking-at "[`'#]"))
                  (setq bnd (bounds-of-thing-at-point 'sexp))))
           (save-excursion
             (goto-char (car bnd))
             (lispy--skip-delimiter-preceding-syntax-backward)
             (cons (point) (cdr bnd))))
          ((looking-at (lispy-comment-char 2))
           (lispy--bounds-comment))
          ((and (eq major-mode 'python-mode)
                (lispy-bolp))
           (lispy-bounds-python-block))
          (t
           (let ((res (ignore-errors
                        (bounds-of-thing-at-point
                         (if (looking-at lispy-right)
                             'symbol
                           'sexp)))))
             (if res
                 (save-excursion
                   (goto-char (cdr res))
                   (lispy--in-string-or-comment-p)
                   (skip-chars-backward "[.,]")
                   (cons (car res) (point)))
               (or
                (ignore-errors
                  (bounds-of-thing-at-point 'symbol))
                (and (lispy-looking-back "\" *")
                     (save-excursion
                       (goto-char (match-beginning 0))
                       (lispy--bounds-string)))
                (ignore-errors
                  (bounds-of-thing-at-point 'sentence))
                (ignore-errors
                  (save-excursion
                    (backward-word 1)
                    (bounds-of-thing-at-point 'symbol)))
                (ignore-errors
                  (save-excursion
                    (forward-word 1)
                    (bounds-of-thing-at-point 'symbol))))))))))

(declare-function python-nav-end-of-statement "python")

(defun lispy--bounds-list ()
  "Return the bounds of smallest list that includes the point."
  (save-excursion
    (lispy--exit-string)
    (when (looking-at lispy-left)
      (forward-char))
    (when (lispy-looking-back lispy-right)
      (backward-char))
    (ignore-errors
      (let (beg end)
        (up-list)
        (setq end (point))
        (backward-list)
        (setq beg (point))
        (cons beg end)))))

(defun lispy--bounds-string ()
  "Return bounds of current string."
  (unless (lispy--in-comment-p)
    (let ((beg (or (nth 8 (syntax-ppss))
                   (and (eq (char-after (point)) ?\")
                        (not (eq ?\\ (char-before)))
                        (point)))))
      (when (and beg (not (comment-only-p beg (1+ (point)))))
        (ignore-errors
          (cons beg (save-excursion
                      (goto-char beg)
                      (forward-sexp)
                      (point))))))))

(defun lispy--bounds-comment ()
  "Return bounds of current comment."
  (and (lispy--in-comment-p)
       (save-excursion
         (when (lispy--beginning-of-comment)
           (let ((pt (point)))
             (while (and (lispy--in-comment-p)
                         (forward-comment -1)
                         (lispy-looking-back "^[[:space:]]*")
                         (= 1 (- (count-lines (point) pt)
                                 (if (bolp) 0 1))))
               (setq pt (point)))
             (goto-char pt))
           (if (looking-at "#|")
               (cons (point)
                     (progn
                       (comment-forward)
                       (point)))
             (let ((beg (lispy--beginning-of-comment))
                   (pt (point))
                   (col (current-column)))
               (while (and (lispy--in-comment-p)
                           (forward-comment 1)
                           (lispy--beginning-of-comment)
                           (and (= 1 (- (count-lines pt (point))
                                        (if (bolp) 0 1)))
                                ;; count comments starting in different columns
                                ;; as separate
                                (= col (current-column))
                                ;; if there's code in between,
                                ;; count comments as separate
                                (lispy-looking-back "^\\s-*")))
                 (setq pt (point)))
               (goto-char pt)
               (end-of-line)
               (cons beg (point))))))))

(defun lispy--string-dwim (&optional bounds)
  "Return the string that corresponds to BOUNDS.
`lispy--bounds-dwim' is used if BOUNDS is nil."
  (setq bounds (or bounds (lispy--bounds-dwim)))
  (buffer-substring-no-properties (car bounds) (cdr bounds)))

(declare-function python-info-current-symbol "python")

(defun lispy--current-function ()
  "Return current function as string."
  (if (region-active-p)
      (let ((str (lispy--string-dwim)))
        (if (string-match "\\`[#'`]*\\(.*?\\)'?\\'" str)
            (match-string 1 str)
          nil))
    (save-excursion
      (if (eq major-mode 'python-mode)
          (let ((bnd (bounds-of-thing-at-point 'symbol)))
            (if bnd
                (lispy--string-dwim bnd)
              (up-list -1)
              (python-info-current-symbol)))
        (lispy--back-to-paren)
        (when (looking-at "(\\([^ \n)]+\\)[ )\n]")
          (match-string-no-properties 1))))))

(defun lispy--prin1-fancy (x)
  "Return a propertized `prin1-to-string'-ed X."
  (propertize (prin1-to-string x)
              'face 'font-lock-constant-face))

;;* Utilities: movement
(defun lispy--out-forward (arg &optional ignore-ws)
  "Move outside list forwards ARG times.
Return nil on failure, (point) otherwise."
  (lispy--exit-string)
  (catch 'break
    (dotimes (_i arg)
      (if (ignore-errors (up-list) t)
          (if buffer-read-only
              (deactivate-mark)
            (unless (or ignore-ws lispy-ignore-whitespace)
              (lispy--remove-gaps)
              (lispy--indent-for-tab)))
        (when (lispy-left-p)
          (forward-list))
        (throw 'break nil)))
    (point)))

(defun lispy--out-backward (arg &optional ignore-ws)
  "Move outside list forwards ARG times.
Return nil on failure, t otherwise."
  (let ((oldpt (point))
        newpt)
    (lispy--out-forward arg ignore-ws)
    (when (lispy-right-p)
      (forward-list -1))
    (if (= oldpt (setq newpt (point)))
        nil
      newpt)))

(defun lispy--back-to-paren ()
  "Move to ( going out backwards."
  (let ((lispy-ignore-whitespace t))
    (lispy--exit-string)
    (while (and (not (looking-at "("))
                (lispy--out-backward 1)))))

(defun lispy--exit-string ()
  "When in string, go to its beginning."
  (let ((s (syntax-ppss)))
    (when (nth 3 s)
      (goto-char (nth 8 s)))))

(defun lispy--beginning-of-comment ()
  "Go to beginning of comment on current line."
  (end-of-line)
  (comment-beginning)
  (let ((cs (comment-search-backward (line-beginning-position) t)))
    (or
     (when cs
       (goto-char cs))
     (and (looking-at (concat "^" lispy-outline-header))
          (point)))))

(defun lispy--skip-delimiter-preceding-syntax-backward ()
  "Move backwards past syntax that could precede an opening delimiter such as '.
Specifically, move backwards to the closest whitespace char or opening delimiter
or to the beginning of the line."
  (re-search-backward (concat "[[:space:]]" "\\|"
                              lispy-left "\\|"
                              "^"))
  (goto-char (match-end 0)))

;;* Utilities: evaluation
(defun lispy--eval (e-str)
  "Eval E-STR according to current `major-mode'.
The result is a string."
  (let ((handler (cdr (cl-find-if
                       (lambda (x)
                         (if (listp (car x))
                             (memq major-mode (car x))
                           (eq major-mode (car x))))
                       lispy-eval-alist))))
    (if handler
        (progn
          (setq e-str
                (or e-str (if (> (length handler) 2)
                              (funcall (nth 2 handler))
                            (save-excursion
                              (unless (or (lispy-right-p) (region-active-p))
                                (lispy-forward 1))
                              (lispy--string-dwim)))))
          (require (nth 0 handler))
          (funcall (nth 1 handler) e-str))
      (error "%s isn't supported currently" major-mode))))

(defun lispy-eval-expression ()
  "Like `eval-expression', but for current language."
  (interactive)
  (let ((form (minibuffer-with-setup-hook
                  (if (member major-mode lispy-elisp-modes)
                      #'lispy-mode
                    #'ignore)
                (read-from-minibuffer "Eval: "))))
    (lispy-message (lispy--eval form))))

(defvar lispy-eval-match-data nil)

(defun lispy--eval-elisp-form (lispy-form lexical)
  "Eval LISPY-FORM and return its value.
If LEXICAL is t, evaluate using lexical scoping.
Restore and save `lispy-eval-match-data' appropriately,
so that no other packages disturb the match data."
  (let (val)
    (unwind-protect
         (progn
           (fset '\, #'identity)
           (set-match-data lispy-eval-match-data)
           (setq val (eval lispy-form lexical))
           (setq lispy-eval-match-data (match-data)))
      (fset '\, nil))
    val))

(defalias 'lispy-eval-defun-1
    (if (fboundp 'eval-defun-1)
        'eval-defun-1
      'elisp--eval-defun-1))

(defun lispy--eval-elisp (e-str)
  "Eval E-STR as Elisp code."
  (let ((e-sexp (read e-str)))
    (when (consp e-sexp)
      (cond ((and (memq (car e-sexp) '(defvar defcustom defvar-local))
                  (consp (cdr e-sexp))
                  (boundp (cadr e-sexp)))
             (set (cadr e-sexp) (eval (cl-caddr e-sexp))))
            ((eq (car e-sexp) 'defface)
             (lispy-eval-defun-1 (macroexpand e-sexp)))
            ((memq (car e-sexp) '(\, \,@))
             (setq e-sexp (cadr e-sexp)))))
    (condition-case e
        (prin1-to-string
         (lispy--eval-elisp-form e-sexp lexical-binding))
      (error
       (progn
         (fset '\, nil)
         (let ((es (error-message-string e)))
           (if (and lispy-lax-eval
                    (string-match
                     "^Symbol's value as variable is void: \\(.*\\)$"
                     es))
               (progn
                 (setq es (match-string 1 es))
                 (set (intern es) nil)
                 (message "Caught unbound variable %s, setting it to nil." es))
             (signal (car e) (cdr e)))))))))

(defun lispy-follow ()
  "Find the definition for the symbol at point or at the next char.

Relies on `xref-find-definitions`."
  (interactive)
  ;; HACK: Prevent this `xref` command from prompting for input.
  ;; The issue boils down to `xref--prompt-p`, which decides if the
  ;; prompt should be displayed. If `xref-prompt-for-identifier` is
  ;; a list and starts with `not`, the subsequent values represent a
  ;; whitelist. We add `special-lispy-follow` to that list, since this
  ;; is the value of `this-command` when `xref-prompt-for-identifier`
  ;; is invoked with `call-interactively`.
  (when (and (listp xref-prompt-for-identifier)
             (eq 'not (car xref-prompt-for-identifier)))
    (add-to-list 'xref-prompt-for-identifier 'special-lispy-follow 't))
  (when (buffer-narrowed-p)
    (widen))
  (cond ((lispy-left-p)
         (let ((next-symbol (save-excursion
                              (forward-char 1)
                              (thing-at-point 'symbol 't))))
           (xref-find-definitions next-symbol)))
        ((region-active-p)
         (deactivate-mark)
         (call-interactively 'xref-find-definitions))
        ('t
         (call-interactively 'xref-find-definitions))))

;;* Utilities: slurping and barfing
(defun lispy--slurp-forward ()
  "Grow current sexp forward by one sexp."
  (let ((pt (point))
        (char (char-before)))
    (skip-chars-forward " \t")
    (delete-region pt (point))
    (unless (or (lispy-after-string-p "()")
                (lispy-after-string-p "[]")
                (lispy-after-string-p "{}")
                (eolp))
      (insert " "))
    (when (ignore-errors
            (forward-sexp) t)
      (delete-region (1- pt) pt)
      (insert char))))

(defun lispy--slurp-backward ()
  "Grow current sexp backward by one sexp."
  (let ((pt (point))
        (char (char-after)))
    (backward-sexp)
    (delete-region pt (1+ pt))
    (insert char)
    (backward-char)))

(defun lispy--barf-forward ()
  "Shrink current sexp forward by one sexp."
  (let ((pt (point))
        (char (char-after)))
    (unless (looking-at "()")
      (forward-char)
      (forward-sexp)
      (delete-region pt (1+ pt))
      (skip-chars-forward " \n	")
      (insert char)
      (backward-char)
      (indent-region pt (point))
      (lispy--reindent 1))))

(defun lispy--barf-backward ()
  "Shrink current sexp backward by one sexp."
  (let ((pt (point))
        (char (char-before)))
    (unless (lispy-after-string-p "()")
      (backward-char)
      (backward-sexp)
      (skip-chars-backward " \n	")
      (while (lispy--in-comment-p)
        (goto-char (comment-beginning))
        (skip-chars-backward " \n	"))
      (delete-region (1- pt) pt)
      (insert char)
      (lispy--indent-region (point) pt))))

(defun lispy--replace-regexp-in-code (regexp to-string)
  "Replace text matching REGEXP with TO-STRING in whole buffer.
Ignore the matches in strings and comments."
  (goto-char (point-min))
  (while (re-search-forward regexp nil t)
    (unless (lispy--in-string-or-comment-p)
      (replace-match to-string))))

;;* Utilities: source transformation
(defvar lispy--braces-table
  (let ((table (make-char-table 'syntax-table nil)))
    (modify-syntax-entry ?\{ "(}  " table)
    (modify-syntax-entry ?\} "){  " table)
    (modify-syntax-entry ?\[ "(]  " table)
    (modify-syntax-entry ?\] ")[  " table)
    (modify-syntax-entry ?\( "()  " table)
    (modify-syntax-entry ?\) ")(  " table)
    table)
  "Syntax table for paired braces.")

(defvar lispy--insert-replace-alist-clojure
  '(("#object[" "clojure-object")
    ("#?@(" "clojure-reader-conditional-splice")
    ("@(" "clojure-deref-list")
    ("#(" "clojure-lambda")
    ("#{" "clojure-set")
    ("@{" "clojure-deref-map")
    ("@[" "clojure-deref-vector")
    ("{" "clojure-map")
    ("#?(" "clojure-reader-conditional")))

(defvar lispy--insert-replace-alist-elisp
  '(("#object[" "clojure-object")
    ("#?@(" "clojure-reader-conditional-splice")
    ("#(" "clojure-lambda")
    ("#?(" "clojure-reader-conditional")))

(defun lispy--read-1 ()
  (let* ((alist (if (member major-mode lispy-elisp-modes)
                    lispy--insert-replace-alist-elisp
                  lispy--insert-replace-alist-clojure))
         (regex (regexp-opt (mapcar #'car alist))))
    (goto-char (point-min))
    (while (re-search-forward regex nil t)
      (let* ((head-beg (match-beginning 0))
             (head-end (match-end 0))
             (head (match-string 0))
             (entry (assoc head alist))
             (class (cadr entry))
             str-mid)
        (unless (lispy--in-string-or-comment-p)
          (backward-char 1)
          (save-excursion
            (if (save-match-data
                  (looking-at "((ly-raw string"))
                (forward-list 1)
              (with-syntax-table lispy--braces-table
                (forward-list 1)))
            (setq str-mid (buffer-substring-no-properties head-end (1- (point))))
            (delete-region head-beg (point)))
          (insert "(ly-raw " class " (" str-mid "))")
          (backward-char (+ 3 (length str-mid))))))))

(defvar lispy--clojure-char-literal-regex
  (format "\\\\\\(\\(?:\\(?:\\sw\\|%s\\)\\b\\)\\|[.,!@#$%%&*]\\|u[A-Za-z0-9]+\\)"
          (regexp-opt '("newline" "space" "tab" "formfeed" "backspace" "return")))
  "Regex for Clojure character literals.
See https://clojure.org/guides/weird_characters#_character_literal.")

(defun lispy--read-replace (regex class &optional subexp)
  (setq subexp (or subexp 0))
  (goto-char (point-min))
  (while (re-search-forward regex nil t)
    (cond ((string= (match-string 0) "ly-raw")
           (if (looking-at " clojure-\\(map\\|set\\|lambda\\)")
               (goto-char (match-end 0))
             (up-list)))
          ((lispy--in-string-or-comment-p))
          (t
           (replace-match
            (format "(ly-raw %s %S)"
                    class
                    (substring-no-properties
                     (match-string subexp)))
            t t nil subexp)))))

;; TODO: Make the read test pass on string with semi-colon
(defun lispy--read (str)
  "Read STR including comments and newlines."
  (let* ((deactivate-mark nil)
         (mode major-mode)
         cbnd
         (str (with-temp-buffer
                (funcall mode)
                (insert str)
                ;; ——— ly-raw —————————————————
                (lispy--replace-regexp-in-code "(ly-raw" "(ly-raw raw")
                ;; ——— comments ———————————————
                (goto-char (point-min))
                (while (comment-search-forward (point-max) t)
                  (lispy--beginning-of-comment)
                  (setq cbnd (cons (point) (line-end-position)))
                  (setq str (lispy--string-dwim cbnd))
                  (delete-region (car cbnd) (cdr cbnd))
                  (insert (format "(ly-raw comment %S)" str)))
                ;; ——— reader macro syntax (LISP)
                (goto-char (point-min))
                (while (re-search-forward "#[a-z][\"(]" nil t)
                  (forward-char -1)
                  (unless (lispy--in-string-or-comment-p)
                    (let ((beg (match-beginning 0))
                          rep)
                      (forward-sexp 1)
                      (setq rep (format "(ly-raw lisp-macro %S)"
                                        (buffer-substring-no-properties
                                         beg (point))))
                      (delete-region beg (point))
                      (insert rep))))
                ;; ——— strings ————————————————
                (goto-char (point-min))
                (while (re-search-forward "\"" nil t)
                  (progn
                    (setq cbnd (lispy--bounds-string))
                    (when cbnd
                      (if (or (lispy-after-string-p "ly-raw comment \"")
                              (lispy-after-string-p "ly-raw lisp-macro \""))
                          (goto-char (cdr cbnd))
                        (setq str (lispy--string-dwim cbnd))
                        (delete-region (car cbnd) (cdr cbnd))
                        (insert (format "(ly-raw string %S)" str))))))
                ;; ——— newlines ———————————————
                (lispy--replace-regexp-in-code "\n" " (ly-raw newline)")
                ;; ——— numbers ————————————————
                (goto-char (point-min))
                (while (re-search-forward "\\b[+-]?[0-9]+\\(?:\\.[0-9]+\\)?\\(?:e[+-]?[0-9]*\\)" nil t)
                  (if (setq cbnd (lispy--bounds-string))
                      (goto-char (cdr cbnd))
                    (let ((s (match-string-no-properties 0)))
                      (delete-region (match-beginning 0) (match-end 0))
                      (insert (format "(ly-raw float \"%s\")" s)))))
                ;; ——— () —————————————————————
                (goto-char (point-min))
                (while (re-search-forward "\\(?:[^\\]\\|^\\)\\(()\\)" nil t)
                  (unless (lispy--in-string-or-comment-p)
                    (replace-match "(ly-raw empty)" nil nil nil 1)))
                ;; ——— \ char syntax (Clojure)—
                (when (eq major-mode 'clojure-mode)
                  (lispy--read-replace lispy--clojure-char-literal-regex "clojure-char"))
                ;; namespaced map #520
                (when (memq major-mode lispy-clojure-modes)
                  (goto-char (point-min))
                  (while (re-search-forward "#\\(?:js\\|:\\(?:\\sw\\|\\s_\\)+\\) *\\(?:{\\|\\[\\)" nil t)
                    (let* ((head-beg (match-beginning 0))
                           (head-end (match-end 0))
                           (head (match-string 0))
                           str-mid tail)
                      (unless (lispy--in-string-or-comment-p)
                        (backward-char 1)
                        (save-excursion
                          (with-syntax-table lispy--braces-table
                            (forward-list 1))
                          (setq str-mid (buffer-substring-no-properties head-end (1- (point))))
                          (setq tail (buffer-substring-no-properties (1- (point)) (point)))
                          (delete-region head-beg (point)))
                        (insert
                         (format
                          "(ly-raw splice \"%s\" \"%s\" (%s))"
                          (replace-regexp-in-string " +" "" (substring-no-properties head))
                          tail
                          str-mid))
                        (backward-char (+ 3 (length str-mid)))))))
                ;; ——— #{ or { or #( or @( or #?( or #?@( ——————————
                (lispy--read-1)
                ;; ——— ? char syntax ——————————
                (goto-char (point-min))
                (if (memq major-mode (cons 'hy-mode lispy-clojure-modes))
                    (lispy--read-replace "[[:alnum:]-/*<>_?.,\\\\:!@#=]+" "clojure-symbol")
                  (while (re-search-forward "\\(?:\\s-\\|\\s(\\)\\?" nil t)
                    (unless (lispy--in-string-or-comment-p)
                      (let ((pt (point))
                            sexp)
                        (lispy--skip-elisp-char)
                        (setq sexp (buffer-substring-no-properties pt (point)))
                        (delete-region (1- pt) (point))
                        (insert (format "(ly-raw char %S)" sexp))))))
                (when (eq major-mode 'clojure-mode)
                  (lispy--read-replace " *,+" "clojure-commas"))
                ;; ——— \ char syntax (LISP)————
                (goto-char (point-min))
                (while (re-search-forward "#\\\\\\(.\\)" nil t)
                  (unless (lispy--in-string-or-comment-p)
                    (replace-match (format "(ly-raw lisp-char %S)"
                                           (substring-no-properties
                                            (match-string 0)))
                                   nil t)))
                ;; ——— Clojure gensym —————————
                (goto-char (point-min))
                (while (re-search-forward "\\([a-zA-Z][a-zA-z-/_0-9]*#\\)[ \t\n\r]" nil t)
                  (unless (lispy--in-string-or-comment-p)
                    (replace-match
                     (format "(ly-raw clojure-gensym %S)"
                             (match-string-no-properties 1))
                     t nil nil 1)))
                ;; ——— Clojure keyword —————————
                (goto-char (point-min))
                (while (re-search-forward "\\(:\\.[^][({}) \t\n\r\"]+\\)" nil t)
                  (unless (lispy--in-string-or-comment-p)
                    (replace-match (format "(ly-raw clojure-keyword %S)"
                                           (match-string-no-properties 1)))))
                ;; ——— #' —————————————————————
                (goto-char (point-min))
                (while (re-search-forward "#'" nil t)
                  (unless (lispy--in-string-or-comment-p)
                    (forward-sexp)
                    (insert ")")
                    (replace-match "(ly-raw function ")))
                ;; ——— ,@ —————————————————————
                (goto-char (point-min))
                (while (re-search-forward "\\(?:[^\\]\\|^\\),@" nil t)
                  (unless (lispy--in-string-or-comment-p)
                    (backward-char 2)
                    (let ((beg (point))
                          (sxp (ignore-errors (read (current-buffer)))))
                      (when (and (consp sxp)
                                 (eq (car sxp) '\,@))
                        (insert ")")
                        (goto-char beg)
                        (delete-char 2)
                        (insert "(ly-raw comma-splice ")))))
                ;; ——— #_ —————————————————————
                (goto-char (point-min))
                (while (re-search-forward "#_[({[]" nil t)
                  (if (setq cbnd (lispy--bounds-string))
                      (goto-char (cdr cbnd))
                    (backward-char 1)
                    (let ((beg (point)))
                      (forward-list 1)
                      (insert ")")
                      (goto-char beg)
                      (delete-char -2)
                      (insert "(ly-raw clojure-reader-comment "))))
                ;; ——— #1 —————————————————————
                ;; Elisp syntax for circular lists
                (goto-char (point-min))
                (while (re-search-forward "\\(?:^\\|\\s-\\|\\s(\\)\\(#[0-9]+\\)" nil t)
                  (unless (lispy--in-string-p)
                    (replace-match (format "(ly-raw reference %S)"
                                           (substring-no-properties
                                            (match-string 1)))
                                   nil nil nil 1)))
                ;; ——— ' ——————————————————————
                (goto-char (point-min))
                (while (re-search-forward "'" nil t)
                  (unless (lispy--in-string-or-comment-p)
                    (backward-char 1)
                    (let ((beg (point))
                          (sxp (ignore-errors (read (current-buffer)))))
                      (when (and (consp sxp)
                                 (eq (car sxp) 'quote))
                        (insert ")")
                        (goto-char beg)
                        (delete-char 1)
                        (insert "(ly-raw quote ")))))
                ;; ——— ` ——————————————————————
                (goto-char (point-min))
                (while (re-search-forward "\\(?:[^\\]\\|^\\)`" nil t)
                  (unless (lispy--in-string-or-comment-p)
                    (cond ((looking-at lispy-left)
                           (delete-char -1)
                           (insert "(ly-raw \\` ")
                           (forward-list 1)
                           (insert ")")
                           (backward-list 1)
                           (forward-char 7))
                          ((looking-at "\\sw\\|\\s_\\|[,@]")
                           (let ((beg (point)))
                             (forward-sexp 1)
                             (insert "\")")
                             (goto-char (1- beg))
                             (insert "(ly-raw quasiquote \""))))))
                ;; ——— , ——————————————————————
                (lispy--replace-regexp-in-code "\\\\," "(ly-raw comma-symbol)")
                (goto-char (point-min))
                (while (re-search-forward "[^\\]?,[^@\"]" nil t)
                  (unless (lispy--in-string-or-comment-p)
                    (backward-char 2)
                    (if (memq major-mode lispy-clojure-modes)
                        (progn
                          (delete-char 1)
                          (insert "(ly-raw clojure-comma)"))
                      (let ((beg (point))
                            (sxp (ignore-errors (read (current-buffer)))))
                        (when (and (consp sxp)
                                   (eq (car sxp) '\,))
                          (insert ")")
                          (goto-char beg)
                          (delete-char 1)
                          (insert "(ly-raw \\, "))))))
                ;; ——— angle syntax —————————
                ;; used for markers/buffers/windows/overlays
                (goto-char (point-min))
                (while (re-search-forward "#<" nil t)
                  (unless (lispy--in-string-or-comment-p)
                    (delete-region (match-beginning 0) (match-end 0))
                    (insert "(ly-raw angle \"")
                    (re-search-forward ">")
                    (backward-delete-char 1)
                    (insert "\")")))
                ;; ——— cons cell syntax ———————
                (lispy--replace-regexp-in-code " \\. " " (ly-raw dot) ")
                ;; Racket stuff
                (lispy--replace-regexp-in-code "#t" "(ly-raw racket-true)")
                (lispy--replace-regexp-in-code "#f" "(ly-raw racket-false)")
                (goto-char (point-min))
                (while (re-search-forward "#:\\(\\(?:\\sw\\|\\s_\\)+\\)" nil t)
                  (unless (lispy--in-string-or-comment-p)
                    (replace-match (format "(ly-raw racket-option %s)"
                                           (match-string 1)))))
                ;; Clojure # in a symbol
                (goto-char (point-min))
                (while (re-search-forward "\\_<\\(?:\\sw\\|\\s_\\)+\\_>" nil t)
                  (unless (lispy--in-string-p)
                    (when (cl-position ?# (match-string 0))
                      (let* ((bnd (lispy--bounds-dwim))
                             (str (lispy--string-dwim bnd)))
                        (delete-region (car bnd) (cdr bnd))
                        (insert (format "(ly-raw symbol %S)" str))))))
                ;; Clojure (. object method)
                (goto-char (point-min))
                (while (re-search-forward "(\\.[\t\n ]" nil t)
                  (if (setq cbnd (lispy--bounds-string))
                      (goto-char (cdr cbnd))
                    (forward-char -1)
                    (delete-char -1)
                    (insert "(ly-raw clojure-dot)")))
                ;; ———  ———————————————————————
                (buffer-substring-no-properties
                 (point-min)
                 (point-max)))))
    (ignore-errors
      (read str))))

(defun lispy--skip-elisp-char ()
  (unless (lispy-after-string-p "?")
    (error "unexpected"))
  (if (looking-at "\\\\")
      (forward-sexp 1)
    (forward-char 1)))

(defun lispy--whitespacep (x)
  "Check if X is a whitespace tag."
  (and (consp x)
       (eq (car x) 'ly-raw)
       (or (eq (cadr x) 'newline)
           (eq (cadr x) 'comment))))

(unless (fboundp 'define-error)
  (defun define-error (name message &optional parent)
    "Define NAME as a new error signal.
MESSAGE is a string that will be output to the echo area if such an error
is signaled without being caught by a `condition-case'.
PARENT is either a signal or a list of signals from which it inherits.
Defaults to `error'."
    (unless parent (setq parent 'error))
    (let ((conditions
           (if (consp parent)
               (apply #'nconc
                      (mapcar
                       (lambda (parent)
                         (cons parent
                               (or (get parent 'error-conditions)
                                   (error "Unknown signal `%s'" parent))))
                       parent))
             (cons parent (get parent 'error-conditions)))))
      (put name 'error-conditions
           (delete-dups (copy-sequence (cons name conditions))))
      (when message (put name 'error-message message)))))

(define-error 'unsupported-mode-error "Unsupported mode")
(defun lispy--function-str (fun)
  "Return FUN definition as a string."
  (if (fboundp fun)
      (condition-case e
          (let* ((fnd
                  (save-window-excursion
                    (save-excursion
                      (find-function-noselect fun))))
                 (buf (car fnd))
                 (pt (cdr fnd)))
            (with-current-buffer buf
              (if (derived-mode-p
                   'emacs-lisp-mode
                   'clojure-mode
                   'lisp-mode
                   'scheme-mode)
                  (progn
                    (goto-char pt)
                    (lispy--string-dwim))
                (signal 'unsupported-mode-error major-mode))))
        (unsupported-mode-error
         (signal (car e) (cdr e)))
        (error
         (prin1-to-string (symbol-function fun))))
    (error "%s isn't bound" fun)))

(defun lispy--function-parse (str)
  "Extract the function body and args from it's expression STR."
  (let ((body (lispy--read str))
        args)
    (cond ((eq (car body) 'lambda)
           (setq body (cons 'defun body)))
          ((eq (car body) 'closure)
           (setq body `(defun noname ,@(cddr body))))
          ((eq (car body) 'defsubst)
           (setq body (cons 'defun (cdr body)))))
    (cond ((memq (car body) '(defun defmacro))
           (setq body (lispy--whitespace-trim (cdr body))))
          ((eq (car body) 'defalias)
           (let ((name (cadr (cadr (read str)))))
             (setq body
                   (cons name (cdr (symbol-function name))))))
          (t
           (error "Expected defun, defmacro, or defalias got %s" (car body))))
    (if (symbolp (car body))
        (setq body (lispy--whitespace-trim (cdr body)))
      (error "Expected function name, got %s" (car body)))
    (if (listp (car body))
        (progn
          (setq args (car body))
          (setq body (lispy--whitespace-trim (cdr body))))
      (error "Expected function arguments, got %s" (car body)))
    ;; skip docstring
    (if (and (listp (car body))
             (eq (caar body) 'ly-raw)
             (eq (cadar body) 'string))
        (setq body (lispy--whitespace-trim (cdr body))))
    ;; skip declare
    (if (and (listp (car body))
             (eq (caar body) 'declare))
        (setq body (lispy--whitespace-trim (cdr body))))
    ;; skip interactive
    (if (and (listp (car body))
             (eq (caar body) 'interactive))
        (setq body (lispy--whitespace-trim (cdr body))))
    (list args body)))

(defun lispy--flatten-function (fstr e-args)
  "Return body of FSTR with args replaced by E-ARGS."
  (let* ((p (lispy--function-parse fstr))
         (f-args (car p))
         (body (cadr p))
         f-arg)
    (when (equal f-args '(ly-raw empty))
      (setq f-args nil))
    (while (setq f-arg (pop f-args))
      (cond ((eq f-arg '&rest)
             (setq f-arg (pop f-args))
             (when f-args
               (error "&rest must be last"))
             (setq body (lispy--replace body f-arg (cons 'list e-args))))
            ((eq f-arg '&optional)
             (setq f-arg (pop f-args))
             (setq body (lispy--replace body f-arg (pop e-args))))
            (t
             (setq body (lispy--replace body f-arg (pop e-args))))))
    (if (= (length body) 1)
        (setq body (car body))
      (setq body (cons 'progn body)))))

(defun lispy--fast-insert (f-expr)
  "`lispy--insert' F-EXPR into a temp buffer and return `buffer-string'."
  (insert
   (with-temp-buffer
     (emacs-lisp-mode)
     (lispy--insert f-expr)
     (buffer-string))))

(defun lispy--case->if (case &optional else)
  "Return an if statement based on  CASE statement and ELSE."
  (append
   `(if ,(car case))
   (cond ((null (cdr case)) `((ly-raw newline) nil ,@else))
         ((= (length (cl-remove-if #'lispy--whitespacep (cdr case))) 1)
          (append (cdr case) else))
         (t
          (let ((p (or (cl-position-if-not
                        #'lispy--whitespacep
                        (cdr case))
                       -1)))
            `(,@(cl-subseq (cdr case) 0 p)
                (progn
                  (ly-raw newline)
                  ,@(cl-subseq (cdr case) p))
                ,@else))))))

(defun lispy--cases->ifs (cases)
  "Return nested if statements that correspond to CASES."
  (cond ((= 1 (length cases))
         (if (eq (caar cases) t)
             (let ((then (cdar cases)))
               (if (equal (car then) '(ly-raw newline))
                   (cdr then)
                 then))
           (list (lispy--case->if (car cases)))))
        ((lispy--whitespacep (car cases))
         (cons (car cases)
               (lispy--cases->ifs (cdr cases))))
        (t
         (list
          (lispy--case->if
           (car cases)
           (lispy--cases->ifs (cdr cases)))))))

(defun lispy--whitespace-trim (x)
  "Trim whitespace from start of X."
  (cl-subseq x (cl-position-if-not #'lispy--whitespacep x)))

(defun lispy--replace (lst from to)
  "Recursively replace elements in LST from FROM to TO."
  (cond ((eq lst from)
         to)
        ((not (consp lst))
         lst)
        (t
         (cons
          (lispy--replace (car lst) from to)
          (lispy--replace (cdr lst) from to)))))

;;* Utilities: error reporting
(defun lispy--complain (msg)
  "Display MSG if `lispy-verbose' is t."
  (when (and lispy-verbose
             (null noninteractive))
    (message "[lispy-lite] %s: %s"
             (prin1-to-string this-command)
             msg)))

(defun lispy--complain-not-supported ()
  (lispy--complain "Command not supported for current mode."))

;;* Utilities: rest
(defun lispy--indent-region (beg end)
  "Indent region BEG END without reporting progress."
  (save-excursion
    (setq end (copy-marker end))
    (goto-char beg)
    (while (< (point) end)
      (or (and (bolp) (eolp))
          (indent-according-to-mode))
      (forward-line 1))
    (move-marker end nil)))

(defvar lispy-no-indent-modes '(minibuffer-inactive-mode
                                comint-mode)
  "List of major modes where `indent-for-tab-command' should not be used.
`lispy--indent-for-tab' will do nothing if the current mode or any of its parent
modes is in this list.")

(defun lispy--indent-for-tab ()
  "Call `indent-for-tab-command'."
  (unless (or (memq major-mode lispy-no-indent-modes)
              (apply #'derived-mode-p lispy-no-indent-modes)
              (= 0 (buffer-size)))
    (let ((tab-always-indent t))
      (lispy-flet (message (&rest _x))
        (indent-for-tab-command)))))

(defun lispy--remove-gaps ()
  "Remove dangling `\\s)'."
  (when (and (lispy-right-p)
             (looking-back "[^ \t\n]\\([ \t\n]+\\)\\s)"
                           (condition-case nil
                               (save-excursion
                                 (backward-list)
                                 (point))
                             (error (point-min))))
             (not (eq ?\\ (aref (match-string 0) 0))))
    (unless (save-excursion
              (save-match-data
                (goto-char (match-beginning 1))
                (lispy--in-string-or-comment-p)))
      (delete-region (match-beginning 1)
                     (match-end 1)))))

(defun lispy--surround-region (alpha omega)
  "Surround active region with ALPHA and OMEGA and re-indent."
  (let ((beg (region-beginning))
        (end (region-end)))
    (goto-char end)
    (insert omega)
    (goto-char beg)
    (insert alpha)
    (deactivate-mark)
    (indent-region beg (+ 2 end))))

(defun lispy--mark (bnd)
  "Mark BND.  BND is a cons of beginning and end positions."
  (setq deactivate-mark nil)
  (set-mark (car bnd))
  (goto-char (cdr bnd)))

(defun lispy--space-unless (context)
  "Insert one space.
Unless inside string or comment, or `looking-back' at CONTEXT."
  (let ((inhibit-field-text-motion t))
    (unless (or lispy-no-space
                (bolp)
                (and (window-minibuffer-p)
                     (eq (point) (minibuffer-prompt-end)))
                (lispy--in-string-or-comment-p)
                (lispy-looking-back context))
      (insert " "))))

(defun lispy--delimiter-space-unless (preceding-syntax-alist)
  "Like `lispy--space-unless' but use PRECEDING-SYNTAX-ALIST for decision.
PRECEDING-SYNTAX-ALIST should be an alist of `major-mode' to a list of regexps.
When `looking-back' at any of these regexps, whitespace, or a delimiter, do not
insert a space."
  (lispy--space-unless
   (concat "^\\|\\s-\\|" lispy-left
           (lispy--preceding-syntax preceding-syntax-alist "\\|"))))

(defun lispy--reindent (&optional arg)
  "Reindent current sexp.  Up-list ARG times before that."
  (cond ((region-active-p)
         (indent-region (region-beginning)
                        (region-end)))
        (arg
         (lispy-save-excursion
           (lispy--out-forward arg)
           (backward-list)
           (indent-sexp)))

        ((lispy-right-p)
         (save-excursion
           (backward-list)
           (indent-sexp)))

        ((lispy-left-p)
         (indent-sexp))

        (t
         (save-excursion
           (lispy--out-forward 1)
           (backward-list)
           (indent-sexp)))))

(defun lispy--delete ()
  "Delete one sexp."
  (unless (lispy-left-p)
    (error "Bad position"))
  (let ((bnd (lispy--bounds-list)))
    (delete-region (car bnd) (cdr bnd))
    (cond ((looking-at (concat "\n+" lispy-left))
           (delete-region (match-beginning 0)
                          (1- (match-end 0))))
          ((looking-at "\n\n+"))
          ((looking-at "\\([ ]*\\)\n")
           (delete-region (match-beginning 1)
                          (match-end 1)))
          ((looking-at lispy-right))
          ((eolp))
          (t
           (just-one-space)
           (when (lispy-after-string-p "( ")
             (backward-delete-char 1))))))

(defun lispy--recenter-bounds (bnd)
  "Make sure BND is visible in window.
BND is a cons of start and end points."
  (cond ((> (count-lines (car bnd) (cdr bnd))
            (window-height)))
        ((< (car bnd) (window-start))
         (save-excursion
           (goto-char (car bnd))
           (recenter 0)))
        ((> (cdr bnd) (window-end))
         (save-excursion
           (goto-char (cdr bnd))
           (recenter -1)))))

(defun lispy--prin1-to-string (expr offset mode)
  "Return the string representation of EXPR.
EXPR is indented first, with OFFSET being the column position of
the first character of EXPR.
MODE is the major mode for indenting EXPR."
  (let ((lif lisp-indent-function))
    (with-temp-buffer
      (funcall mode)
      (dotimes (_i offset)
        (insert ?\ ))
      (let ((lisp-indent-function lif))
        (lispy--insert expr))
      (buffer-substring-no-properties
       (+ (point-min) offset)
       (point-max)))))

(defun lispy--splice-to-str (sexp)
  "Return the printed representation of SEXP.
The outer delimiters are stripped."
  (if sexp
      (substring
       (prin1-to-string sexp) 1 -1)
    ""))

(defun lispy--insert (expr)
  "Insert the EXPR read by `lispy--read'."
  (let ((start-pt (point))
        beg
        sxp type)
    (prin1 expr (current-buffer))
    (save-restriction
      (narrow-to-region start-pt (point))
      (goto-char (point-min))
      (while (and (re-search-forward "(ly-raw" nil t)
                  (setq beg (match-beginning 0)))
        (goto-char beg)
        (setq sxp (ignore-errors (read (current-buffer))))
        (setq type (cadr sxp))
        (cl-case type
          (newline
           (delete-region beg (point))
           (delete-char
            (- (skip-chars-backward " ")))
           (insert "\n"))
          ((string comment symbol float quasiquote)
           (delete-region beg (point))
           (insert (cl-caddr sxp)))
          (comma-symbol
           (delete-region beg (point))
           (insert "\\,"))
          (ignore
           (delete-region beg (point))
           (backward-delete-char 1))
          (raw
           (delete-region beg (point))
           (prin1 (cons 'ly-raw (cddr sxp))
                  (current-buffer))
           (backward-list)
           (forward-char 7))
          (quote
           (delete-region beg (point))
           (insert "'")
           (let ((it (cl-caddr sxp)))
             (if it
                 (prin1 it (current-buffer))
               (insert "()")))
           (goto-char beg))
          (empty
           (delete-region beg (point))
           (insert "()"))
          (char
           (delete-region beg (point))
           (insert "?" (cl-caddr sxp)))
          ((clojure-char)
           (delete-region beg (point))
           (insert (cl-caddr sxp)))
          ((clojure-commas)
           (delete-region (1- beg) (point))
           (insert (cl-caddr sxp)))
          (clojure-symbol
           (delete-region beg (point))
           (insert (cl-caddr sxp)))
          (lisp-char
           (delete-region beg (point))
           (insert (cl-caddr sxp)))
          (lisp-macro
           (delete-region beg (point))
           (insert (cl-caddr sxp)))
          ((clojure-gensym clojure-keyword)
           (delete-region beg (point))
           (insert (cl-caddr sxp)))
          (function
           (delete-region beg (point))
           (insert (format "#'%S" (cl-caddr sxp)))
           (goto-char beg))
          (clojure-dot
           (delete-region beg (point))
           (insert "."))
          (clojure-lambda
           (delete-region beg (point))
           (insert (format "#%S" (cl-caddr sxp)))
           (goto-char beg))
          (clojure-set
           (delete-region beg (point))
           (insert (format "#{%s}" (lispy--splice-to-str (cl-caddr sxp))))
           (goto-char beg))
          (clojure-map
           (delete-region beg (point))
           (insert (format "{%s}" (lispy--splice-to-str (cl-caddr sxp))))
           (goto-char beg))
          (clojure-object
           (delete-region beg (point))
           (insert (format "#object[%s]" (lispy--splice-to-str (cl-caddr sxp))))
           (goto-char beg))
          (splice
           (delete-region beg (point))
           (insert
            (nth 2 sxp)
            (lispy--splice-to-str (car (nthcdr 4 sxp)))
            (nth 3 sxp))
           (goto-char beg))
          (clojure-deref-map
           (delete-region beg (point))
           (insert (format "@{%s}" (lispy--splice-to-str (cl-caddr sxp))))
           (goto-char beg))
          (clojure-deref-vector
           (delete-region beg (point))
           (insert (format "@[%s]" (lispy--splice-to-str (cl-caddr sxp))))
           (goto-char beg))
          (clojure-deref-list
           (delete-region beg (point))
           (insert (format "@(%s)" (lispy--splice-to-str (cl-caddr sxp))))
           (goto-char beg))
          (clojure-reader-conditional-splice
           (delete-region beg (point))
           (insert (format "#?@(%s)" (lispy--splice-to-str (cl-caddr sxp))))
           (goto-char beg))
          (clojure-reader-conditional
           (delete-region beg (point))
           (insert (format "#?(%s)" (lispy--splice-to-str (cl-caddr sxp))))
           (goto-char beg))
          (clojure-reader-comment
           (delete-region beg (point))
           (insert (format "#_%S" (cl-caddr sxp)))
           (goto-char beg))
          (clojure-comma
           (delete-region beg (point))
           (delete-horizontal-space)
           (insert ", "))
          (racket-true
           (delete-region beg (point))
           (insert "#t"))
          (racket-false
           (delete-region beg (point))
           (insert "#f"))
          (racket-option
           (delete-region beg (point))
           (insert (format "#:%S" (cl-caddr sxp))))
          (angle
           (delete-region beg (point))
           (insert (format "#<%s>" (cl-caddr sxp)))
           (goto-char beg))
          (reference
           (delete-region beg (point))
           (insert (cl-caddr sxp)))
          (\`
           (if (> (length sxp) 3)
               (progn
                 (goto-char beg)
                 (insert "`")
                 (delete-region (+ (point) 1)
                                (+ (point) 11)))
             (delete-region beg (point))
             (insert "`")
             (prin1 (cl-caddr sxp) (current-buffer)))
           (goto-char beg))
          (\,
           (delete-region beg (point))
           (insert ",")
           (prin1 (cl-caddr sxp) (current-buffer))
           (goto-char beg))
          (comma-splice
           (delete-region beg (point))
           (insert ",@")
           (prin1 (cl-caddr sxp) (current-buffer))
           (goto-char beg))
          (dot
           (delete-region beg (point))
           (insert "."))
          (t (goto-char (1+ beg)))))
      (goto-char (point-min))
      (while (re-search-forward "\\(?:\\s_\\|\\sw\\)\\(\\\\\\?\\)" nil t)
        (replace-match "?" t t nil 1))
      (goto-char (point-min))
      (while (re-search-forward "\\sw\\(\\\\\\.\\)" nil t)
        (unless (save-match-data
                  (lispy--in-string-p))
          (replace-match "." nil nil nil 1)))
      (goto-char (point-min))
      (while (re-search-forward "[0-9]+\\(\\\\#\\)" nil t)
        (replace-match "#" nil t nil 1))
      (when lispy-do-fill
        (goto-char (point-min))
        (while (re-search-forward " " nil t)
          (cond ((lispy--in-string-p))

                ((lispy--in-comment-p)
                 (fill-paragraph)
                 (goto-char (cdr (lispy--bounds-comment))))

                ((> (current-column) fill-column)
                 (newline-and-indent)))))
      (goto-char (point-max))
      (widen)))
  (when (and (lispy-right-p)
             (not (lispy--in-comment-p)))
    (backward-list)
    (indent-sexp)
    (forward-list)))

(declare-function clojure-align "ext:clojure-mode")

(defun lispy--trim-whitespace-at-bol ()
  "If the point is at '(', remove whitespace (tab and blank space) before point."
  (when (and (looking-at "(")
             (= (point)
                (save-excursion
                  (lispy--out-backward 99)
                  (point))))
    (let ((pt (point)))
      (skip-chars-backward " \t")
      (delete-region pt (point)))))

(defun lispy--get-lisp-indent-function ()
  (if (looking-at "(\\(cl-defun\\|defhydra\\)")
      #'common-lisp-indent-function
    lisp-indent-function))

(defun lispy--prettify-emacs-lisp-sexp ()
  (let* ((lisp-indent-function (lispy--get-lisp-indent-function))
         (bnd (lispy--bounds-dwim))
         (str (lispy--string-dwim bnd))
         (offset (save-excursion (goto-char (car bnd)) (current-column)))
         (was-left (lispy-left-p)))
    (cond ((looking-at (lispy-comment-char 2)))
          (t
           (let* ((max-lisp-eval-depth 10000)
                  (max-specpdl-size 10000)
                  (res (lispy--sexp-normalize (lispy--read str)))
                  (new-str (lispy--prin1-to-string res offset major-mode)))
             (unless (string= str new-str)
               ;; We should not do this if new-str failed to eval.
               (unless (string= "nil" new-str)
                 (delete-region (car bnd)
                                (cdr bnd))
                 (insert new-str))
               (when was-left
                 (backward-list))))))))

(defun lispy--prettify-1 ()
  "Normalize/prettify current sexp."
  (lispy--trim-whitespace-at-bol)
  (cond ((memq major-mode lispy-clojure-modes)
         (call-interactively 'clojure-align))
        ((eq 'emacs-lisp-mode major-mode)
         (lispy--prettify-emacs-lisp-sexp))))

(defun lispy--sexp-trim-trailing-newlines (foo comment)
  "Trim trailing (ly-raw newline) from FOO.
Treat comments differently when COMMENT is t."
  (if (and (consp foo) (consp (cdr foo)))
      (let ((expr (reverse foo)))
        (while (and (consp expr)
                    (listp expr)
                    (equal (car expr) '(ly-raw newline))
                    (not (and comment
                              (lispy--raw-comment-p (cadr expr)))))
          (setq expr (cdr expr)))
        (reverse expr))
    foo))

(defun lispy--sexp-normalize (foo)
  "Return a pretty version of FOO.
Only `ly-raw' lists within FOO are manipulated."
  (cond ((null foo)
         nil)

        ((consp foo)
         (cons (lispy--sexp-normalize
                (lispy--sexp-trim-trailing-newlines (car foo) t))
               (lispy--sexp-normalize
                (lispy--sexp-trim-trailing-newlines (cdr foo) t))))
        (t
         foo)))

(defun lispy--teleport (beg end endp regionp)
  "Move text from between BEG END to point.
Leave point at the beginning or end of text depending on ENDP.
Make text marked if REGIONP is t."
  (let ((str (buffer-substring-no-properties beg end))
        (beg1 (1+ (point)))
        (size (buffer-size))
        (deactivate-mark nil))
    (if (and (>= (point) beg)
             (<= (point) end))
        (progn
          (message "Can't teleport expression inside itself")
          (goto-char beg))
      (goto-char beg)
      (delete-region beg end)
      (when (and (eolp)
                 (lispy-bolp))
        (delete-region (line-beginning-position)
                       (1+ (point))))
      (when (> beg1 beg)
        (cl-decf beg1 (- size (buffer-size))))
      (goto-char beg1)
      (when (looking-at lispy-left)
        (save-excursion
          (newline-and-indent)))
      (unless (lispy-looking-back "[ ([{]")
        (insert " ")
        (cl-incf beg1))
      (insert str)
      (unless (looking-at "[\n)]")
        (insert "\n")
        (backward-char))
      (lispy-save-excursion
        (lispy--reindent 1)
        (goto-char (1- beg1))
        (indent-sexp))
      (if regionp
          (progn
            (setq deactivate-mark nil)
            (set-mark-command nil)
            (goto-char beg1)
            (when endp
              (exchange-point-and-mark)))
        (unless endp
          (goto-char beg1)
          (skip-chars-forward "'"))))))

(defun lispy--swap-regions (bnd1 bnd2)
  "Swap buffer regions BND1 and BND2.
Return a cons of the new text cordinates."
  (when (> (car bnd1) (car bnd2))
    (cl-rotatef bnd1 bnd2))
  (let ((str1 (lispy--string-dwim bnd1))
        (str2 (lispy--string-dwim bnd2)))
    (goto-char (car bnd2))
    (delete-region (car bnd2) (cdr bnd2))
    (insert str1)
    (when (lispy--in-comment-p)
      (unless (eolp)
        (newline-and-indent)))
    (goto-char (car bnd1))
    (delete-region (car bnd1) (cdr bnd1))
    (insert str2)
    (goto-char (car bnd1)))
  (let* ((l1 (- (cdr bnd1) (car bnd1)))
         (l2 (- (cdr bnd2) (car bnd2)))
         (new-beg (+ (car bnd2) (- l2 l1)))
         (new-end (+ new-beg l1)))
    (cons
     (cons (car bnd1) (+ (car bnd1) l2))
     (cons new-beg new-end))))

(defun lispy--ensure-visible ()
  "Remove overlays hiding point."
  (let ((overlays (overlays-at (point)))
        ov expose)
    (while (setq ov (pop overlays))
      (if (and (invisible-p (overlay-get ov 'invisible))
               (setq expose (overlay-get ov 'isearch-open-invisible)))
          (funcall expose ov)))))

(defun lispy--delete-pair-in-string (left right)
  "Delete a pair of LEFT and RIGHT in string."
  (let ((bnd (lispy--bounds-string)))
    (when bnd
      (let ((pos (cond ((looking-at left)
                        (save-excursion
                          (let ((b1 (match-beginning 0))
                                (e1 (match-end 0))
                                b2 e2)
                            (when (re-search-forward right (cdr bnd) t)
                              (setq b2 (match-beginning 0)
                                    e2 (match-end 0))
                              (delete-region b2 e2)
                              (delete-region b1 e1)
                              b1))))
                       ((looking-at right)
                        (save-excursion
                          (let ((b1 (match-beginning 0))
                                (e1 (match-end 0))
                                b2 e2)
                            (when (re-search-backward left (car bnd) t)
                              (setq b2 (match-beginning 0)
                                    e2 (match-end 0))
                              (delete-region b1 e1)
                              (delete-region b2 e2)
                              (+ (point) (- b1 e2)))))))))
        (when pos
          (goto-char pos))))))

(defvar ediff-temp-indirect-buffer)
(defun lispy--make-ediff-buffer (buffer ext bnd)
  "Create a copy of BUFFER with EXT added to the name.
Use only the part bounded by BND."
  (cl-multiple-value-bind (name mode str)
      (with-current-buffer buffer
        (list (concat (buffer-name) ext) major-mode (lispy--string-dwim bnd)))
    (with-current-buffer (get-buffer-create name)
      (funcall mode)
      (insert str "\n")
      (indent-region (point-min) (point-max))
      (require 'ediff-init)
      (setq ediff-temp-indirect-buffer t)
      (list (current-buffer) (point-min) (point-max)))))

(defvar macrostep-keymap)
(defvar lispy--compat-cmd nil
  "Store the looked up compat command.")

(defun lispy--insert-or-call (def plist)
  "Return a lambda to call DEF if position is special.
Otherwise call `self-insert-command'.
PLIST currently accepts:
- :disable with a mode to disable
- :override with a lambda to conditionally abort command"
  (let ((disable (plist-get plist :disable))
        (override (plist-get plist :override))
        (inserter (plist-get plist :inserter)))
    `(lambda ()
       ,(format "Call `%s' when special, self-insert otherwise.\n\n%s"
                (symbol-name def) (documentation def))
       (interactive)
       ,@(when disable `((,disable -1)))
       (unless (looking-at lispy-outline)
         (lispy--ensure-visible))
       (cond ,@(cond ((null override) nil)
                     ((functionp override)
                      `((funcall ,override)))
                     ((eq (car override) 'cond)
                      (cdr override))
                     (t
                      (error "Unexpected :override %S" override)))

             ,@(when (memq 'god-mode lispy-compat)
                     '(((and (or (bound-and-true-p god-global-mode)
                                 (bound-and-true-p god-local-mode)))
                        (call-interactively 'god-mode-self-insert))))

             ,@(when (memq 'macrostep lispy-compat)
                     '(((and (bound-and-true-p macrostep-mode)
                         (setq lispy--compat-cmd (lookup-key macrostep-keymap (this-command-keys))))
                        (call-interactively lispy--compat-cmd))))

             ,@(when (memq 'magit-blame-mode lispy-compat)
                     '(((and (bound-and-true-p magit-blame-mode)
                         (setq lispy--compat-cmd (lookup-key magit-blame-mode-map (this-command-keys))))
                        (call-interactively lispy--compat-cmd))))

             ((region-active-p)
              (call-interactively ',def))

             ((lispy--in-string-or-comment-p)
              (setq this-command 'self-insert-command)
              (call-interactively 'self-insert-command))

             ((or (lispy-left-p)
                  (lispy-right-p)
                  (and (lispy-bolp)
                       (or (looking-at lispy-outline-header)
                           (looking-at lispy-outline))))
              (call-interactively ',def))

             (t
              (setq this-command 'self-insert-command)
              (call-interactively
               (quote
                ,(or inserter
                     'self-insert-command))))))))

(defun lispy--find-unmatched-delimiters (beg end)
  "Return the positions of unmatched delimiters between BEG and END.
When the region is a greater size than `lispy-safe-threshold', it will not be
checked and nil will be returned."
  (if (> (- end beg) lispy-safe-threshold)
      nil
    (save-excursion
      (goto-char beg)
      (let ((lispy-delimiters (concat (substring lispy-right 0 -1)
                                      "\""
                                      (substring lispy-left 1)))
            matched-left-quote-p
            string-bounds
            string-end
            comment-end
            left-positions
            right-positions)
        (while (re-search-forward lispy-delimiters end t)
          (let* ((match-beginning (match-beginning 0))
                 (matched-delimiter (buffer-substring-no-properties
                                     match-beginning
                                     (match-end 0))))
            (cond
              ((and lispy-safe-actions-ignore-strings
                    (save-excursion
                      (goto-char match-beginning)
                      (setq string-bounds (lispy--bounds-string))
                      (setq string-end (cdr string-bounds))))
               (setq matched-left-quote-p (= (1- (point))
                                             (car string-bounds)))
               (cond ((< (1- string-end) end)
                      (goto-char string-end)
                      ;; when skipping strings, will only match right quote
                      ;; if left quote is not in the region
                      (when (not matched-left-quote-p)
                        (push (1- string-end) right-positions)))
                     (t
                      (when matched-left-quote-p
                        ;; unmatched left quote
                        (push match-beginning left-positions))
                      (goto-char end))))
              ((and lispy-safe-actions-ignore-comments
                    (save-excursion
                      (goto-char match-beginning)
                      (setq comment-end (cdr (lispy--bounds-comment)))))
               (if (< comment-end end)
                   (goto-char comment-end)
                 (goto-char end)))
              (t
               (unless (looking-back "\\\\." (- (point) 2))
                 (if (or (string-match lispy-left matched-delimiter)
                         (and (string= matched-delimiter "\"")
                              (lispy--in-string-p)))
                     (push match-beginning left-positions)
                   (if (> (length left-positions) 0)
                       (pop left-positions)
                     (push match-beginning right-positions))))))))
        (nreverse (append left-positions right-positions))))))

(defun lispy--maybe-split-safe-region (beg end &optional end-unsafe-p)
  "Return a list of regions between BEG and END that are safe to delete.
It is expected that there are no unmatched delimiters in between BEG and END.
Split the region if deleting it would pull unmatched delimiters into a comment.
Specifically, split the region if all of the following are true:

- `lispy-safe-actions-no-pull-delimiters-into-comments' is non-nil
- BEG is inside a comment
- END is not in a comment
- Either there are unmatched delimiters on the line after END or END-UNSAFE-P is
  non-nil

Otherwise, just return a list with the initial region. The regions are returned
in reverse order so that they can be easily deleted without recalculation."
  (if (and lispy-safe-actions-no-pull-delimiters-into-comments
           ;; check that BEG is inside a comment
           ;; `lispy--in-comment-p' returns t at comment start which is
           ;; unwanted here
           (and (save-excursion
                  (nth 4 (syntax-ppss beg))))
           (save-excursion
             (goto-char end)
             ;; check that END is not inside or a comment and that the
             ;; following line has unmatched delimiters or has been specified
             ;; as unsafe to pull into a comment
             (and (not (lispy--in-comment-p))
                  (or end-unsafe-p
                      (lispy--find-unmatched-delimiters
                       end
                       (line-end-position))))))
      ;; exclude newline; don't pull END into a comment
      (let ((comment-end-pos (save-excursion
                               (goto-char beg)
                               (cdr (lispy--bounds-comment)))))
        (list (cons (1+ comment-end-pos) end)
              (cons beg comment-end-pos)))
    (list (cons beg end))))

(defun lispy--find-safe-regions (beg end)
  "Return a list of regions between BEG and END that are safe to delete.
The regions are returned in reverse order so that they can be easily deleted
without recalculation."
  (let ((unmatched-delimiters (lispy--find-unmatched-delimiters beg end))
        (maybe-safe-pos beg)
        safe-regions)
    (dolist (unsafe-pos unmatched-delimiters)
      (unless (= maybe-safe-pos unsafe-pos)
        (setq safe-regions
              (nconc (lispy--maybe-split-safe-region maybe-safe-pos unsafe-pos
                                                     t)
                     safe-regions)))
      (setq maybe-safe-pos (1+ unsafe-pos)))
    (setq safe-regions
          (nconc (lispy--maybe-split-safe-region maybe-safe-pos end)
                 safe-regions))))

(defun lispy--maybe-safe-delete-region (beg end)
  "Delete the region from BEG to END.
If `lispy-safe-delete' is non-nil, exclude unmatched delimiters."
  (if lispy-safe-delete
      (let ((safe-regions (lispy--find-safe-regions beg end)))
        (dolist (safe-region safe-regions)
          (delete-region (car safe-region) (cdr safe-region))))
    (delete-region beg end)))

(defun lispy--maybe-safe-kill-region (beg end)
  "Kill the region from BEG to END.
If `lispy-safe-delete' is non-nil, exclude unmatched delimiters."
  (if lispy-safe-delete
      (let ((safe-regions (lispy--find-safe-regions beg end))
            safe-strings)
        (dolist (safe-region safe-regions)
          (push (lispy--string-dwim safe-region) safe-strings)
          (delete-region (car safe-region) (cdr safe-region)))
        (kill-new (apply #'concat safe-strings)))
    (kill-region beg end)))

(defun lispy--maybe-safe-region (beg end)
  "Return the text from BEG to END.
If `lispy-safe-copy' is non-nil, exclude unmatched delimiters."
  (if lispy-safe-copy
      (let ((safe-regions (lispy--find-safe-regions beg end))
            safe-strings)
        (dolist (safe-region safe-regions)
          (push (lispy--string-dwim safe-region) safe-strings))
        (apply #'concat safe-strings))
    (lispy--string-dwim (cons beg end))))

(defvar lispy--pairs
  '(("(" . ")")
    ("[" . "]")
    ("{" . "}")))

(defun lispy--balance (text)
  "Return TEXT with unmatched delimiters added to the beginning or end.
This does not attempt to deal with unbalanced double quotes as it is not always
possible to infer which side the missing quote should be added to."
  (let ((old-major-mode major-mode))
    (with-temp-buffer
      (funcall old-major-mode)
      (insert text)
      (let ((unmatched-positions
             (lispy--find-unmatched-delimiters (point-min) (point-max)))
            add-to-beginning
            add-to-end
            delim)
        (dolist (pos unmatched-positions)
          (setq delim (buffer-substring pos (1+ pos)))
          (cond ((string-match lispy-left delim)
                 (push (cdr (assoc delim lispy--pairs))
                       add-to-end))
                ((string-match lispy-right delim)
                 (push (car (rassoc delim lispy--pairs))
                       add-to-beginning))))
        (when add-to-beginning
          (goto-char (point-min))
          (insert (apply #'concat add-to-beginning)))
        (when add-to-end
          (goto-char (point-max))
          (when (and lispy-safe-actions-no-pull-delimiters-into-comments
                     (lispy--in-comment-p))
            (push "\n" add-to-end))
          (insert (apply #'concat add-to-end)))
        (buffer-substring (point-min) (point-max))))))

(defun lispy--maybe-safe-current-kill ()
  "Return the most recent kill.
If `lispy-safe-paste' is non-nil, any unmatched delimiters will be added to it."
  (if lispy-safe-paste
      (lispy--balance (current-kill 0))
    (current-kill 0)))

;;* Key definitions
(defvar ac-trigger-commands '(self-insert-command))

(defvar mc/cmds-to-run-for-all nil)
(defvar mc/cmds-to-run-once nil)
(mapc (lambda (x) (add-to-list 'mc/cmds-to-run-once x))
      '(lispy-cursor-down))
(mapc (lambda (x) (add-to-list 'mc/cmds-to-run-for-all x))
      '(lispy-parens lispy-brackets lispy-braces lispy-quotes
        lispy-kill lispy-delete))

(defadvice ac-handle-post-command (around ac-post-command-advice activate)
  "Don't `auto-complete' when region is active."
  (unless (region-active-p)
    ad-do-it))

(defun lispy--delsel-advice (orig-fun)
  "Advice for `delete-selection-mode'.
Usage:
 (advice-add 'delete-selection-pre-hook :around 'lispy--delsel-advice)"
  (if (and (use-region-p)
           (string-match-p "^special" (symbol-name this-command)))
      (progn
        (delete-active-region)
        (setq this-command 'ignore)
        (self-insert-command 1))
    (funcall orig-fun)))

(defun lispy--undo-tree-advice (&optional _arg)
  "Advice to run before `undo-tree-undo'.

Otherwise, executing undo in middle of a lispy overlay operation
irreversibly corrupts the undo tree state. "
  (lispy-map-delete-overlay))

(advice-add 'undo-tree-undo :before 'lispy--undo-tree-advice)

(defun lispy-define-key (keymap key def &rest plist)
  "Forward to (`define-key' KEYMAP KEY FUNC).
FUNC is obtained from (`lispy--insert-or-call' DEF PLIST)."
  (declare (indent 3))
  (require 'eldoc)
  (let ((func (defalias (intern (concat "special-" (symbol-name def)))
                (lispy--insert-or-call def plist))))
    (add-to-list 'ac-trigger-commands func)
    (unless (memq func mc/cmds-to-run-once)
      (add-to-list 'mc/cmds-to-run-for-all func))
    (eldoc-add-command func)
    (define-key keymap (kbd key) func)))

(lispy-defverb
 "goto"
 (("d" lispy-goto)
  ("f" lispy-follow)
  ("b" pop-tag-mark)
  ("q" lispy-quit)))

(defun lispy-move-and-slurp-actions ()
  (interactive)
  (cl-case (read-char-from-minibuffer "Actions:\n
h: Move left
l: Move right
j: Slurp down
k: Slurp up
\n")
    (?h (call-interactively 'lispy-move-left))
    (?j (call-interactively 'lispy-down-slurp))
    (?k (call-interactively 'lispy-up-slurp))
    (?l (call-interactively 'lispy-move-right))))

(defhydra lh-knight ()
  "knight"
  ("j" lispy-knight-down)
  ("k" lispy-knight-up)
  ("z" nil))

(setq lispy-mode-map-special
  (let ((map (make-sparse-keymap)))
    ;; navigation
    (lispy-define-key map "h" 'lispy-left)
    (lispy-define-key map "l" 'lispy-flow)
    (lispy-define-key map "j" 'lispy-down)
    (lispy-define-key map "k" 'lispy-up)
    ;; (lispy-define-key map "m" 'lispy-move-and-slurp-actions)
    (lispy-define-key map "P" 'lispy-paste)
    (lispy-define-key map "o" 'lispy-occur)
    (lispy-define-key map "z" 'lh-knight/body)
    ;; Paredit transformations
    (lispy-define-key map ">" 'lispy-slurp)
    (lispy-define-key map "<" 'lispy-barf)
    (lispy-define-key map "x" 'lispy-splice)
    (lispy-define-key map "r" 'lispy-raise)
    (lispy-define-key map "R" 'lispy-raise-some)
    (lispy-define-key map "+" 'lispy-join)
    ;; more transformations
    (lispy-define-key map "C" 'lispy-convolute)
    (lispy-define-key map "X" 'lispy-convolute-left)
    (lispy-define-key map "w" 'lispy-move-up)
    (lispy-define-key map "s" 'lispy-move-down)
    (lispy-define-key map "J" 'lispy-oneline)
    (lispy-define-key map "M" 'lispy-alt-multiline)
    (lispy-define-key map "S" 'lispy-stringify)
    ;; marking
    (lispy-define-key map "f" 'lispy-ace-symbol)
    (lispy-define-key map "K" 'lispy-describe)
    (lispy-define-key map "v" 'lispy-mark-list)
    ;; dialect-specific
    (lispy-define-key map "e" 'lispy-eval)
    (lispy-define-key map "E" 'lispy-eval-defun)
    (lispy-define-key map "g" 'lispy-goto)
    (lispy-define-key map "F" 'lispy-ace-symbol-beginning-of-defun)
    (lispy-define-key map "G" 'lispy-follow t)
    (lispy-define-key map "d" 'lispy-delete)
    (lispy-define-key map "D" 'lispy-kill)
    ;; (lispy-define-key map "D" 'pop-tag-mark)
    (lispy-define-key map "B" 'lispy-beginning-of-defun)
    (lispy-define-key map "_" 'lispy-underscore)
    ;; miscellanea
    (define-key map (kbd "SPC") 'lispy-space)
    (lispy-define-key map "i" 'lispy-tab)
    (lispy-define-key map "N" 'lispy-narrow)
    (lispy-define-key map "W" 'lispy-widen)
    (lispy-define-key map "c" 'lispy-clone)
    (lispy-define-key map "u" 'lispy-undo)
    (lispy-define-key map "q" 'lispy-ace-paren
      :override '(cond ((bound-and-true-p view-mode)
                        (View-quit))))
    (lispy-define-key map "Q" 'lispy-ace-char)
    ;; (lispy-define-key map "v" 'lispy-view)
    (lispy-define-key map "y" 'lispy-new-copy)
    (lispy-define-key map "t" 'lispy-teleport)
    (lispy-define-key map "n" 'lispy-new-copy)
    (lispy-define-key map "b" 'lispy-back)
    ;; (lispy-define-key map "B" 'lispy-ediff-regions)
    (lispy-define-key map "x" 'lispy-splice)
    (lispy-define-key map "V" 'lispy-visit)
    (lispy-define-key map "-" 'lispy-ace-subword)
    (lispy-define-key map "." 'lispy-repeat)
    (lispy-define-key map "~" 'lispy-tilde)
    (lispy-define-key map "a" 'forward-char)
    ;; digit argument
    (mapc (lambda (x) (lispy-define-key map (format "%d" x) 'digit-argument))
          (number-sequence 0 9))
    map))

;;* Parinfer compat
(defun lispy--auto-wrap (func arg preceding-syntax-alist)
  "Helper to create versions of the `lispy-pair' commands that wrap by default."
  (cond ((not arg)
         (setq arg -1))
        ((or (eq arg '-)
             (and (numberp arg)
                  (= arg -1)))
         (setq arg nil)))
  (let (bounds)
    (when (and (numberp arg)
               (= arg -1)
               (setq bounds (lispy--bounds-dwim))
               (= (point) (cdr bounds)))
      (lispy--delimiter-space-unless preceding-syntax-alist)))
  (funcall func arg))

(defun lispy-parens-auto-wrap (arg)
  "Like `lispy-parens' but wrap to the end of the line by default.
With an arg of -1, never wrap."
  (interactive "P")
  (lispy--auto-wrap #'lispy-parens arg lispy-parens-preceding-syntax-alist))

(defun lispy-brackets-auto-wrap (arg)
  "Like `lispy-brackets' but wrap to the end of the line by default.
With an arg of -1, never wrap."
  (interactive "P")
  (lispy--auto-wrap #'lispy-brackets arg lispy-brackets-preceding-syntax-alist))

(defun lispy-braces-auto-wrap (arg)
  "Like `lispy-braces' but wrap to the end of the line by default.
With an arg of -1, never wrap."
  (interactive "P")
  (lispy--auto-wrap #'lispy-braces arg lispy-braces-preceding-syntax-alist))

(defun lispy-barf-to-point-nostring (arg)
  "Call `lispy-barf-to-point' with ARG unless in string or comment.
Self-insert otherwise."
  (interactive "P")
  (if (or (lispy--in-string-or-comment-p)
          (lispy-looking-back "?\\\\"))
      (self-insert-command (prefix-numeric-value arg))
    (lispy-barf-to-point arg)))

(defun lispy-delete-backward-or-splice-or-slurp (arg)
  "Call `lispy-delete-backward' unless after a delimiter.
After an opening delimiter, splice. After a closing delimiter, slurp to the end
of the line without moving the point. When in a position where slurping will
not have an effect such as after the final delimiters before the end of a line,
move backward. In comments and strings, call `lispy-delete-backward'. When after
the opening quote of a string, delete the entire string. When after the closing
quote of a string, move backward."
  (interactive "p")
  (let ((string-bounds (lispy--bounds-string)))
    (cond ((and (not string-bounds)
                (save-excursion
                  (backward-char)
                  (lispy--in-string-p)))
           (backward-char))
          ((and string-bounds
                (= (1- (point)) (car string-bounds)))
           (backward-char)
           (lispy-delete 1))
          ((lispy--in-string-or-comment-p)
           (lispy-delete-backward arg))
          ((looking-back lispy-left (1- (point)))
           (when (looking-at "[[:space:]]")
             (fixup-whitespace))
           (backward-char)
           (save-excursion
             (lispy-other)
             (delete-char -1))
           (lispy--delete-leading-garbage)
           (delete-char 1))
          ((looking-back lispy-right (1- (point)))
           (let ((tick (buffer-chars-modified-tick)))
             (save-excursion
               (lispy-slurp -1))
             (when (= tick (buffer-chars-modified-tick))
               (backward-char arg))))
          (t
           (lispy-delete-backward arg)))))

(defun lispy-delete-or-splice-or-slurp (arg)
  "Call `lispy-delete' unless before a delimiter.
Before an opening delimiter, splice. Before a closing delimiter, slurp to the
end of the line without moving the point. When in a position where slurping will
not have an effect such as at the final delimiters before the end of a line,
move forward. In comments and strings, call `lispy-delete'. When before the
opening quote of a string, delete the entire string. When before the closing
quote of a string, move forward."
  (interactive "p")
  (let ((string-bounds (lispy--bounds-string)))
    (cond ((and string-bounds
                (= (1+ (point)) (cdr string-bounds)))
           (forward-char))
          ((and string-bounds
                (= (point) (car string-bounds)))
           (lispy-delete 1))
          ((lispy--in-string-or-comment-p)
           (lispy-delete arg))
          ((looking-at lispy-left)
           (save-excursion
             (lispy-other)
             (delete-char -1))
           (lispy--delete-leading-garbage)
           (delete-char 1))
          ((looking-at lispy-right)
           (let ((tick (buffer-chars-modified-tick)))
             (save-excursion
               (forward-char)
               (lispy-slurp -1))
             (when (= tick (buffer-chars-modified-tick))
               (forward-char arg))))
          (t
           (lispy-delete arg)))))

;;* Paredit compat
(defun lispy-close-round-and-newline (arg)
  "Forward to (`lispy-out-forward-newline' ARG).
Insert \")\" in strings and comments."
  (interactive "p")
  (if (or (lispy--in-string-or-comment-p)
          (lispy-after-string-p "?\\"))
      (insert ")")
    (lispy-out-forward-newline arg)))

(defun lispy-open-square (arg)
  "Forward to (`lispy-brackets' ARG).
Insert \"[\" in strings and comments."
  (interactive "P")
  (if (lispy--in-string-or-comment-p)
      (insert "[")
    (lispy-brackets arg)))

(defun lispy-open-curly (arg)
  "Forward to( `lispy-braces' ARG).
Insert \"{\" in strings and comments."
  (interactive "P")
  (if (lispy--in-string-or-comment-p)
      (insert "{")
    (lispy-braces arg)))

(defun lispy-close-square (arg)
  "Forward to function `lispy-right' with ARG.
Insert \"]\" in strings and comments."
  (interactive "p")
  (if (lispy--in-string-or-comment-p)
      (insert "]")
    (lispy-right arg)))

(defun lispy-close-curly (arg)
  "Forward to function `lispy-right' with ARG.
Insert \"}\" in strings and comments."
  (interactive "p")
  (if (lispy--in-string-or-comment-p)
      (insert "}")
    (lispy-right arg)))

(defun lispy-doublequote (arg)
  "Insert a pair of quotes around the point.
When ARG is non-nil, unquote the current string."
  (interactive "P")
  (let (bnd)
    (cond ((region-active-p)
           (if arg
               (lispy-unstringify)
             (lispy-stringify)))

          ((and (setq bnd (lispy--bounds-string))
                (not (= (point) (car bnd))))
           (if (= (point) (1- (cdr bnd)))
               (forward-char 1)
             (if arg
                 (lispy-unstringify)
               (insert "\\\""))))

          (arg
           (lispy-stringify))

          ((lispy-after-string-p "?\\")
           (self-insert-command 1))

          ((lispy--in-comment-p)
           (insert "\""))

          (t
           (lispy--space-unless "^\\|\\s-\\|\\s(\\|[#]")
           (insert "\"\"")
           (unless (looking-at "\n\\|)\\|}\\|\\]\\|$")
             (just-one-space)
             (backward-char 1))
           (backward-char)))))

(defun lispy-meta-doublequote (arg)
  "Stringify current expression or forward to (`lispy-meta-doublequote' ARG)."
  (interactive "P")
  (let ((bnd (lispy--bounds-string)))
    (if bnd
        (goto-char (cdr bnd))
      (if (lispy-left-p)
          (lispy-stringify)
        (lispy-doublequote arg)))))

(defun lispy-forward-delete (arg)
  "Delete ARG sexps."
  (interactive "p")
  (let (bnd)
    (cond ((lispy-left-p)
           (forward-char 1))
          ((looking-at lispy-right)
           (forward-char 1)
           (setq bnd (lispy--bounds-dwim))
           (delete-region (car bnd) (cdr bnd)))
          ((setq bnd (lispy--bounds-string))
           (cond ((eq (point) (car bnd))
                  (forward-char 1))
                 ((= 2 (- (cdr bnd) (car bnd)))
                  (delete-region (car bnd) (cdr bnd)))
                 (t
                  (lispy-delete arg))))
          (t
           (lispy-delete arg)))))

(defun lispy-backward-delete (arg)
  "Delete ARG sexps backward."
  (interactive "p")
  (let (bnd)
    (cond
      ((lispy--in-comment-p)
       (backward-delete-char-untabify arg))
      ((and (eq (char-before) ?\")
            (null (lispy--bounds-string)))
       (backward-char 1))
      ((lispy-looking-back lispy-left)
       (backward-char)
       (delete-region
        (point)
        (save-excursion
          (forward-sexp 1)
          (point))))
      ((lispy-right-p)
       (backward-char 1))
      ((and (setq bnd (lispy--bounds-string))
            (= 2 (- (cdr bnd) (car bnd))))
       (delete-region (car bnd) (cdr bnd)))
      (t (lispy-delete-backward arg)))))

(defun lispy-wrap-round (arg)
  "Forward to `lispy-parens' with a default ARG of 1."
  (interactive "P")
  (lispy-parens (or arg 1)))

(defun lispy-splice-sexp-killing-backward ()
  "Forward to `lispy-raise'."
  (interactive)
  (let ((bnd (lispy--bounds-list)))
    (if (eq (point) (car bnd))
        (lispy-raise-some)
      (lispy--mark (cons (point) (1- (cdr bnd))))
      (lispy-raise 1)
      (deactivate-mark))))

(defun lispy-splice-sexp-killing-forward ()
  "Forward to `lispy-raise'."
  (interactive)
  (if (lispy-right-p)
      (lispy-raise-some)
    (let ((bnd (lispy--bounds-list)))
      (if (eq (point) (car bnd))
          (lispy-raise-some)
        (lispy--mark (cons (1+ (car bnd)) (point)))
        (lispy-raise 1)
        (deactivate-mark)))))

(defun lispy-raise-sexp ()
  "Forward to `lispy-raise'."
  (interactive)
  (if (or (lispy-left-p)
          (lispy-right-p))
      (lispy-raise 1)
    (lispy-mark-symbol)
    (lispy-other)
    (lispy-raise 1)
    (deactivate-mark)))

(defun lispy-convolute-sexp ()
  "Forward to `lispy-convolute'."
  (interactive)
  (unless (lispy-left-p)
    (lispy--out-backward 1))
  (lispy-convolute 1)
  (lispy--out-backward 1))

(defun lispy-forward-slurp-sexp (arg)
  "Forward to (`lispy-slurp' ARG)."
  (interactive "p")
  (save-excursion
    (unless (lispy-right-p)
      (lispy--out-forward 1))
    (lispy-slurp arg)))

(defun lispy-forward-barf-sexp (arg)
  "Forward to (`lispy-barf' ARG)."
  (interactive "p")
  (save-excursion
    (unless (lispy-left-p)
      (lispy--out-forward 1))
    (lispy-barf arg)))

(defun lispy-backward-slurp-sexp (arg)
  "Forward to (`lispy-slurp' ARG)."
  (interactive "p")
  (save-excursion
    (unless (lispy-left-p)
      (lispy--out-backward 1))
    (lispy-slurp arg)))

(defun lispy-backward-barf-sexp (arg)
  "Forward to (`lispy-barf' ARG)."
  (interactive "p")
  (save-excursion
    (unless (lispy-left-p)
      (lispy--out-backward 1))
    (lispy-barf arg)))

(defvar lispy-mode-map-base
  (let ((map (make-sparse-keymap)))
    ;; navigation
    (define-key map (kbd "C-a") 'lispy-move-beginning-of-line)
    (define-key map (kbd "C-e") 'lispy-move-end-of-line)
    ;; killing
    (define-key map (kbd "C-k") 'lispy-kill)
    (define-key map (kbd "M-d") 'lispy-kill-word)
    (define-key map (kbd "M-DEL") 'lispy-backward-kill-word)
    ;; misc
    (define-key map (kbd "(") 'lispy-parens)
    (define-key map (kbd "[") 'lispy-brackets)
    (define-key map (kbd ";") 'lispy-comment)
    (define-key map (kbd "M-q") 'lispy-fill)
    (define-key map (kbd "C-j") 'lispy-newline-and-indent)
    (define-key map (kbd "RET") 'lispy-newline-and-indent-plain)
    ;; tags
    (define-key map (kbd "M-,") 'pop-tag-mark)
    map))

(defvar lispy-mode-map-paredit
  (let ((map (copy-keymap lispy-mode-map-base)))
    (define-key map (kbd "M-)") 'lispy-close-round-and-newline)
    (define-key map (kbd "C-M-n") 'lispy-forward)
    (define-key map (kbd "C-M-p") 'lispy-backward)
    (define-key map (kbd "[") 'lispy-open-square)
    (define-key map (kbd "]") 'lispy-close-square)
    (define-key map (kbd "{") 'lispy-open-curly)
    (define-key map (kbd "}") 'lispy-close-curly)
    (define-key map (kbd ")") 'lispy-right-nostring)
    (define-key map (kbd "\"") 'lispy-doublequote)
    (define-key map (kbd "M-\"") 'lispy-meta-doublequote)
    (define-key map (kbd "C-d") 'lispy-forward-delete)
    (define-key map (kbd "DEL") 'lispy-backward-delete)
    (define-key map (kbd "C-M-f") 'lispy-forward)
    (define-key map (kbd "C-M-b") 'lispy-backward)
    (define-key map (kbd "M-(") 'lispy-wrap-round)
    (define-key map (kbd "M-s") 'lispy-splice)
    (define-key map (kbd "M-<up>") 'lispy-splice-sexp-killing-backward)
    (define-key map (kbd "M-<down>") 'lispy-splice-sexp-killing-forward)
    (define-key map (kbd "M-r") 'lispy-raise-sexp)
    (define-key map (kbd "M-?") 'lispy-convolute-sexp)
    (define-key map (kbd "C-)") 'lispy-forward-slurp-sexp)
    (define-key map (kbd "C-<right>") 'lispy-forward-slurp-sexp)
    (define-key map (kbd "C-}") 'lispy-forward-barf-sexp)
    (define-key map (kbd "C-<left>") 'lispy-forward-barf-sexp)
    (define-key map (kbd "C-(") 'lispy-backward-slurp-sexp)
    (define-key map (kbd "C-M-<left>") 'lispy-backward-slurp-sexp)
    (define-key map (kbd "C-M-<right>") 'lispy-backward-barf-sexp)
    (define-key map (kbd "C-{") 'lispy-backward-barf-sexp)
    (define-key map (kbd "M-S") 'lispy-split)
    (define-key map (kbd "M-J") 'lispy-join)
    (define-key map (kbd "C-M-u") 'lispy-left)
    (define-key map (kbd "C-M-n") 'lispy-right)
    map))

(defvar lispy-mode-map-parinfer
  (let ((map (copy-keymap lispy-mode-map-base)))
    (define-key map (kbd "(") 'lispy-parens-auto-wrap)
    (define-key map (kbd "[") 'lispy-brackets-auto-wrap)
    (define-key map (kbd "{") 'lispy-braces-auto-wrap)
    (define-key map (kbd "\"") 'lispy-quotes)
    (define-key map (kbd ")") 'lispy-barf-to-point-nostring)
    (define-key map (kbd "]") 'lispy-barf-to-point-nostring)
    (define-key map (kbd "}") 'lispy-barf-to-point-nostring)
    (define-key map (kbd "TAB") 'lispy-indent-adjust-parens)
    (define-key map (kbd "<backtab>") 'lispy-dedent-adjust-parens)
    (define-key map (kbd "DEL") 'lispy-delete-backward-or-splice-or-slurp)
    (define-key map (kbd "C-d") 'lispy-delete-or-splice-or-slurp)
    (define-key map (kbd ":") 'lispy-colon)
    (define-key map (kbd "^") 'lispy-hat)
    (define-key map (kbd "'") 'lispy-tick)
    (define-key map (kbd "`") 'lispy-backtick)
    (define-key map (kbd "#") 'lispy-hash)
    map))

(defvar lispy-mode-map-evilcp
  (let ((map (copy-keymap lispy-mode-map-base)))
    (define-key map (kbd "M-)") 'lispy-close-round-and-newline)
    (define-key map (kbd "[") 'lispy-open-square)
    (define-key map (kbd "]") 'lispy-close-square)
    (define-key map (kbd "{") 'lispy-open-curly)
    (define-key map (kbd "}") 'lispy-close-curly)
    (define-key map (kbd ")") 'lispy-right-nostring)
    (define-key map (kbd "\"") 'lispy-doublequote)
    (define-key map (kbd "M-\"") 'lispy-meta-doublequote)
    (define-key map (kbd "DEL") 'lispy-backward-delete)
    (define-key map (kbd "M-s") 'lispy-splice)
    (define-key map (kbd "M-<up>") 'lispy-splice-sexp-killing-backward)
    (define-key map (kbd "M-<down>") 'lispy-splice-sexp-killing-backward)
    (define-key map (kbd "M-r") 'lispy-raise-sexp)
    (define-key map (kbd "M-?") 'lispy-convolute-sexp)
    (define-key map (kbd "M-S") 'lispy-split)
    (define-key map (kbd "M-J") 'lispy-join)
    (define-key map (kbd "{") 'lispy-braces)
    (define-key map (kbd "}") 'lispy-brackets)
    (define-key map (kbd "]") 'lispy-forward)
    (define-key map (kbd "[") 'lispy-backward)
    (define-key map (kbd "M-(") 'evil-cp-wrap-next-round)
    (define-key map (kbd "M-{") 'evil-cp-wrap-next-curly)
    (define-key map (kbd "M-}") 'evil-cp-wrap-next-square)
    (define-key map (kbd "<") 'evil-cp-<)
    (define-key map (kbd ">") 'evil-cp->)
    (define-key map (kbd "y") 'lispy-new-copy)
    (define-key map (kbd "<C-return>") 'lispy-open-line)
    (define-key map (kbd "<M-return>") 'lispy-meta-return)
    (define-key map (kbd "M-k") 'lispy-move-up)
    (define-key map (kbd "M-j") 'lispy-move-down)
    (define-key map (kbd "M-p") 'lispy-clone)
    (define-key map (kbd "M-\"") 'paredit-meta-doublequote)
    map))

(defvar lispy-mode-map-c-digits
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-1") 'lispy-describe-inline)
    (define-key map (kbd "C-2") 'lispy-arglist-inline)
    (define-key map (kbd "C-3") 'lispy-right)
    (define-key map (kbd "C-4") 'lispy-x)
    (define-key map (kbd "C-7") 'lispy-cursor-down)
    (define-key map (kbd "C-8") 'lispy-parens-down)
    (define-key map (kbd "C-9") 'lispy-out-forward-newline)
    map))

(declare-function View-quit "view")

(defun lispy-special ()
  (interactive)
  (cond ((lispy-left-p) (lispy-other))
        ((lispy-right-p) (lispy-other))
        ('t (lispy-backward 1))))

(defvar lispy-mode-map-lispy
  (let ((map (copy-keymap lispy-mode-map-base)))
    ;; navigation
    (define-key map (kbd "TAB") 'lispy-special)
    (define-key map (kbd ")") 'lispy-right-nostring)
    ;; kill-related
    (define-key map (kbd "C-y") 'lispy-yank)
    (define-key map (kbd "C-d") 'lispy-delete)
    (define-key map (kbd "DEL") 'lispy-delete-backward)
    (define-key map (kbd "M-k") 'lispy-kill-sentence)
    (define-key map (kbd "M-m") 'lispy-mark-symbol)
    (define-key map (kbd "C-,") 'lispy-kill-at-point)
    (define-key map (kbd "C-M-,") 'lispy-mark)
    ;; pairs
    (define-key map (kbd "{") 'lispy-braces)
    (define-key map (kbd "}") 'lispy-brackets)
    (define-key map (kbd "\"") 'lispy-quotes)
    ;; insert
    (define-key map (kbd ":") 'lispy-colon)
    (define-key map (kbd "^") 'lispy-hat)
    (define-key map (kbd "@") 'lispy-at)
    (define-key map (kbd "'") 'lispy-tick)
    (define-key map (kbd "`") 'lispy-backtick)
    (define-key map (kbd "#") 'lispy-hash)
    (define-key map (kbd "M-j") 'lispy-split)
    (define-key map (kbd "M-J") 'lispy-join)
    (define-key map (kbd "<C-return>") 'lispy-open-line)
    (define-key map (kbd "<M-return>") 'lispy-meta-return)
    (define-key map (kbd "M-RET") 'lispy-meta-return)
    map))

(defvar lispy-mode-map-oleh
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "φ") 'lispy-parens)
    (define-key map (kbd "σ") 'lispy-braces)
    (define-key map (kbd "ρ") 'lispy-brackets)
    (define-key map (kbd "θ") 'lispy-quotes)
    (define-key map (kbd "χ") 'lispy-right)
    (define-key map (kbd "C-M-a") 'lispy-beginning-of-defun)
    (define-key map (kbd "<return>") 'lispy-alt-line)
    (define-key map (kbd "RET") 'lispy-newline-and-indent-plain)
    map))

(defcustom lispy-key-theme '(special lispy c-digits)
  "List of key themes used to compose `lispy-mode-map'."
  :type
  '(set
    (const special)
    (radio
     (const lispy)
     (const paredit)
     (const evilcp))
    (const c-digits)
    (const oleh)))

(defun lispy-set-key-theme (theme)
  "Set `lispy-mode-map' for according to THEME.
THEME is a list of choices: 'special, 'lispy, 'paredit, 'evilcp, 'c-digits."
  (setq lispy-mode-map
        (make-composed-keymap
         (delq nil
               (list
                (when (memq 'special theme) lispy-mode-map-special)
                (when (memq 'lispy theme) lispy-mode-map-lispy)
                (when (memq 'paredit theme) lispy-mode-map-paredit)
                (when (memq 'parinfer theme) lispy-mode-map-parinfer)
                (when (memq 'evilcp theme) lispy-mode-map-evilcp)
                (when (memq 'c-digits theme) lispy-mode-map-c-digits)
                (when (memq 'oleh theme) lispy-mode-map-oleh)))))
  (setcdr
   (assq 'lispy-mode minor-mode-map-alist)
   lispy-mode-map))

(lispy-set-key-theme lispy-key-theme)

(provide 'lispy)

;;; lispy.el ends here
