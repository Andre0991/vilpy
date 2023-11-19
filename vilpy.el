;;; vilpy.el --- vi-like Paredit. -*- lexical-binding: t -*-

;; Author: André Peric Tavares <andre.peric.tavares@gmail.com>
;; URL: https://github.com/Andre0991/vilpy
;; Version: 0.1.5 (beta)
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

;;; Code:

;;* Requires
; built-in
(eval-when-compile
  (require 'eldoc))
(require 'mode-local)
(require 'help-fns)
(require 'outline)
(require 'newcomment)
(require 'delsel)
(require 'pcase)
(require 'cl-lib)
; external
(require 'avy)


;;; Langugages configuration

(defvar vilpy--handlers-alist
  '((:emacs-lisp . ((:decider-fn . (lambda () (or (derived-mode-p 'emacs-lisp-mode)
                                                  (derived-mode-p 'lisp-interaction-mode))))
                    (:eval-last-sexp . eval-last-sexp)
                    (:eval-defun . eval-defun)
                    (:eval-region . eval-region)
		    (:eval-buffer . eval-buffer)
                    (:describe-symbol . vilpy--emacs-lisp-describe-symbol)
                    (:indent-sexp . vilpy--prettify-emacs-lisp-sexp)))
    (:inf-clojure . ((:decider-fn . (lambda () (bound-and-true-p inf-clojure-minor-mode)))
                     (:eval-last-sexp . inf-clojure-eval-last-sexp)
		     (:describe-symbol . vilpy--inf-clojure-describe-symbol)
                     (:eval-defun . inf-clojure-eval-defun)
                     (:eval-region . inf-clojure-eval-region)
		     (:eval-buffer . inf-clojure-eval-buffer)
                     (:indent-sexp . vilpy-clojure-indent)))
    (:cider . ((:decider-fn . (lambda () (bound-and-true-p cider-mode)))
               (:eval-last-sexp . cider-eval-last-sexp)
               (:describe-symbol . vilpy--cider-describe-symbol)
               (:eval-defun . cider-eval-defun-at-point)
               (:eval-region . cider-eval-region)
	       (:eval-buffer . cider-eval-buffer)
               (:indent-sexp . vilpy-clojure-indent)))
    ;; Fallback for clojure, in case `cider` and `inf-clojure` are not activated
    ;; Do not move this up to in this list - that would always ignore cider and inf-clojure,
    ;; which should have higher priority.
    (:clojure . ((:decider-fn . (lambda () (memq major-mode vilpy-clojure-modes)))
                 (:indent-sexp . vilpy-clojure-indent))))
  "An alist that determine which functions will run for language specific features.
Some commands (eg. `vilpy-eval`) consider this list for deciding the appropriate handler
for some feature.
The alist keys are arbitrary, but they tipically represent major or minor modes.
The values are describe below:
`decider-fn`: A function with no arguments that returns non-nil if the set of commands
in this list is appropriate for the current buffer.
`eval-last-sexp`, `eval-defun`, `eval-region`, `describe-symbol` and
`indent-sexp` should be interactive functions.")

(defvar vilpy-elisp-modes
  '(emacs-lisp-mode
    lisp-interaction-mode
    minibuffer-inactive-mode)
  "Modes for which emacs-lisp related functions are appropriate.")

(defvar vilpy-clojure-modes
  '(clojure-mode
    clojure-ts-mode
    clojurescript-mode
    clojurec-mode)
  "Modes for which clojure related functions are appropriate.")

(defvar vilpy-map-input-overlay nil
  "The input overlay for mapping transformations.")

;; TODO: Should this be suspect to comment-char handling as well?
(defvar-local vilpy-outline-header ";;"
  "Store the buffer-local outline start.")

;;* Customization
(defgroup vilpy nil
  "List navigation and editing for the Lisp family."
  :group 'bindings
  :prefix "vilpy-")

(defvar vilpy-left "[([{]"
  "Opening delimiter.")

(defvar vilpy-right "[])}]"
  "Closing delimiter.")

(defvar vilpy-outline "^;;\\(?:;[^#]\\|\\*+\\)"
  "Outline delimiter.")

(defcustom vilpy-no-space nil
  "When non-nil, don't insert a space before parens/brackets/braces/colons."
  :type 'boolean
  :group 'vilpy)
(make-variable-buffer-local 'vilpy-no-space)

(defcustom vilpy-lax-eval t
  "When non-nil, fix \"unbound variable\" error by setting the it to nil.
This is useful when hacking functions with &optional arguments.
So evaling (setq mode (or mode major-mode)) will set mode to nil on
the first eval, and to major-mode on the second eval."
  :type 'boolean
  :group 'vilpy)

(defcustom vilpy-verbose t
  "If t, vilpy will display some messages on error state.
These messages are similar to \"Beginning of buffer\" error for
`backward-char' and can safely be ignored."
  :type 'boolean
  :group 'vilpy)

(defcustom vilpy-close-quotes-at-end-p nil
  "If t, when pressing the `\"' at the end of a quoted string, it will move you past the end quote."
  :type 'boolean
  :group 'vilpy)

(defcustom vilpy-avy-style-char 'pre
  "Method of displaying the overlays for a char during visual selection."
  :type '(choice
          (const :tag "Pre" pre)
          (const :tag "At" at)
          (const :tag "At full" at-full)
          (const :tag "Post" post)))

(defcustom vilpy-avy-style-paren 'at
  "Method of displaying the overlays for a paren during visual selection."
  :type '(choice
          (const :tag "Pre" pre)
          (const :tag "At" at)
          (const :tag "At full" at-full)
          (const :tag "Post" post)))

(defcustom vilpy-avy-style-symbol 'pre
  "Method of displaying the overlays for a symbol during visual selection."
  :type '(choice
          (const :tag "Pre" pre)
          (const :tag "At" at)
          (const :tag "At full" at-full)
          (const :tag "Post" post)))

(defcustom vilpy-avy-keys (number-sequence ?a ?z)
  "Keys for jumping."
  :type '(repeat :tag "Keys" (character :tag "char")))

(defvar vilpy-mode-map (make-sparse-keymap))

(defvar vilpy-ignore-whitespace nil
  "When set to t, function `vilpy-right' will not clean up whitespace.")

(defcustom vilpy-compat '()
  "List of package compatibility options.
Enabling them adds overhead, so make sure that you are actually
using those packages."
  :type '(repeat
          (choice
           (const :tag "god-mode" god-mode)
           (const :tag "magit-blame-mode" magit-blame-mode)
           (const :tag "cider" cider)
           (const :tag "macrostep" macrostep))))

(defvar-local vilpy-old-outline-settings nil
  "Store the old values of `outline-regexp' and `outline-level'.
`vilpy-mode' overrides those while it's on.")

(defcustom vilpy-safe-delete nil
  "When non-nil, killing/deleting an active region keeps delimiters balanced.
This applies to `vilpy-delete', `vilpy-paste', and
`vilpy-delete-backward'."
  :group 'vilpy
  :type 'boolean)

(defcustom vilpy-safe-copy nil
  "When non-nil, `vilpy-copy' won't copy unbalanced delimiters in a region."
  :group 'vilpy
  :type 'boolean)

(defcustom vilpy-safe-paste nil
  "When non-nil, `vilpy-paste' will add missing delimiters."
  :group 'vilpy
  :type 'boolean)

(defcustom vilpy-safe-threshold 1500
  "The max size of an active region that vilpy will try to keep balanced.
This only applies when `vilpy-safe-delete', `vilpy-safe-copy', and/or
`vilpy-safe-paste' are non-nil."
  :group 'vilpy
  :type 'number)

(defcustom vilpy-safe-actions-ignore-strings t
  "When non-nil, don't try to act safely in strings.
Any unmatched delimiters inside of strings will be copied or deleted. This only
applies when `vilpy-safe-delete', `vilpy-safe-copy', and/or `vilpy-safe-paste'
are non-nil."
  :group 'vilpy
  :type 'boolean)

(defcustom vilpy-safe-actions-ignore-comments t
  "When non-nil, don't try to act safely in comments.
Any unmatched delimiters inside of comments will be copied or deleted. This only
applies when `vilpy-safe-delete', `vilpy-safe-copy', and/or `vilpy-safe-paste'
are non-nil."
  :group 'vilpy
  :type 'boolean)

(defcustom vilpy-safe-actions-no-pull-delimiters-into-comments nil
  "When non-nil, don't pull unmatched delimiters into comments when deleting.
This prevents the accidental unbalancing of expressions from commenting out
delimiters. This only applies when `vilpy-safe-delete', `vilpy-safe-copy',
and/or `vilpy-safe-paste' are non-nil."
  :group 'vilpy
  :type 'boolean)

(defcustom vilpy-insert-space-after-wrap t
  "When non-nil, insert a space after the point when wrapping.
This applies to the commands that use `vilpy-pair'."
  :group 'vilpy
  :type 'boolean)

(defcustom vilpy-thread-last-macro "thread-last"
  "Threading macro to use by default in command `vilpy-thread-last'."
  :type '(radio
          (const :tag "Elisp" "thread-last")
          (const :tag "Clojure" "->>")
          (string :tag "Custom")))

(defun vilpy-comment-char (&optional level postfix)
  "Get the `comment-start' character, or `;' if nil, repeated LEVEL times concated with POSTFIX."
  (concat
   (apply #'concat (make-list (or level 1) (or comment-start ";")))
   (or postfix "")))

;;;###autoload
(define-minor-mode vilpy-mode
  "Minor mode for navigating and editing LISP dialects.

When `vilpy-mode' is on, most unprefixed keys,
i.e. [a-zA-Z+-./<>], conditionally call commands instead of
self-inserting. The condition (called special further on) is one
of:

- the point is before \"(\"
- the point is after \")\"
- the region is active

For instance, when special, \"j\" moves down one sexp, otherwise
it inserts itself.

When special, [0-9] call `digit-argument'.

When `vilpy-mode' is on, \"[\" and \"]\" move forward and
backward through lists, which is useful to move into special.

\\{vilpy-mode-map}"
  :keymap vilpy-mode-map
  :group 'vilpy
  :lighter " VP"
  (if vilpy-mode
      (progn
        (require 'eldoc)
        (eldoc-remove-command 'special-vilpy-eval)
        (eldoc-remove-command 'special-vilpy-x)
        (eldoc-add-command 'vilpy-space)
        (setq vilpy-old-outline-settings
              (cons outline-regexp outline-level))
        (setq-local outline-level 'vilpy-outline-level)
        (cond ((eq major-mode 'latex-mode)
               (setq-local vilpy-outline "^\\(?:%\\*+\\|\\\\\\(?:sub\\)?section{\\)")
               (setq vilpy-outline-header "%")
               (setq-local outline-regexp "\\(?:%\\*+\\|\\\\\\(?:sub\\)?section{\\)"))
              ((eq major-mode 'python-mode)
               (setq-local vilpy-outline "^#\\*+")
               (setq vilpy-outline-header "#")
               (setq-local outline-regexp "#\\*+")
               (setq-local outline-heading-end-regexp "\n"))
              (t
               (setq-local outline-regexp (substring vilpy-outline 1)))))
    (when vilpy-old-outline-settings
      (setq outline-regexp (car vilpy-old-outline-settings))
      (setq outline-level (cdr vilpy-old-outline-settings))
      (setq vilpy-old-outline-settings nil))))

;;* Macros
(defmacro vilpy-dotimes (n &rest bodyform)
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

(defmacro vilpy-save-excursion (&rest body)
  "More intuitive (`save-excursion' BODY)."
  (declare (indent 0))
  `(let ((out (save-excursion
                ,@body)))
     (when (vilpy-bolp)
       (back-to-indentation))
     out))

(defmacro vilpy-from-left (&rest body)
  "Ensure that BODY is executed from start of list."
  (declare (debug (body)))
  (let ((at-start (cl-gensym "at-start")))
    `(let ((,at-start (vilpy--leftp)))
       (unless ,at-start
         (vilpy-other))
       (unwind-protect
            (vilpy-save-excursion
              ,@body)
         (unless (eq ,at-start (vilpy--leftp))
           (vilpy-other))))))

(defmacro vilpy-flet (binding &rest body)
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

(defmacro vilpy-multipop (lst n)
  "Remove LST's first N elements and return them."
  `(if (<= (length ,lst) ,n)
       (prog1 ,lst
         (setq ,lst nil))
     (prog1 ,lst
       (setcdr
        (nthcdr (1- ,n) (prog1 ,lst (setq ,lst (nthcdr ,n ,lst))))
        nil))))

;;* Globals: navigation
(defsubst vilpy-right-p ()
  "Return t if after variable `vilpy-right'."
  (looking-back vilpy-right
                (line-beginning-position)))

(defsubst vilpy-left-p ()
  "Return t if before variable `vilpy-left'."
  (looking-at vilpy-left))

(defsubst vilpy-looking-back (regexp)
  "Forward to (`looking-back' REGEXP)."
  (looking-back regexp (line-beginning-position)))

(defun vilpy-forward (arg)
  "Move forward list ARG times or until error.
Return t if moved at least once,
otherwise call function `vilpy-right' and return nil."
  (interactive "p")
  (when (= arg 0)
    (setq arg 2000))
  (vilpy--exit-string)
  (let ((bnd (vilpy--bounds-comment)))
    (when bnd
      (goto-char (1+ (cdr bnd)))))
  (let ((pt (point))
        (r (vilpy-dotimes arg
             (when (= (point) (point-max))
               (error "Reached end of buffer"))
             (forward-list))))
    ;; `forward-list' returns true at and of buffer
    (if (or (null r)
            (= pt (point))
            (and (not (vilpy-right-p))
                 (progn
                   (backward-list)
                   (forward-list)
                   (= pt (point)))))
        (prog1 nil
          (vilpy--out-forward 1))
      (point))))

(defun vilpy-backward (arg)
  "Move backward list ARG times or until error.
If couldn't move backward at least once, move up backward and return nil."
  (interactive "p")
  (when (= arg 0)
    (setq arg 2000))
  (vilpy--exit-string)
  (let ((bnd (vilpy--bounds-comment)))
    (when bnd
      (goto-char (car bnd))))
  (let ((pt (point))
        (r (vilpy-dotimes arg
             (when (= (point) (point-min))
               (error "Reached beginning of buffer"))
             (backward-list))))
    ;; `backward-list' returns true at beginning of buffer
    (if (or (null r)
            (= pt (point))
            (and (not (vilpy-left-p))
                 (progn
                   (forward-list)
                   (backward-list)
                   (= pt (point)))))
        (prog1 nil
          (condition-case nil
              (progn
                (vilpy--out-forward 1)
                (backward-list))
            (error
             (progn
               (goto-char pt)
               (up-list -1)))))
      (point))))

(defun vilpy-right (arg)
  "Move outside list forwards ARG times.
Return nil on failure, t otherwise."
  (interactive "p")
  (vilpy--remember)
  (when (bound-and-true-p abbrev-mode)
    (ignore-errors (expand-abbrev)))
  (cond ((region-active-p)
         (vilpy-mark-right arg))
        ((looking-at vilpy-outline)
         (vilpy-outline-right))
        (t
         (vilpy--out-forward arg))))

(defun vilpy-step-out (arg)
  "Move outside list forwards ARG times.
Return nil on failure, t otherwise."
  (interactive "p")
  (vilpy--remember)
  (cond ((region-active-p)
         (vilpy-mark-left arg))
        ((looking-at vilpy-outline)
         (vilpy-outline-left))
        (t
         (or (vilpy--out-backward arg)
             (ignore-errors
               (up-list -1))))))

(defun vilpy-out-forward-newline (arg)
  "Call `vilpy--out-forward', then ARG times `newline-and-indent'."
  (interactive "p")
  (vilpy--out-forward 1)
  (vilpy-dotimes arg
    (newline-and-indent)))

(defun vilpy--re-search-in-code (regexp direction &optional count)
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
      (unless (vilpy--in-string-or-comment-p)
        (cl-decf to-move)))
    (if (= to-move (abs count))
        (progn
          (goto-char pt)
          nil)
      (if (eq direction 'forward)
          (goto-char (match-beginning 0)))
      (- count to-move))))

;;* Locals: navigation
(defun vilpy-step-in (arg)
  "Move inside list ARG times.
Don't enter strings or comments.
Return nil if can't move."
  (interactive "p")
  (vilpy--remember)
  (let ((pt (point))
        r)
    (cond
      ((and (vilpy-bolp)
            (looking-at (vilpy-comment-char)))
       (setq r (vilpy--re-search-in-code vilpy-left 'forward arg)))
      ((vilpy-left-p)
       (setq r (vilpy--re-search-in-code vilpy-left 'forward arg)))
      ((vilpy-right-p)
       (backward-char)
       (when (setq r (vilpy--re-search-in-code vilpy-right 'backward arg))
         (forward-char))))
    (or r
        (progn
          (goto-char pt)
          nil))))

(defun vilpy-down (arg)
  "Move down ARG times inside current list."
  (interactive "p")
  (vilpy--remember)
  (cond ((region-active-p)
         (let ((leftp (= (point) (region-beginning))))
           (when leftp
             (exchange-point-and-mark))
           (cond ((save-excursion
                    (skip-chars-forward " \n")
                    (eobp)))
                 ((vilpy--symbolp (vilpy--string-dwim))
                  (vilpy-dotimes arg
                    (when (vilpy-slurp 1)
                      (vilpy-other)
                      (vilpy-barf 1)
                      (vilpy-other))))

                 ((looking-at "[\n ]+\\(;\\)")
                  (deactivate-mark)
                  (goto-char (match-beginning 1))
                  (vilpy--mark (vilpy--bounds-comment)))

                 (t
                  (vilpy-dotimes arg
                    (forward-sexp 1)
                    (vilpy-other)
                    (if (vilpy--in-comment-p)
                        (progn
                          (goto-char (1+ (cdr (vilpy--bounds-comment))))
                          (skip-chars-forward "\n"))
                      (forward-sexp 2)
                      (forward-sexp -1))
                    (vilpy-other))))
           (when leftp
             (exchange-point-and-mark))))

        ((vilpy-left-p)
         (vilpy-forward arg)
         (let ((pt (point))
               (vilpy-ignore-whitespace t))
           (if (vilpy-forward 1)
               (vilpy-backward 1)
             (goto-char pt)
             (vilpy-other))))

        ((vilpy-right-p)
         (let ((pt (point)))
           (unless (vilpy-forward arg)
             (goto-char pt))))

        ((or (looking-at vilpy-outline)
             (and (bolp) (looking-at (vilpy-comment-char))))
         (let ((pt (point))
               (outline-regexp vilpy-outline))
           (vilpy-dotimes arg
             (outline-next-visible-heading 1)
             (if (looking-at vilpy-outline)
                 (setq pt (point))
               (goto-char pt)
               (error "Last outline reached")))))

        (t
         (vilpy-forward 1)
         (vilpy-backward 1)))
  (vilpy--ensure-visible))

(defun vilpy-up (arg)
  "Move up ARG times inside current list."
  (interactive "p")
  (vilpy--remember)
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
                  (vilpy--mark (vilpy--bounds-comment))
                  (exchange-point-and-mark))
                 ((vilpy--symbolp (vilpy--string-dwim))
                  (vilpy-dotimes arg
                    (when (vilpy-slurp 1)
                      (vilpy-other)
                      (vilpy-barf 1)
                      (vilpy-other))))
                 (t
                  (vilpy-dotimes arg
                    (backward-sexp 1)
                    (vilpy-other)
                    (if (vilpy--in-comment-p)
                        (progn
                          (goto-char (1- (car (vilpy--bounds-comment))))
                          (skip-chars-backward "\n"))
                      (backward-sexp 2)
                      (backward-sexp -1))
                    (vilpy-other))))
           (unless leftp
             (exchange-point-and-mark))))

        ((vilpy-left-p)
         (let ((pt (point)))
           (unless (vilpy-backward arg)
             (goto-char pt))))

        ((vilpy-right-p)
         (vilpy-backward arg)
         (let ((pt (point)))
           (if (vilpy-backward 1)
               (vilpy-forward 1)
             (goto-char pt)
             (vilpy-other))))

        ((or (looking-at vilpy-outline)
             (and (bolp) (looking-at (vilpy-comment-char))))
         (let ((pt (point))
               (outline-regexp vilpy-outline))
           (vilpy-dotimes arg
             (outline-previous-visible-heading 1)
             (if (looking-at vilpy-outline)
                 (setq pt (point))
               (goto-char pt)
               (error "First outline reached")))))
        (t
         (vilpy-backward 1)
         (vilpy-forward 1)))
  (vilpy--ensure-visible))

(defvar vilpy-pos-ring (make-ring 100)
  "Ring for point/mark position and restriction history.")

(defun vilpy--remember ()
  "Store the current point and mark in history."
  (let* ((emptyp (zerop (ring-length vilpy-pos-ring)))
         (top (unless emptyp
                (ring-ref vilpy-pos-ring 0)))
         (restriction (when (buffer-narrowed-p)
                        (cons (set-marker (make-marker)
                                          (point-min))
                              (set-marker (make-marker)
                                          (point-max))))))
    (if (region-active-p)
        (let* ((bnd (vilpy--bounds-dwim))
               (bnd (cons
                     (move-marker (make-marker) (car bnd))
                     (move-marker (make-marker) (cdr bnd)))))
          (when (or emptyp
                    (not (equal bnd top)))
            (ring-insert vilpy-pos-ring (list bnd restriction))))
      (when (or emptyp
                (not (equal (point-marker) top)))
        (ring-insert vilpy-pos-ring (list (point-marker) restriction))))))

(defvar vilpy-back-restore-restriction t
  "When non-nil, restore buffer restriction on `vilpy-back'.")

(defun vilpy-back (arg)
  "Move point to ARGth previous position.
If position isn't special, move to previous or error."
  (interactive "p")
  (when (buffer-narrowed-p)
    (widen))
  (vilpy-dotimes arg
    (if (zerop (ring-length vilpy-pos-ring))
        (vilpy--complain "At beginning of point history")
      (let* ((data (ring-remove vilpy-pos-ring 0))
             (marker (pop data))
             (restriction (pop data))
             (beg (car restriction))
             (end (cdr restriction)))
        ;; After deleting some text, markers that point to it converge
        ;; to one point
        (while (and (not (zerop (ring-length vilpy-pos-ring)))
                    (equal (ring-ref vilpy-pos-ring 0)
                           marker))
          (ring-remove vilpy-pos-ring 0))
        (if (consp marker)
            (vilpy--mark marker)
          (deactivate-mark)
          (switch-to-buffer (marker-buffer marker))
          (goto-char marker))
        (when (and vilpy-back-restore-restriction
                   restriction)
          (narrow-to-region beg end)
          (set-marker beg nil)
          (set-marker end nil))))))

(defun vilpy-knight-down ()
  "Make a knight-like move: down and right."
  (interactive)
  (cond ((vilpy-right-p)
         (vilpy-other))
        ((vilpy-left-p))
        (t (vilpy-backward 1)))
  (let ((pt (point))
        (bnd (save-excursion
               (vilpy-beginning-of-defun)
               (vilpy--bounds-list))))
    (catch 'done
      (while t
        (forward-line)
        (cond ((>= (point) (cdr bnd))
               (goto-char pt)
               (throw 'done nil))
              ((looking-at (concat "\\s-*" vilpy-left))
               (goto-char (1- (match-end 0)))
               (throw 'done t)))))))

(defun vilpy-knight-up ()
  "Make a knight-like move: up and right."
  (interactive)
  (cond ((vilpy-right-p)
         (vilpy-other))
        ((vilpy-left-p))
        (t (vilpy-backward 1)))
  (let ((pt (point))
        (bnd (save-excursion
               (vilpy-beginning-of-defun)
               (vilpy--bounds-list))))
    (catch 'done
      (while t
        (beginning-of-line 0)
        (cond ((< (point) (car bnd))
               (goto-char pt)
               (throw 'done nil))
              ((looking-at (concat "\\s-*" vilpy-left))
               (goto-char (1- (match-end 0)))
               (throw 'done t)))))))

(defun vilpy-other ()
  "Switch to the different side of current sexp."
  (interactive)
  (cond ((and (region-active-p)
              (not (= (region-beginning) (region-end))))
         (exchange-point-and-mark))
        ((vilpy-right-p)
         (backward-list))
        ((vilpy-left-p)
         (forward-list))
        (t
         (user-error "Unexpected"))))

(defun vilpy-go-to-first-defun ()
  "Sets the mark and moves the point to the first defun."
  (interactive)
  (push-mark)
  (vilpy-beginning-of-defun)
  (let ((previous-point (point)))
    (vilpy-up 1)
    (while (not (= previous-point (point)))
      (setq previous-point (point))
      (vilpy-up 1)))
  (message "Mark saved where command was called"))

(defun vilpy-go-to-last-defun ()
  "Sets the mark and moves the point to the last defun."
  (interactive)
  (push-mark)
  (vilpy-beginning-of-defun)
  (let ((previous-point (point)))
    (vilpy-down 1)
    (while (not (= previous-point (point)))
      (setq previous-point (point))
      (vilpy-down 1)))
  (message "Mark saved where command was called"))

;;* Globals: kill, yank, delete, mark, copy
(defun vilpy-kill ()
  "Kill line, keeping parens consistent."
  (interactive)
  (let (bnd)
    (cond ((or (vilpy--in-comment-p)
               (and (looking-at " *;")
                    (save-excursion
                      (goto-char (match-end 0))
                      (vilpy--in-comment-p))))
           (kill-line))

          ((and (setq bnd (vilpy--bounds-string))
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
           (vilpy--indent-for-tab))
          ((and (looking-at vilpy-right) (looking-back vilpy-left
                                                       (line-beginning-position)))
           (delete-char 1)
           (backward-delete-char 1))
          ((vilpy-left-p)
           (if (progn
                 (setq bnd (vilpy--bounds-list))
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
               (when (setq bnd (vilpy--bounds-comment))
                 (goto-char (cdr bnd))))
             (skip-chars-forward " \t")
             (kill-region beg (point)))))))

(defun vilpy-kill-word (arg)
  "Kill ARG words, keeping parens consistent."
  (interactive "p")
  (if (< arg 0)
      (vilpy-backward-kill-word (- arg))
    (let (bnd)
      (vilpy-dotimes arg
        (while (not (or (eobp)
                        (memq (char-syntax (char-after))
                              '(?w ?_))))
          (forward-char 1))
        (when (or (vilpy-looking-back (concat vilpy-left " +"))
                  (vilpy-looking-back (vilpy-comment-char 1 " +")))
          (delete-horizontal-space))
        (if (setq bnd (vilpy--bounds-string))
            (save-restriction
              (narrow-to-region (1+ (car bnd)) (1- (cdr bnd)))
              (kill-word 1)
              (widen))
          (kill-word 1))))))

(defun vilpy-backward-kill-word (arg)
  "Kill ARG words backward, keeping parens consistent."
  (interactive "p")
  (let (bnd
        (pt (point))
        (last-command (if (eq last-command 'vilpy-backward-kill-word)
                          'kill-region
                        last-command)))
    (vilpy-dotimes arg
      (when (vilpy--in-comment-p)
        (skip-chars-backward " \n"))
      (if (memq (char-syntax (char-before))
                '(?w ?_ ?\s))
          (if (vilpy-looking-back "\\_<\\s_+")
              (delete-region (match-beginning 0)
                             (match-end 0))
            (backward-kill-word 1)
            (when (and (vilpy--in-string-p)
                       (not (vilpy-looking-back "\\\\\\\\"))
                       (vilpy-looking-back "\\\\"))
              (delete-char -1)))
        (delete-region (point) pt)
        (while (not (or (bobp)
                        (memq (char-syntax (char-before))
                              '(?w ?_))))
          (backward-char 1))
        (if (setq bnd (vilpy--bounds-string))
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
                               (when (and (not (vilpy-looking-back "\\\\\\\\"))
                                          (vilpy-looking-back "\\\\"))
                                 (backward-char))
                               (point))
                             (point-max))
                (widen)))
          (backward-kill-word 1))))))

(defvar vilpy-delete-sexp-from-within nil
  "When cursor is adjacent to an opening or closing pair,
`vilpy-delete' or `vilpy-delete-backward' toward the delimiter
will kill the whole sexp (string or list).")

(defun vilpy-delete (arg)
  "Delete ARG sexps."
  (interactive "p")
  (let (bnd)
    (cond ((< arg 0)
           (vilpy-delete-backward (- arg)))

          ((region-active-p)
           (vilpy--maybe-safe-delete-region (region-beginning) (region-end)))

          ((setq bnd (vilpy--bounds-string))
           (cond ((eq (1+ (point)) (cdr bnd))
                  (goto-char (car bnd))
                  (when vilpy-delete-sexp-from-within
                    (vilpy-delete arg)))
                 ((looking-at "\\\\\"")
                  (if (eq (+ (point) 2) (cdr bnd))
                      (goto-char (car bnd))
                    (delete-char 2)))
                 ((and (looking-at "\"")
                       (vilpy-looking-back "\\\\"))
                  (backward-char 1)
                  (delete-char 2))
                 ((vilpy--delete-pair-in-string "\\\\\\\\(" "\\\\\\\\)"))
                 ((looking-at "\\\\\\\\")
                  (delete-char 2))
                 ((and (looking-at "\\\\")
                       (vilpy-looking-back "\\\\"))
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
                    (vilpy--in-string-or-comment-p))
                  (delete-char arg))
                 (t
                  (vilpy--exit-string))))

          ((vilpy--in-comment-p)
           (if (vilpy-bolp)
               (let ((bnd (vilpy--bounds-comment)))
                 (delete-region (car bnd) (cdr bnd)))
             (delete-char arg)))

          ((looking-at vilpy-right)
           (vilpy-step-out 1)
           (when vilpy-delete-sexp-from-within
             (vilpy-delete arg)))

          ((vilpy-left-p)
           (vilpy--delete-leading-garbage)
           (vilpy-dotimes arg
             (vilpy--delete)))

          ((eolp)
           (delete-char 1)
           (let ((pt (point)))
             (skip-chars-forward " ")
             (delete-region pt (point))
             (unless (or (eolp)
                         (bolp)
                         (vilpy-bolp)
                         (eq (char-before) ?\ ))
               (insert " "))))

          (t
           (delete-char arg)))))

(defun vilpy--delete-leading-garbage ()
  "Delete any syntax before an opening delimiter such as '.
Delete backwards to the closest whitespace char or opening delimiter or to the
beginning of the line."
  (let ((pt (point))
        (end (save-excursion (re-search-backward ")" nil t))))
    (re-search-backward
     (concat "[[:space:]]" "\\|" vilpy-left "\\|" "^")
     end t)
    (goto-char (match-end 0))
    (delete-region (point) pt)))

(defun vilpy--delete-whitespace-backward ()
  "Delete spaces backward."
  (let ((pt (point)))
    (skip-chars-backward " ")
    (delete-region (point) pt)))

(defvar vilpy-delete-backward-recenter -20
  "When cursor is near top of screen when calling
  `vilpy-delete-backward', recenter cursor with arg.")

(defun vilpy-delete-backward (arg)
  "From \")|\", delete ARG sexps backwards.
Otherwise (`backward-delete-char-untabify' ARG)."
  (interactive "p")
  (let (bnd)
    (cond ((< arg 0)
           (vilpy-delete (- arg)))

          ((use-region-p)
           (vilpy--maybe-safe-delete-region (region-beginning)
                                            (region-end)))
          ((bobp))

          ((and (setq bnd (vilpy--bounds-string))
                (not (eq (point) (car bnd))))
           (cond ((eq (- (point) (car bnd)) 1)
                  (goto-char (cdr bnd))
                  (if vilpy-delete-sexp-from-within
                      (vilpy-delete-backward arg)))
                 ((or (looking-back "\\\\\\\\(" (car bnd))
                      (looking-back "\\\\\\\\)" (car bnd)))
                  (let ((pt (point)))
                    (goto-char (match-beginning 0))
                    (unless (vilpy--delete-pair-in-string
                             "\\\\\\\\(" "\\\\\\\\)")
                      (goto-char pt)
                      (backward-delete-char-untabify arg))))
                 ((looking-back "[^\\]\\\\[^\\]" (car bnd))
                  (backward-delete-char 2))
                 (t
                  (backward-delete-char-untabify arg))))

          ((looking-at vilpy-outline)
           (if (vilpy-looking-back (concat vilpy-outline ".*\n"))
               (delete-region
                (match-beginning 0)
                (match-end 0))
             (delete-char -1)))

          ((vilpy--in-comment-p)
           (cond ((vilpy-looking-back "^ +")
                  (delete-region (max (1- (match-beginning 0))
                                      (point-min))
                                 (match-end 0))
                  (vilpy--indent-for-tab))
                 ((and (looking-at "$") (vilpy-looking-back (vilpy-comment-char 1 " +")))
                  (let ((pt (point)))
                    (skip-chars-backward " ;")
                    (delete-region (point) pt)
                    (if (vilpy-looking-back "^")
                        (vilpy--indent-for-tab)
                      (let ((p (point)))
                        (vilpy--out-forward 1)
                        (vilpy--prettify-1)
                        (goto-char p)))))
                 (t
                  (backward-delete-char-untabify arg))))

          ((vilpy-looking-back "\\\\.")
           (backward-delete-char-untabify arg))

          ((and (vilpy-looking-back (concat vilpy-right " "))
                (looking-at " *$"))
           (backward-delete-char-untabify arg))

          ((or (and (vilpy-right-p)
                    (or (memq major-mode vilpy-clojure-modes)
                        (not (vilpy-looking-back "[\\?]."))))
               (and (vilpy-looking-back (concat vilpy-right " "))
                    (or (vilpy-left-p) (looking-at "\""))))
           (let ((pt (point)))
             (vilpy-backward arg)
             (unless (vilpy-right-p)
               (vilpy--skip-delimiter-preceding-syntax-backward))
             (skip-chars-backward " \t")
             (while (plist-get (text-properties-at (point)) 'read-only)
               (forward-char))
             (delete-region (point) pt)
             (unless (or (looking-at " ")
                         (vilpy-bolp)
                         (and (vilpy-right-p)
                              (not (or (vilpy-left-p)
                                       (looking-at "\""))))
                         (vilpy-looking-back vilpy-left)
                         ;; REPL prompt, e.g. `ielm'
                         (vilpy-after-string-p "> "))
               (just-one-space))
             (setq pt (point))
             (if (and
                  (not (vilpy-bolp))
                  (not (vilpy-left-p))
                  (progn
                    (skip-chars-backward " \t\n")
                    (vilpy-right-p)))
                 (delete-region (point) pt)
               (goto-char pt)
               (vilpy--indent-for-tab))))

          ((and (vilpy-looking-back vilpy-left)
                (not (vilpy-looking-back "[\\?].")))
           (vilpy--out-forward 1)
           (vilpy-delete-backward 1))

          ((eq (char-before) ?\")
           (backward-char 1)
           (let ((bnd (vilpy--bounds-string)))
             (delete-region (car bnd)
                            (cdr bnd))
             (vilpy--delete-whitespace-backward)
             (unless (looking-at " ")
               (insert " "))
             (vilpy--indent-for-tab)))

          ((and (vilpy-after-string-p "\" ")
                (not (looking-at vilpy-right)))
           (let ((pt (point)))
             (backward-char 2)
             (delete-region (car (vilpy--bounds-string)) pt))
           (vilpy--delete-whitespace-backward)
           (unless (vilpy-looking-back vilpy-left)
             (just-one-space))
           (vilpy--indent-for-tab))

          ((vilpy-bolp)
           (delete-region
            (line-beginning-position)
            (point))
           (unless (bobp)
             (if (and (not (eolp))
                      (save-excursion
                        (backward-char 1)
                        (vilpy--in-comment-p)))
                 (progn
                   (backward-char 1)
                   (let ((bnd (vilpy--bounds-comment)))
                     (delete-region (car bnd) (cdr bnd)))
                   (delete-char 1))
               (backward-delete-char 1)
               (unless (or (eolp)
                           (looking-at vilpy-right)
                           (vilpy-looking-back vilpy-left))
                 (just-one-space)))
             (vilpy--indent-for-tab)))

          ((vilpy-looking-back "[^ ]  +")
           (delete-region (+ (match-beginning 0) 2) (point)))

          (t
           (backward-delete-char-untabify arg))))
  (when (and (buffer-file-name)
             (< (- (line-number-at-pos (point))
                   (line-number-at-pos (window-start)))
                5)
             vilpy-delete-backward-recenter)
    (ignore-errors
      (recenter vilpy-delete-backward-recenter)))
  (when (and (vilpy-left-p)
             (not (vilpy--in-string-or-comment-p)))
    (indent-sexp)))

(defun vilpy-mark ()
  "Mark the quoted string or the list that includes the point.
Extend region when it's aleardy active."
  (interactive)
  (let ((bounds (or (vilpy--bounds-comment)
                    (vilpy--bounds-string)
                    (vilpy--bounds-list))))
    (when bounds
      (vilpy--mark bounds))))

(defun vilpy-mark-list (arg)
  "Mark list from special position.
When ARG is more than 1, mark ARGth element."
  (interactive "p")
  (when (called-interactively-p 'interactive)
    (vilpy--remember))
  (cond ((> arg 1)
         (vilpy-mark-car)
         (vilpy-down (1- arg)))
        ((= arg 0)
         (let ((bnd (vilpy--bounds-dwim)))
           (vilpy--mark
            (cons (+ (car bnd) (if (eq (char-after (car bnd)) ?\#) 2 1))
                  (1- (cdr bnd))))))
        ((region-active-p)
         (deactivate-mark)
         (if (vilpy--in-comment-p)
             (progn
               (beginning-of-line)
               (skip-chars-forward " "))
           (skip-chars-forward ",@'`")))
        ((vilpy-left-p)
         (vilpy--mark
          (vilpy--bounds-dwim)))
        ((vilpy-right-p)
         (vilpy--mark
          (vilpy--bounds-dwim))
         (vilpy-other))
        ((and (vilpy-bolp) (looking-at (vilpy-comment-char)))
         (vilpy--mark (vilpy--bounds-comment))))
  (setq this-command 'vilpy-mark-list))

(declare-function evil-execute-in-normal-state "ext:evil-execute-in-normal-state")
(defun vilpy-execute-in-normal-state ()
  "Execute `evil-execute-in-normal-state` and restore the previous evil state."
  (interactive)
  ;; `evil-execute-in-normal-state` uses `this-command` internally for deciding if the
  ;; previous position will be restored. We need to to set it here, otherwise `this-command`
  ;; would get a `special` prefix and the it wouldn't get back to insert state.
  (setq this-command 'evil-execute-in-normal-state)
  (evil-execute-in-normal-state))

(defvar-local vilpy-bind-var-in-progress nil)

(defun vilpy-mark-symbol ()
  "Mark current symbol."
  (interactive)
  (let (bnd)
    (cond (vilpy-bind-var-in-progress
           (vilpy-map-done)
           (setq vilpy-bind-var-in-progress nil)
           (forward-sexp 2)
           (vilpy-mark-symbol))

          ((vilpy--in-comment-p)
           (if (and (looking-at "\\(?:\\w\\|\\s_\\)*'")
                    (setq bnd (match-end 0))
                    (looking-back "`\\(?:\\w\\|\\s_\\)*"
                                  (line-beginning-position)))
               (progn
                 (goto-char (match-beginning 0))
                 (set-mark (point))
                 (goto-char bnd))
             (vilpy--mark (vilpy--bounds-comment))))

          ((and
            (not (region-active-p))
            (setq bnd (vilpy--bounds-string))
            (= (1+ (point))
               (cdr bnd)))
           (vilpy--mark bnd))

          ((and (vilpy-after-string-p "\"")
                (not (vilpy--in-string-or-comment-p)))
           (set-mark-command nil)
           (forward-sexp -1)
           (exchange-point-and-mark))

          ((looking-at " *[[({]")
           (if (and (vilpy-looking-back "\\sw\\|\\s_")
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
                       (while (vilpy--in-string-or-comment-p)
                         (re-search-forward "[() \n]"))
                       (backward-char 1))
                   (error
                    (message "No further symbols found")
                    (deactivate-mark)
                    (goto-char pt)))))))

          ((region-active-p)
           (let ((bnd (vilpy--bounds-string)))
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

          ((vilpy-right-p)
           (skip-chars-backward "}]) \n")
           (set-mark-command nil)
           (re-search-backward "[][{}() \n]")
           (while (vilpy--in-string-or-comment-p)
             (re-search-backward "[() \n]"))
           (forward-char 1))

          ((looking-at vilpy-right)
           (vilpy--mark
            (save-excursion
              (backward-char 1)
              (vilpy--bounds-dwim))))

          (t
           (vilpy--mark (vilpy--bounds-dwim))))))

(defun vilpy-copy ()
  "Copy marked region or sexp to kill ring."
  (interactive)
  (let ((str (if (region-active-p)
                 (vilpy--maybe-safe-region (region-beginning)
                                           (region-end))
               (vilpy--string-dwim))))
    (unless (equal str (ignore-errors
                         (current-kill 0)))
      (kill-new str))))

;;* Globals: pairs
(defvar vilpy-parens-only-left-in-string-or-comment t
  "Whether \"(\" should insert only the left paren in strings and comments.")

(defun vilpy-pair (left right preceding-syntax-alist)
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
            (vilpy--surround-region ,left ,right)
            (when (and (vilpy-looking-back vilpy-left)
                       (or (vilpy-left-p)
                           (> (or arg 0) 0)))
              (insert " "))
            (backward-char 1))
           ((and (vilpy--in-string-p)
                 (vilpy-looking-back "\\\\\\\\"))
            (insert ,left "\\\\" ,right)
            (backward-char 3))
           ((vilpy--in-string-or-comment-p)
            (if (and vilpy-parens-only-left-in-string-or-comment
                     (string= ,left "(")
                     (= ?\( (aref (this-command-keys-vector) 0)))
                (insert "(")
              (insert ,left ,right)
              (backward-char 1)))
           ((vilpy-after-string-p "?\\")
            (insert ,left))
           ((not arg)
            (vilpy--indent-for-tab)
            (vilpy--delimiter-space-unless ,preceding-syntax-alist)
            (insert ,left ,right)
            (unless (or (eolp)
                        (vilpy--in-string-p)
                        (looking-at "\n\\|)\\|}\\|\\]"))
              (just-one-space)
              (backward-char 1))
            (when (looking-at ,(regexp-quote left))
              (insert " ")
              (backward-char))
            (backward-char))
           (t
            ;; don't jump backwards or out of a list when not at a sexp
            (unless (or (vilpy--not-at-sexp-p ,preceding-syntax-alist)
                        (and (memq major-mode vilpy-clojure-modes)
                             (looking-at vilpy-left)
                             (vilpy-after-string-p "#")))
              (when (vilpy--bounds-dwim)
                (goto-char (car (vilpy--bounds-dwim)))))
            (vilpy--indent-for-tab)
            (insert ,left ,right)
            (save-excursion
              (vilpy-slurp arg))
            (when (or (looking-at vilpy-right)
                      (and (eolp)
                           (looking-back vilpy-right (1- (point)))))
              ;; failed to wrap anything
              (backward-char))
            (when (and vilpy-insert-space-after-wrap
                       (not (vilpy--in-empty-list-p ,preceding-syntax-alist))
                       (not (eolp)))
              (just-one-space)
              (backward-char))))))

(defvar vilpy-parens-preceding-syntax-alist
  '((lisp-mode . ("[#`',.@]+" "#[0-9]*" "#[.,Ss+-]" "#[0-9]+[=Aa]"))
    (emacs-lisp-mode . ("[#`',@]+" "#s" "#[0-9]+="))
    (clojure-mode . ("[`'~@]+" "#" "#\\?@?"))
    (clojure-ts-mode . ("[`'~@]+" "#" "#\\?@?"))
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
`vilpy-parens'.")

(defvar vilpy-braces-preceding-syntax-alist
  '((clojure-mode . ("[`'^]" "#[:]*[A-z.:]*"))
    (clojure-ts-mode . ("[`'^]" "#[:]*[A-z.:]*"))
    (clojurescript-mode . ("[`'^]" "#[:]*[A-z.:]*"))
    (clojurec-mode . ("[`'^]" "#[:]*[A-z.:]*"))
    (cider-repl-mode . ("[`'^]" "#[:]*[A-z.:]*"))
    (cider-clojure-interaction-mode . ("[`'^]" "#[:]*[A-z.:]*"))
    (janet-mode . ("[@;]"))
    (t . nil))
  "An alist of `major-mode' to a list of regexps.
Each regexp describes valid syntax that can precede an opening brace in that
major mode. These regexps are used to determine whether to insert a space for
`vilpy-braces'.")

(defalias 'vilpy-parens
    (vilpy-pair "(" ")" 'vilpy-parens-preceding-syntax-alist)
  "`vilpy-pair' with ().")

(defalias 'vilpy-braces
    (vilpy-pair "{" "}" 'vilpy-braces-preceding-syntax-alist)
  "`vilpy-pair' with {}.")

(defun vilpy-quotes (arg)
  "Insert a pair of quotes around the point.

When the region is active, wrap it in quotes instead.
When inside string, if ARG is nil quotes are quoted,
otherwise the whole string is unquoted."
  (interactive "P")
  (let (bnd)
    (cond ((region-active-p)
           (if arg
               (vilpy-unstringify)
             (vilpy-stringify)))
          ((and (setq bnd (vilpy--bounds-string))
                (not (= (point) (car bnd))))
           (if arg
               (vilpy-unstringify)
             (if (and vilpy-close-quotes-at-end-p (looking-at "\""))
                 (forward-char 1)
                 (progn (insert "\\\"\\\""))
               (backward-char 2))))

          (arg
           (vilpy-stringify))

          ((vilpy-after-string-p "?\\")
           (self-insert-command 1))

          (t
           (vilpy--space-unless "^\\|\\s-\\|\\s(\\|[#]")
           (insert "\"\"")
           (unless (looking-at "\n\\|)\\|}\\|\\]\\|$")
             (just-one-space)
             (backward-char 1))
           (backward-char)))))

(defun vilpy-open-line-below ()
  "Insert an empty line below the current line and move the point to it."
  (interactive)
  (end-of-line)
  (open-line 1)
  (forward-char)
  (vilpy--indent-for-tab))

(defun vilpy-open-line-above ()
  "Insert parenthesis in an empty line above the current line and move the point to them."
  (interactive)
  (end-of-line 0)
  (open-line 1)
  (forward-char)
  (vilpy--indent-for-tab))

(defun vilpy-parens-down ()
  "Exit the current sexp, and start a new sexp below."
  (interactive)
  (condition-case nil
      (progn
        (vilpy--out-forward 1)
        (if (looking-at "\n *\\()\\)")
            (progn
              (goto-char (match-beginning 1))
              (insert "()")
              (vilpy--indent-for-tab)
              (backward-char))

          (insert "\n()")
          (vilpy--indent-for-tab)
          (backward-char)))
    (error (indent-new-comment-line))))

;;* Globals: insertion
(defun vilpy-space (arg)
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
        ((vilpy--in-string-or-comment-p)
         (call-interactively 'self-insert-command))
        ((eq arg 4)
         (when (vilpy--leftp)
           (vilpy-other))
         (backward-char)
         (unless (vilpy-bolp)
           (newline-and-indent)))
        ((or (eq arg 2)
             (when (eq arg 3)
               (vilpy-other)
               t))

         (if (vilpy-left-p)
             (progn
               (forward-char)
               (just-one-space)
               (backward-char))
           (backward-char)
           (just-one-space)))
        ((and (vilpy-looking-back vilpy-left)
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
         (when (and (vilpy-left-p)
                    (vilpy-looking-back "[[({] "))
           (backward-char)))))

(defvar vilpy-brackets-preceding-syntax-alist
  '((clojure-mode . ("[`']" "#[A-z.]*"))
    (clojure-ts-mode . ("[`']" "#[A-z.]*"))
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
`vilpy-brackets'.")

(defalias 'vilpy-brackets
    (vilpy-pair "[" "]" 'vilpy-brackets-preceding-syntax-alist)
    "`vilpy-pair' with [].")

(defun vilpy-tick (arg)
  "Insert ' ARG times.
When the region is active and marks a string, unquote it.
Otherwise, when the region is active, toggle ' at the start of the region."
  (interactive "p")
  (cond ((vilpy--string-markedp)
         (vilpy-unstringify))
        ((region-active-p)
         (vilpy-toggle-char ?\'))
        (t
         (vilpy--space-unless "\\s-\\|\\s(\\|[~#:?'`]\\|\\\\")
         (self-insert-command arg))))

(defun vilpy-underscore (&optional arg)
  "Insert _ ARG times.
For Clojure modes, toggle #_ sexp comment."
  (interactive "p")
  (setq arg (or arg 1))
  (if (memq major-mode vilpy-clojure-modes)
      (let ((leftp (vilpy--leftp)))
        (unless leftp
          (vilpy-other))
        (if (vilpy-after-string-p "#_")
            (delete-char -2)
          (insert "#_"))
        (unless leftp
          (vilpy-other)))
    (self-insert-command arg)))

(defun vilpy-backtick ()
  "Insert `.
When the region is active, surrounds it with backticks."
  (interactive)
  (if (region-active-p)
      (vilpy--surround-region "`" "'")
    (vilpy--space-unless "\\s-\\|\\s(\\|[:?`']\\|\\\\")
    (insert "`")))

(defun vilpy-toggle-char (char)
  "Toggle CHAR at the start of the region."
  (let ((bnd (vilpy--bounds-dwim))
        deactivate-mark)
    (save-excursion
      (goto-char (car bnd))
      (if (eq (char-after) char)
          (delete-char 1)
        (insert char)))))

(defun vilpy-hash ()
  "Insert #."
  (interactive)
  (if (and (or (memq major-mode vilpy-clojure-modes)
               (memq major-mode '(nrepl-repl-mode
                                  cider-clojure-interaction-mode)))
           (vilpy-looking-back "\\sw #"))
      (progn
        (backward-delete-char 2)
        (insert "#"))
    (vilpy--space-unless "\\s-\\|\\s(\\|[#:?'`,]\\\\?")
    (insert "#")))

(declare-function cider-eval-print-last-sexp "ext:cider-eval")
(declare-function cider-repl-newline-and-indent "ext:cider-repl")
(declare-function ielm-return "ielm")
(declare-function mode-local-bind "mode-local")

(defun vilpy-newline-and-indent ()
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
         (vilpy-newline-and-indent-plain))
        ((vilpy-left-p)
         (skip-chars-backward ",@'`#")
         (newline-and-indent)
         (skip-chars-forward ",@'`#")
         (indent-sexp))
        (t
         (vilpy-newline-and-indent-plain))))

(declare-function cider-repl-return "ext:cider-repl")
(declare-function slime-repl-return "ext:slime-repl")
(declare-function sly-mrepl-return "ext:sly-mrepl")
(declare-function racket-repl-submit "ext:racket-repl")
(defun vilpy-newline-and-indent-plain ()
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
       (if (and (not (vilpy--in-string-or-comment-p))
                (if (memq major-mode vilpy-clojure-modes)
                    (vilpy-looking-back "[^#`'@~][#`'@~]+")
                  (vilpy-looking-back "[^#`',@|][#`',@]+")))
           (save-excursion
             (goto-char (match-beginning 0))
             (newline-and-indent))
         (newline-and-indent))
       (let ((vilpy-ignore-whitespace t))
         (save-excursion
           (vilpy--out-backward 1)
           (unless (< 50000
                      (- (save-excursion (forward-list 1))
                         (point)))
             (indent-sexp))))))))

;;* Globals: miscellanea
(defun vilpy-string-oneline ()
  "Convert current string to one line."
  (interactive)
  (when (eq (char-before) ?\")
    (backward-char 1))
  (let (bnd str)
    (setq str (vilpy--string-dwim (setq bnd (vilpy--bounds-string))))
    (delete-region (car bnd) (cdr bnd))
    (insert (replace-regexp-in-string "\n" "\\\\n" str))))

;;* Locals: navigation
;;* Locals: Paredit transformations
(defun vilpy--sub-slurp-forward (arg)
  "Grow current marked symbol by ARG words forwards.
Return the amount of successful grow steps, nil instead of zero."
  (when (looking-at "\\s_")
    (let ((end (cdr (bounds-of-thing-at-point 'symbol)))
          prev)
      (vilpy-dotimes arg
        (setq prev (point))
        (forward-word 1)
        (when (> (point) end)
          (goto-char prev)
          (throw 'result (1- i)))))))

(defun vilpy--sub-slurp-backward (arg)
  "Grow current marked symbol by ARG backwards.
Return the amount of successful grow steps, nil instead of zero."
  (when (vilpy-looking-back "\\s_")
    (let ((beg (car (bounds-of-thing-at-point 'symbol)))
          prev)
      (vilpy-dotimes arg
        (setq prev (point))
        (backward-word 1)
        (when (< (point) beg)
          (goto-char prev)
          (throw 'result (1- i)))))))

(defun vilpy-slurp (arg)
  "Grow current sexp by ARG sexps.
If ARG is zero, grow as far as possible. If ARG is -1, grow until the end or
beginning of the line. If it is not possible to slurp to the end of the line,
slurp as far as possible within the line. If before a multi-line list, slurp to
the end of the line where that list ends."
  (interactive "p")
  (if (region-active-p)
      (if (= (point) (region-end))
          (cond ((= arg 0)
                 (while (and (vilpy-dotimes 1 (forward-sexp 1))
                             (not (looking-at "\\'")))))
                ((= arg -1)
                 (while (and (not (looking-at (concat vilpy-right "*$")))
                             (vilpy-dotimes 1 (forward-sexp 1)))))
                ((or (looking-at "\\s_")
                     (save-excursion
                       (goto-char (region-beginning))
                       (and (not (vilpy-left-p))
                            (vilpy-looking-back "\\s_"))))
                 (vilpy--sub-slurp-forward arg))
                ((looking-at "[\n ]+;")
                 (goto-char (match-end 0))
                 (goto-char (cdr (vilpy--bounds-comment))))
                (t
                 (vilpy-dotimes arg
                   (forward-sexp 1))))
        (cond ((= arg 0)
               (while (and (vilpy-dotimes 1 (forward-sexp -1))
                           (not (looking-at "\\`")))))
              ((= arg -1)
               (while (and (not (vilpy-looking-back "^[[:space:]]*"))
                           (vilpy-dotimes 1 (forward-sexp -1)))))
              ((or (and (not (vilpy-left-p))
                        (vilpy-looking-back "\\s_"))
                   (save-excursion
                     (goto-char (region-end))
                     (looking-at "\\s_")))
               (vilpy--sub-slurp-backward arg))
              ((save-excursion
                 (skip-chars-backward " \n")
                 (vilpy--in-comment-p))
               (skip-chars-backward " \n")
               (goto-char (car (vilpy--bounds-comment))))
              (t
               (vilpy-dotimes arg
                 (forward-sexp -1)))))
    (if (vilpy-right-p)
        (cond ((= arg 0)
               (let ((last-pos (point)))
                 (while (and (vilpy-dotimes 1
                               (vilpy--slurp-forward)
                               (vilpy--reindent))
                             (not (= (point) last-pos)))
                   (setq last-pos (point)))))
              ((= arg -1)
               (while (and (not (looking-at (concat "\\("
                                                    vilpy-right
                                                    "\\|$\\)")))
                           (vilpy-dotimes 1
                             (vilpy--slurp-forward)))))
              (t
               (vilpy-dotimes arg
                 (vilpy--slurp-forward))))
      (if (vilpy-left-p)
          (cond ((= arg 0)
                 ;; vilpy--slurp-backward errors when reaching another delimiter
                 (while (and (vilpy-dotimes 1
                               (vilpy--slurp-backward))
                             (not (vilpy-looking-back "\\`")))))
                ((= arg -1)
                 (while (and (not (vilpy-looking-back "^[[:space:]]*"))
                             (vilpy-dotimes 1
                               (vilpy--slurp-backward)))))
                (t
                 (vilpy-dotimes arg
                   (vilpy--slurp-backward))))))
    (vilpy--reindent)))

(defun vilpy-down-slurp ()
  "Move current sexp or region into the next sexp."
  (interactive)
  (let ((bnd (vilpy--bounds-dwim))
        (leftp (vilpy--leftp))
        (regionp (region-active-p))
        (bolp (bolp))
        deactivate-mark)
    (when (vilpy-left-p)
      (forward-sexp))
    (let ((pt (save-excursion
                (when (vilpy-forward 1)
                  (vilpy-backward 1)
                  (point)))))
      (when pt
        (goto-char pt)
        (vilpy--teleport (car bnd) (cdr bnd) (not leftp) regionp)
        (save-excursion
          (backward-char 1)
          (when (vilpy-looking-back (concat vilpy-right " +"))
            (just-one-space))
          (when (and bolp (vilpy-looking-back "^ +"))
            (delete-region (match-beginning 0)
                           (match-end 0)))
          (indent-sexp))))))

(defun vilpy-up-slurp ()
  "Move current sexp or region into the previous sexp.
If the point is by itself on a line or followed only by right delimiters, slurp
the point into the previous list. This can be of thought as indenting the code
to the next level and adjusting the parentheses accordingly."
  (interactive)
  (let* ((empty-line-p (vilpy--empty-line-p))
         (list-start (when (eq empty-line-p 'right)
                       (save-excursion
                         (re-search-forward vilpy-right)
                         (vilpy-other)
                         (point))))
         (failp (when list-start
                  (= list-start
                     (save-excursion
                       (re-search-backward vilpy-left)
                       (point)))))
         (bnd (if empty-line-p
                  (cons (point) (point))
                (vilpy--bounds-dwim)))
         (regionp (region-active-p))
         (endp (or (vilpy-right-p)
                   (and (region-active-p) (= (point) (region-end)))))
         p-beg p-end
         (deactivate-mark nil)
         bsize)
    (deactivate-mark)
    (goto-char (car bnd))
    (if (or failp
            (not (vilpy-backward 1)))
        (progn
          (vilpy--complain "No list above to slurp into")
          (if regionp
              (vilpy--mark bnd)
            (goto-char
             (if endp
                 (cdr bnd)
               (car bnd)))))
      (setq p-beg (point))
      (forward-list)
      (setq p-end (point))
      (goto-char (car bnd))
      (setq bsize (buffer-size))
      (vilpy-save-excursion
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
        (vilpy--mark bnd))
      (if endp
          (goto-char (cdr bnd))
        (if (region-active-p)
            (vilpy-other)
          (goto-char (car bnd)))))))

(defun vilpy--backward-sexp-or-comment ()
  "When in comment, move to the comment start.
Otherwise, move to the previous sexp."
  (if (vilpy--in-comment-p)
      (goto-char (car (vilpy--bounds-comment)))
    (forward-sexp -1))
  (skip-chars-backward " \n"))

(defun vilpy--forward-sexp-or-comment ()
  "When before comment, move to the comment end.
Otherwise, move to the next sexp."
  (if (save-excursion
        (skip-chars-forward " \n")
        (vilpy--in-comment-p))
      (progn
        (skip-chars-forward " \n")
        (goto-char (cdr (vilpy--bounds-comment))))
    (forward-sexp 1)))

(defun vilpy-barf (arg)
  "Shrink current sexp or region by ARG sexps."
  (interactive "p")
  (cond ((region-active-p)
         (let* ((bnd (vilpy--bounds-dwim))
                (str (vilpy--string-dwim bnd))
                (one-symbolp (vilpy--symbolp str)))
           (if (= (point) (region-end))
               (cond (one-symbolp
                      (vilpy-dotimes arg
                        (if (re-search-backward "\\sw\\s_+" (region-beginning) t)
                            (forward-char 1)
                          (throw 'result i))))
                     ((vilpy--in-comment-p)
                      (goto-char (car (vilpy--bounds-comment)))
                      (if (= (point) (region-beginning))
                          (goto-char (cdr (vilpy--bounds-comment)))
                        (skip-chars-backward " \n")))
                     (t
                      (cl-incf arg)
                      (vilpy-dotimes arg
                        (vilpy--backward-sexp-or-comment))
                      (when (< (point) (car bnd))
                        (goto-char (car bnd)))
                      (vilpy--forward-sexp-or-comment)))
             (cond (one-symbolp
                    (vilpy-dotimes arg
                      (if (re-search-forward "\\s_+\\sw" (region-end) t)
                          (backward-char 1)
                        (throw 'result i))))
                   ((vilpy--in-comment-p)
                    (goto-char (cdr (vilpy--bounds-comment)))
                    (if (= (region-beginning) (region-end))
                        (goto-char (car bnd))
                      (skip-chars-forward " \n")))
                   (t
                    (save-restriction
                      (narrow-to-region (point-min)
                                        (region-end))
                      (cl-incf arg)
                      (vilpy-dotimes arg
                        (vilpy--forward-sexp-or-comment))
                      (if (vilpy--in-comment-p)
                          (goto-char (car (vilpy--bounds-comment)))
                        (forward-sexp -1))
                      (widen)))))))

        ((looking-at "()"))

        ((vilpy-right-p)
         (vilpy-dotimes arg
           (vilpy--barf-backward)))

        ((vilpy-left-p)
         (vilpy-dotimes arg
           (vilpy--barf-forward)))))

(defun vilpy-splice (arg)
  "Splice ARG sexps into containing list."
  (interactive "p")
  (vilpy-dotimes arg
    (let ((bnd (vilpy--bounds-dwim))
          (deactivate-mark nil))
      (cond ((region-active-p)
             (save-excursion
               (goto-char (cdr bnd))
               (re-search-backward vilpy-right)
               (delete-region (point) (cdr bnd)))
             (save-excursion
               (goto-char (car bnd))
               (re-search-forward vilpy-left)
               (delete-region (car bnd) (point))))
            ((vilpy-splice-let))

            ((vilpy-left-p)
             (save-excursion
               (goto-char (cdr bnd))
               (delete-char -1))
             (vilpy--delete-leading-garbage)
             (delete-char 1)
             (vilpy-forward 1)
             (vilpy-backward 1))

            ((vilpy-right-p)
             (setq bnd (vilpy--bounds-dwim))
             (delete-char -1)
             (goto-char (car bnd))
             (let ((pt (point)))
               (re-search-forward vilpy-left nil t)
               (delete-region pt (point)))
             (vilpy-backward 1)
             (forward-list))

            (t
             (setq bnd (vilpy--bounds-list))
             (save-excursion
               (goto-char (cdr bnd))
               (delete-char -1))
             (save-excursion
               (goto-char (car bnd))
               (delete-char 1)))))))

(defun vilpy-find (item tree)
  (cond ((null tree)
         nil)
        ((consp tree)
         (or (vilpy-find item (car tree))
             (vilpy-find item (cdr tree))))
        (t
         (eq item tree))))

(defun vilpy-splice-let ()
  "Join the current `let' into the parent `let'."
  (when (save-excursion
          (and (looking-at "(let")
               (vilpy--out-backward 1)
               (looking-at "(let")))
    (if (memq major-mode vilpy-clojure-modes)
        (vilpy-splice-let-clojure)
      (let ((child-binds (save-excursion
                           (vilpy-step-in 1)
                           (read (vilpy--string-dwim))))
            (parent-binds
             (mapcar (lambda (x) (if (consp x) (car x) x))
                     (save-excursion
                       (vilpy-up 1)
                       (read (vilpy--string-dwim)))))
            (end (save-excursion
                   (vilpy-step-in 2)
                   (point)))
            (beg (save-excursion
                   (vilpy-up 1)
                   (vilpy-other)
                   (1- (point)))))
        (save-excursion
          (forward-list)
          (delete-char -1))
        (delete-region beg end)
        (when parent-binds
          (newline-and-indent))
        (vilpy-step-out 2)
        (when (cl-find-if (lambda (v) (vilpy-find v child-binds))
                          parent-binds)
          (cond
            ((looking-at "(let\\*"))
            ((looking-at "(\\(let\\)")
             (replace-match "(let*")
             (vilpy--out-backward 1)
             (indent-sexp))
            (t
             (error "unexpected"))))
        (vilpy--prettify-1)
        (vilpy-step-in 2)
        (when parent-binds
          (vilpy-down (length parent-binds))))
      t)))

(defun vilpy-splice-let-clojure ()
  "Join the current Clojure `let' form into the parent `let'."
  (let ((end (save-excursion
               (vilpy-step-in 1)
               (1+ (point))))
        (beg (save-excursion
               (vilpy-up 1)
               (vilpy-other)
               (1- (point)))))
    (save-excursion
      (forward-list)
      (delete-char -1))
    (delete-region beg end)
    (insert "\n")
    (vilpy--out-backward 2)
    (vilpy--prettify-1)
    t))

(defun vilpy-raise (arg)
  "Use current sexp or region as replacement for its parent.
Do so ARG times."
  (interactive "p")
  (vilpy-dotimes arg
    (let ((regionp (region-active-p))
          (leftp (vilpy--leftp))
          (deactivate-mark nil)
          bnd1 bnd2)
      ;; re-indent first
      (vilpy-save-excursion (vilpy--out-forward 1))
      (unless leftp
        (vilpy-other))
      (setq bnd1 (vilpy--bounds-dwim))
      (deactivate-mark)
      (vilpy--out-forward 1)
      (setq bnd2 (vilpy--bounds-dwim))
      (delete-region (cdr bnd2) (cdr bnd1))
      (delete-region (car bnd2) (car bnd1))
      (if regionp
          (progn
            (indent-region (car bnd2) (point))
            (vilpy--mark (cons (car bnd2) (point))))
        (vilpy-from-left
         (indent-sexp)))
      (unless (eq leftp (vilpy--leftp))
        (vilpy-other)))))

(defun vilpy-raise-some ()
  "Use current sexps as replacement for their parent.
The outcome when ahead of sexps is different from when behind."
  (interactive)
  (let ((pt (point)))
    (cond ((region-active-p))

          ((vilpy-left-p)
           (if (null (vilpy--out-forward 1))
               (progn
                 (goto-char pt)
                 (vilpy--complain "Not enough depth to raise"))
             (backward-char 1)
             (set-mark (point))
             (goto-char pt)))

          ((vilpy-right-p)
           (if (null (vilpy--out-forward 1))
               (progn
                 (goto-char pt)
                 (vilpy--complain "Not enough depth to raise"))
             (backward-list)
             (forward-char 1)
             (set-mark (point))
             (goto-char pt)))

          (t
           (error "Unexpected")))
    (vilpy-raise 1)
    (deactivate-mark)))

(defun vilpy-convolute (arg)
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
               (vilpy--out-forward (1+ arg)))
             (save-excursion
               (vilpy--out-backward (1+ arg))))
        (let (beg end)
          (vilpy-from-left
           (setq beg (point))
           (setq end (vilpy--out-backward arg))
           (vilpy--out-backward 1)
           (vilpy--swap-regions (cons beg end)
                                (cons (point) (point)))
           (vilpy--reindent arg))
          (unless only-upper
            (vilpy-from-left
             (vilpy-other)
             (setq beg (point))
             (setq end (vilpy--out-forward arg))
             (vilpy--out-forward 1)
             (vilpy--swap-regions (cons beg end)
                                  (cons (point) (point)))
             (ignore-errors
               (vilpy-other))
             (vilpy--reindent (1+ arg)))))
      (error "Not enough depth to convolute"))))

(defvar vilpy-repeat--command nil
  "Command to use with `vilpy-repeat'.")

(defvar vilpy-repeat--prefix-arg nil
  "Prefix arg to use with `vilpy-repeat'.")

(defun vilpy-repeat ()
  "Repeat last command with last prefix arg."
  (interactive)
  (unless (memq last-command
                '(special-vilpy-repeat vilpy-repeat))
    (setq vilpy-repeat--command last-command)
    (setq vilpy-repeat--prefix-arg
          (or last-prefix-arg 1)))
  (setq current-prefix-arg vilpy-repeat--prefix-arg)
  (funcall vilpy-repeat--command))

(defun vilpy-join ()
  "Join sexps."
  (interactive)
  (let ((pt (point))
        bnd)
    (cond ((vilpy-right-p)
           (when (vilpy-forward 1)
             (backward-list)
             (delete-char 1)
             (goto-char pt)
             (backward-delete-char 1)
             (vilpy--out-forward 1)
             (vilpy--reindent 1)))
          ((vilpy-left-p)
           (when (vilpy-backward 1)
             (forward-list)
             (backward-delete-char 1)
             (goto-char (1- pt))
             (delete-char 1)
             (vilpy-save-excursion
               (forward-char 1)
               (vilpy-step-out 2)
               (vilpy--prettify-1))))
          ((and (setq bnd (vilpy--bounds-string))
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

(defun vilpy-split ()
  "Split sexps."
  (interactive)
  (let (bnd
        char-left
        char-right)
    (cond ((vilpy--in-comment-p)
           (indent-new-comment-line))
          ((and (setq bnd (vilpy--bounds-string))
                (not (= (point) (car bnd))))
           (insert "\"\"")
           (when (eolp)
             (delete-char 1))
           (backward-char)
           (newline-and-indent))
          ((vilpy-split-let-binding))
          (t
           (when (save-excursion
                   (prog1 (vilpy--out-forward 1)
                     (setq char-right (char-before))
                     (forward-list -1)
                     (setq char-left (char-after))))
             (insert (string char-right char-left))
             (backward-char 2)
             (vilpy-right 1))
           (newline-and-indent)
           (when (vilpy-left-p)
             (indent-sexp))))))

(defun vilpy-split-let-binding ()
  (when (and
         (or (vilpy-left-p)
             (vilpy-right-p))
         (save-excursion
           (vilpy--out-backward 2)
           (looking-at "(let")))
    (save-excursion
      (vilpy--out-forward 2)
      (insert ")"))
    (save-excursion
      (insert ")\n(let (")
      (vilpy--out-backward 3)
      (vilpy--prettify-1))
    (vilpy-step-in 1)
    (vilpy-down 1)
    t))

;;* Locals: more transformations
(defun vilpy-move-up (arg)
  "Move current expression up ARG times.  Don't exit parent list.
Also works from inside the list."
  (interactive "p")
  (if (or (vilpy-left-p)
          (vilpy-right-p)
          (region-active-p)
          (looking-at vilpy-outline))
      (vilpy--move-up-special arg)
    (let ((offset (-
                   (point)
                   (progn
                     (vilpy--out-backward 1)
                     (point)))))
      (vilpy--move-up-special arg)
      (forward-char offset))))

(defun vilpy-move-down (arg)
  "Move current expression down ARG times.  Don't exit parent list.
Also works from inside the list."
  (interactive "p")
  (if (or (vilpy-left-p)
          (vilpy-right-p)
          (region-active-p)
          (looking-at vilpy-outline))
      (vilpy--move-down-special arg)
    (let ((offset (-
                   (point)
                   (progn
                     (vilpy--out-backward 1)
                     (point)))))
      (vilpy--move-down-special arg)
      (forward-char offset))))

(defun vilpy--move-up-region (arg)
  "Swap the marked region ARG positions up.
Precondition: the region is active and the point is at `region-beginning'."
  (cond
    ((and (looking-at "\\_<")
          (save-excursion
            (goto-char (region-end))
            (looking-at "-"))))
    ((vilpy-after-string-p "-")
     (let ((bnd1 (vilpy--bounds-dwim))
           bnd2)
       (vilpy-up arg)
       (setq bnd2 (vilpy--bounds-dwim))
       (vilpy--swap-regions bnd1 bnd2)
       (setq deactivate-mark nil)
       (set-mark (point))
       (forward-char (- (cdr bnd1) (car bnd1)))))
    ((= arg 1)
     (let ((bnd1 (vilpy--bounds-dwim))
           (bnd0 (save-excursion
                   (deactivate-mark)
                   (if (ignore-errors (up-list) t)
                       (vilpy--bounds-dwim)
                     (cons (point-min) (point-max)))))
           bnd2)
       (goto-char (car bnd1))
       (if (re-search-backward "[^ \t\n`'#({[]" (car bnd0) t)
           (progn
             (deactivate-mark)
             (if (vilpy--in-comment-p)
                 (setq bnd2 (vilpy--bounds-comment))
               (when (eq (char-after) ?\")
                 (forward-char)
                 (backward-sexp))
               (when (memq (char-after) '(?\) ?\] ?\}))
                 (forward-char))
               (setq bnd2 (vilpy--bounds-dwim)))
             (vilpy--swap-regions bnd1 bnd2)
             (setq deactivate-mark nil)
             (goto-char (car bnd2))
             (set-mark (point))
             (forward-char (- (cdr bnd1) (car bnd1))))
         (setq deactivate-mark nil)
         (vilpy--mark bnd1)))
     (exchange-point-and-mark))
    (t
     (let ((bnd1 (vilpy--bounds-dwim)))
       (vilpy-up arg)
       (vilpy--mark
        (car
         (vilpy--swap-regions
          bnd1 (vilpy--bounds-dwim)))))
     (exchange-point-and-mark))))

(defun vilpy--move-up-special (arg)
  "Move current expression up ARG times.  Don't exit parent list."
  (let ((at-start (vilpy--leftp)))
    (unless (or at-start (looking-at vilpy-outline))
      (vilpy-other))
    (cond ((region-active-p)
           (vilpy--move-up-region arg))
          (t
           (vilpy--mark (vilpy--bounds-dwim))
           (vilpy-move-up arg)
           (deactivate-mark)
           (vilpy-other)))
    (unless at-start (vilpy-other))))

(defun vilpy--move-down-region (arg)
  "Swap the marked region ARG positions down.
Precondition: the region is active and the point is at `region-beginning'."
  (cond
    ((and (vilpy-after-string-p "-")
          (save-excursion
            (goto-char (region-end))
            (looking-at "\\_>"))))
    ((save-excursion
       (goto-char (region-end))
       (looking-at "-"))
     (let ((bnd1 (vilpy--bounds-dwim))
           bnd2)
       (vilpy-down arg)
       (setq bnd2 (vilpy--bounds-dwim))
       (vilpy--swap-regions bnd1 bnd2)
       (goto-char (cdr bnd2))
       (setq deactivate-mark nil)
       (set-mark (point))
       (forward-char (- (car bnd1) (cdr bnd1)))))
    ((= arg 1)
     (let ((bnd1 (vilpy--bounds-dwim))
           (bnd0 (save-excursion
                   (deactivate-mark)
                   (if (ignore-errors (up-list) t)
                       (vilpy--bounds-dwim)
                     (cons (point-min) (point-max)))))
           bnd2)
       (goto-char (cdr bnd1))
       (if (re-search-forward "[^ \t\n]" (max (1- (cdr bnd0))
                                              (point)) t)
           (progn
             (deactivate-mark)
             (if (vilpy--in-comment-p)
                 (setq bnd2 (vilpy--bounds-comment))
               (when (memq (char-before) '(?\( ?\" ?\[ ?\{))
                 (backward-char))
               (setq bnd2 (vilpy--bounds-dwim)))
             (vilpy--swap-regions bnd1 bnd2)
             (setq deactivate-mark nil)
             (goto-char (cdr bnd2))
             (set-mark (point))
             (backward-char (- (cdr bnd1) (car bnd1))))
         (vilpy--mark bnd1)
         (exchange-point-and-mark))))
    (t
     (let ((bnd1 (vilpy--bounds-dwim)))
       (vilpy-down arg)
       (vilpy--mark
        (cdr
         (vilpy--swap-regions
          bnd1 (vilpy--bounds-dwim))))
       (vilpy-other)))))

(defun vilpy--move-down-special (arg)
  "Move current expression down ARG times.  Don't exit parent list."
  (let ((at-start (vilpy--leftp)))
    (unless (or at-start (looking-at vilpy-outline))
      (vilpy-other))
    (cond ((region-active-p)
           (vilpy--move-down-region arg))
          ((looking-at vilpy-outline)
           (vilpy-dotimes arg
             (let ((bnd1 (vilpy--bounds-outline))
                   bnd2)
               (goto-char (1+ (cdr bnd1)))
               (if (and (setq bnd2 (vilpy--bounds-outline))
                        (not (equal bnd1 bnd2)))
                   (progn
                     (vilpy--swap-regions bnd1 bnd2)
                     (forward-char (1+ (- (cdr bnd2) (car bnd2)))))
                 (goto-char (car bnd1))))))
          (t
           (vilpy--mark (vilpy--bounds-dwim))
           (vilpy-move-down arg)
           (deactivate-mark)
           (vilpy-other)))
    (unless at-start (vilpy-other))))

(defun vilpy-move-left (arg)
  "Move region left ARG times."
  (interactive "p")
  (vilpy-dotimes arg
    (when (save-excursion (ignore-errors (up-list) t))
      (let* ((regionp (region-active-p))
             (leftp (vilpy--leftp))
             (bnd (vilpy--bounds-dwim))
             (str (vilpy--string-dwim bnd))
             pt)
        (delete-region (car bnd) (cdr bnd))
        (cond ((looking-at " *;"))
              ((and (looking-at "\n")
                    (vilpy-bolp))
               (delete-region
                (line-beginning-position)
                (1+ (point))))
              ((looking-at "\\([\n ]+\\)[^\n ;]")
               (delete-region (match-beginning 1)
                              (match-end 1))))
        (deactivate-mark)
        (vilpy--out-backward 1)
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
            (vilpy-other)))))))

(defun vilpy-move-right (arg)
  "Move region right ARG times."
  (interactive "p")
  (vilpy-dotimes arg
    (when (save-excursion (ignore-errors (up-list) t))
      (let* ((regionp (region-active-p))
             (leftp (vilpy--leftp))
             (bnd (vilpy--bounds-dwim))
             (str (vilpy--string-dwim bnd))
             pt)
        (delete-region (car bnd) (cdr bnd))
        (cond ((looking-at " *;"))
              ((and (looking-at "\n")
                    (vilpy-bolp))
               (delete-region
                (line-beginning-position)
                (1+ (point))))
              ((looking-at "\\([\n ]+\\)[^\n ;]")
               (delete-region (match-beginning 1)
                              (match-end 1))))
        (vilpy--out-backward 1)
        (deactivate-mark)
        (vilpy-other)
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
            (vilpy-other)))))))

(defun vilpy-clone (arg)
  "Clone sexp ARG times.
When the sexp is top level, insert an additional newline."
  (interactive "p")
  (let* ((bnd (vilpy--bounds-dwim))
         (str (vilpy--string-dwim bnd))
         (pt (point)))
    (cond ((region-active-p)
           (vilpy-dotimes arg
             (cl-labels
                 ((doit ()
                    (let (deactivate-mark)
                      (save-excursion
                        (newline)
                        (insert str)
                        (vilpy--indent-for-tab)))))
               (if (= (point) (region-end))
                   (doit)
                 (exchange-point-and-mark)
                 (doit)
                 (exchange-point-and-mark)))))
          ((vilpy-left-p)
           (goto-char (car bnd))
           (cond ((and (bolp) (looking-at "(defun"))
                  (vilpy-dotimes arg
                    (insert str)
                    (newline)
                    (newline))
                  (goto-char pt))
                 ((and (bolp)
                       (save-excursion
                         (goto-char (cdr bnd))
                         (looking-at "\n;; =>")))
                  (vilpy-dotimes arg
                    (insert str)
                    (newline-and-indent)
                    (vilpy-move-down 1)))
                 (t
                  (vilpy-dotimes arg
                    (insert str)
                    (newline-and-indent))
                  (goto-char pt))))
          ((vilpy-right-p)
           (if (save-excursion
                 (backward-list)
                 (and (bolp) (looking-at "(defun")))
               (vilpy-dotimes arg
                 (newline)
                 (newline-and-indent)
                 (insert str))
             (vilpy-dotimes arg
               (newline-and-indent)
               (insert str))))
          (t
           (error "Unexpected")))))

(defvar vilpy--oneline-comments nil
  "Collect comments for `vilpy--oneline'.")

(defun vilpy-mapcan-tree (func expr)
  "Reduce with FUNC all lists in EXPR."
  (cond ((null expr)
         nil)
        ((and (vectorp expr) (> (length expr) 0))
         (apply #'vector
                (funcall func
                         (vilpy-mapcan-tree func (aref expr 0))
                         (vilpy-mapcan-tree
                          func
                          (cdr
                           (mapcar #'identity expr))))))
        ((listp expr)
         (funcall func
                  (vilpy-mapcan-tree func (car expr))
                  (vilpy-mapcan-tree func (cdr expr))))
        (t
         expr)))

(defun vilpy--oneline (expr &optional ignore-comments)
  "Remove newlines from EXPR.
When IGNORE-COMMENTS is not nil, don't remove comments.
Instead keep them, with a newline after each comment."
  (vilpy-mapcan-tree
   (lambda (x y)
     (cond ((equal x '(ly-raw newline))
            y)
           ((vilpy--raw-comment-p x)
            (if (null ignore-comments)
                (progn
                  (push x vilpy--oneline-comments)
                  y)
              (if (equal (car y) '(ly-raw newline))
                  (cons x y)
                `(,x (ly-raw newline) ,@y))))
           ((and (vilpy--raw-string-p x)
                 (null ignore-comments))
            (cons `(ly-raw string ,(replace-regexp-in-string "\n" "\\\\n" (cl-caddr x)))
                  y))
           (t
            (cons x y))))
   expr))

(defun vilpy-oneline ()
  "Squeeze current sexp into one line.
Comments will be moved ahead of sexp."
  (interactive)
  (cond ((vilpy--in-comment-p)
         (let* ((bnd (vilpy--bounds-comment))
                (str (vilpy--string-dwim bnd)))
           (delete-region (car bnd) (cdr bnd))
           (insert (vilpy-comment-char 2 " ")
                   (mapconcat #'identity
                              (split-string str "[ \n]*;;[ \n]*" t)
                              " "))
           (beginning-of-line)
           (back-to-indentation)))
        ((and (region-active-p)
              (= (char-after (region-beginning)) ?\")
              (= (char-before (region-end)) ?\"))
         (vilpy-string-oneline))
        (t
         (unless (or (vilpy-left-p)
                     (vilpy-right-p))
           (vilpy--out-backward 1))
         (let* ((bnd (vilpy--bounds-dwim))
                (str (vilpy--string-dwim bnd))
                (from-left (vilpy-left-p))
                expr)
           (delete-region (car bnd) (cdr bnd))
           (when (region-active-p)
             (deactivate-mark))
           (setq vilpy--oneline-comments nil)
           (if (setq expr (ignore-errors
                            (vilpy--oneline
                             (vilpy--read str))))
               (progn
                 (mapc (lambda (x)
                         (vilpy--insert x)
                         (newline))
                       vilpy--oneline-comments)
                 (vilpy--insert expr))
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

(defun vilpy-multiline (&optional arg)
  "Spread current sexp over multiple lines.
When ARG is `fill', do nothing for short expressions."
  (interactive "p")
  (unless (or (vilpy-left-p)
              (vilpy-right-p))
    (vilpy--out-backward 1))
  (vilpy-from-left
   (let* ((bnd (vilpy--bounds-list))
          (str (vilpy--string-dwim bnd))
          (plain-expr (read str))
          (expr (vilpy--read str))
          res)
     (unless (and (eq arg 'fill)
                  (< (length str) 80))
       (unless (listp plain-expr)
         (setq plain-expr nil))
       (if (or (cl-some #'listp plain-expr)
               (member '(ly-raw newline) expr))
           (let ((pt (point)))
             (vilpy-forward 1)
             (while (and (vilpy-step-in 1) (> (point) pt))
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
                           (vilpy--read str))))
         (when (vectorp expr)
           (setq res (apply #'vector res)))
         (vilpy--insert res))))))

(defvar-local vilpy--multiline-take-3
    '(defvar defun defmacro defcustom defgroup defvar-local declare-function
      define-key nth throw define-error defadvice defhydra defsubst)
  "List of constructs for which the first 3 elements are on the first line.")

(dolist (mode vilpy-clojure-modes)
  (setq-mode-local
   mode
   vilpy--multiline-take-3 '()))

(defvar vilpy--multiline-take-3-arg
  '(defun defmacro declare-function define-error defadvice defhydra defsubst)
  "List of constructs for which the first 3 elements are on the first line.
The third one is assumed to be the arglist and will not be changed.")

(defvar-local vilpy--multiline-take-2
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
      vilpy-dotimes cond case cl-case
      defalias 1+ 1- dotimes dolist boundp fboundp macrop
      null consp oddp zerop plusp minusp kbd
      not pop listp or and)
  "List of constructs for which the first 2 elements are on the first line.")

(dolist (mode vilpy-clojure-modes)
  (setq-mode-local
   mode
   vilpy--multiline-take-2 '(loop recur for fn def defn ns if -> ->>
                                  + +' - -' * *' / > >= < <= = ==
                                  or and not
                                  assoc! assoc assoc-in concat)))

(defvar vilpy--multiline-take-2-arg '(declare lambda
                                      make-variable-buffer-local
                                      bound-and-true-p
                                      called-interactively-p
                                      vilpy-dotimes dotimes)
  "List of constructs for which the first 2 elements are on the first line.
The second one will not be changed.")

(defun vilpy-interleave (x lst &optional step)
  "Insert X in between each element of LST.
Don't insert X when it's already there.
When STEP is non-nil, insert in between each STEP elements instead."
  (setq step (or step 1))
  (let ((res (nreverse (vilpy-multipop lst step)))
        item)
    (while lst
      (unless (equal (car res) x)
        (push x res))
      (unless (equal (car res)
                     (car (setq item (vilpy-multipop lst step))))
        (setq res (nconc (nreverse item) res))))
    (nreverse res)))

(defcustom vilpy-multiline-threshold 32
  "Don't multiline expresssions shorter than this when printed as a string."
  :type 'integer)

(defun vilpy--translate-newlines (str)
  "Replace quoted newlines with real ones in STR."
  (with-temp-buffer
    (insert str)
    (goto-char (point-min))
    (while (re-search-forward "\\\\n" nil t)
      (unless (= ?\\
                 (char-before (- (point) 2)))
        (replace-match "\n" nil t)))
    (buffer-string)))

(defun vilpy--multiline-1 (expr &optional quoted)
  "Transform a one-line EXPR into a multi-line.
When QUOTED is not nil, assume that EXPR is quoted and ignore some rules."
  (cond ((vectorp expr)
         (apply #'vector
                (vilpy--multiline-1
                 (mapcar #'identity expr))))
        ((not (listp expr))
         expr)
        ((and vilpy-multiline-threshold
              (< (length (vilpy--prin1-to-string
                          expr 0 'emacs-lisp-mode))
                 vilpy-multiline-threshold))
         expr)
        ((and (eq (car-safe expr) 'ly-raw)
              (memq (cadr expr) '(clojure-map clojure-set)))
         (list 'ly-raw (cadr expr)
               (vilpy-interleave '(ly-raw newline)
                                 (mapcar #'vilpy--multiline-1 (cl-caddr expr))
                                 2)))
        ((and (eq (car-safe expr) 'ly-raw)
              (eq (nth 1 expr) 'splice))
         (list 'ly-raw (nth 1 expr) (nth 2 expr) (nth 3 expr)
               (vilpy-interleave '(ly-raw newline)
                                 (mapcar #'vilpy--multiline-1 (car (nthcdr 4 expr)))
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
                                  ,(vilpy--translate-newlines
                                    (cadr expr)))))
                  (t (unless (= (length expr) 2)
                       (error "Unexpected expr: %S" expr))
                     (unless (null res)
                       (error "Stray ly-raw in %S" expr))
                     (setq res (list 'ly-raw (car expr)
                                     (vilpy--multiline-1
                                      (cadr expr)
                                      (car (memq (car expr) '(quote \` clojure-lambda))))))))
                (setq expr nil))
               ((vectorp elt)
                (push
                 (apply #'vector
                        (vilpy--multiline-1
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
               ((and (not quoted) (memq elt vilpy--multiline-take-3))
                (push elt res)
                ;; name
                (when expr
                  (push (pop expr) res))
                ;; value
                (when expr
                  (if (memq elt vilpy--multiline-take-3-arg)
                      (push (pop expr) res)
                    (push (car (vilpy--multiline-1 (list (pop expr)))) res)))
                (push '(ly-raw newline) res))
               ((and (not quoted) (memq elt vilpy--multiline-take-2))
                (push elt res)
                (when (memq elt vilpy--multiline-take-2-arg)
                  (push (pop expr) res)
                  (push '(ly-raw newline) res)))
               ((and (memq elt '(let let*))
                     expr
                     (or (memq major-mode vilpy-clojure-modes)
                         (and
                          (listp (car expr))
                          (listp (cdar expr)))))
                (push elt res)
                (let ((body (pop expr)))
                  (push
                   (if (memq major-mode vilpy-clojure-modes)
                       (apply #'vector
                              (vilpy-interleave '(ly-raw newline)
                                                (mapcar #'vilpy--multiline-1 body) 2))
                     (vilpy-interleave
                      '(ly-raw newline)
                      (mapcar
                       (lambda (x)
                         (if (and (listp x)
                                  (not (eq (car x) 'ly-raw)))
                             (cons (car x)
                                   (vilpy--multiline-1 (cdr x)))
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
                (setq elt (vilpy--multiline-1 elt))
                (if (equal elt '(ly-raw newline))
                    (unless (equal elt (car res))
                      (push elt res))
                  (push elt res)
                  (push '(ly-raw newline) res)))))
           (cond ((equal (car res) 'ly-raw)
                  res)
                 ((equal (car res) '(ly-raw newline))
                  (if (and (cdr res)
                           (vilpy--raw-comment-p (cadr res)))
                      (nreverse res)
                    (nreverse (cdr res))))
                 (t
                  (nreverse res)))))))

(defun vilpy-alt-multiline (&optional silent)
  "Spread current sexp over multiple lines.
When SILENT is non-nil, don't issue messages."
  (interactive)
  (unless (or (vilpy-left-p)
              (vilpy-right-p))
    (vilpy--out-backward 1))
  (let* ((bnd (vilpy--bounds-dwim))
         (str (vilpy--string-dwim bnd))
         (expr (vilpy--read str))
         (expr-o (vilpy--oneline expr t))
         (expr-m (vilpy--multiline-1 expr-o))
         (leftp (vilpy--leftp))
         (print-circle nil))
    (cond ((equal expr expr-m)
           (unless silent
             (message "No change")))
          ((and (memq major-mode vilpy-elisp-modes)
                (not
                 (condition-case nil
                     (equal (read str)
                            (read (vilpy--prin1-to-string
                                   expr-m 0 major-mode)))
                   (error
                    (vilpy--complain "Got an unreadable expr (probably overlay)")
                    t))))
           (error "Got a bad transform: %S" expr-m))
          (t
           (delete-region (car bnd) (cdr bnd))
           (vilpy--insert expr-m)
           (when leftp
             (backward-list))))))

(defvar vilpy-do-fill nil
  "If t, `vilpy-insert-1' will try to fill.")

(defcustom vilpy-move-after-commenting t
  "When non-nil, adjust point to next sexp after commenting out a
  sexp. If at last sexp in list, move out and backwards to
  enclosing sexp."
  :type 'boolean
  :group 'vilpy)

(defcustom vilpy-comment-use-single-semicolon nil
  "When non-nil, prefer single semicolons for comments at the
  right of the source code (after vilpy-right or at eol)."
  :type 'boolean
  :group 'vilpy)

(defun vilpy-comment (&optional arg)
  "Comment ARG sexps."
  (interactive "p")
  (setq arg (or arg 1))
  (if (and (> arg 1) (vilpy--in-comment-p))
      (let ((bnd (vilpy--bounds-comment)))
        (uncomment-region (car bnd) (cdr bnd)))
    (vilpy-dotimes arg
      (let (bnd)
        (cond ((region-active-p)
               (comment-dwim nil)
               (when (vilpy--in-string-or-comment-p)
                 (vilpy--out-backward 1)))
              ((vilpy--in-string-or-comment-p)
               (cond ((and (eq major-mode 'emacs-lisp-mode)
                           (vilpy-after-string-p ";;; "))
                      (delete-char -1)
                      (insert "###autoload")
                      (forward-char 1))
                     ((vilpy-after-string-p (vilpy-comment-char 2 " "))
                      (backward-char 1)
                      (insert (vilpy-comment-char))
                      (forward-char 1))
                     ((and vilpy-comment-use-single-semicolon
                           (vilpy-after-string-p (vilpy-comment-char 1 " ")))
                      (delete-region
                       (point)
                       (progn
                         (skip-chars-backward (vilpy-comment-char 1 " \n"))
                         (point)))
                      (insert (concat " " (vilpy-comment-char 2 " "))))
                     (t
                      (self-insert-command 1))))
              ((memq (char-before) '(?\\ ?\#))
               (self-insert-command 1))
              ((vilpy-left-p)
               (setq bnd (vilpy--bounds-dwim))
               (when vilpy-move-after-commenting
                 (vilpy-down 1))
               (comment-region (car bnd) (cdr bnd))
               (when vilpy-move-after-commenting
                 (when (or (vilpy--in-string-or-comment-p)
                           (looking-at (vilpy-comment-char)))
                   (vilpy--out-backward 1))))
              ((vilpy-right-p)
               (if vilpy-comment-use-single-semicolon
                   (progn
                     (unless (eolp)
                       (newline-and-indent)
                       (skip-chars-backward "\n\t "))
                     (comment-dwim nil)
                     (just-one-space))
                 (progn
                   (newline-and-indent)
                   (insert (vilpy-comment-char 2 " "))
                   (unless (eolp)
                     (newline)
                     (vilpy--reindent 1)
                     (skip-chars-backward "\n\t ")
                     (forward-char 1)))))
              ((eolp)
               (comment-dwim nil)
               (when vilpy-comment-use-single-semicolon
                 (just-one-space)))
              ((looking-at " *[])}]")
               (if vilpy-comment-use-single-semicolon
                   (if (vilpy-bolp)
                       (insert (vilpy-comment-char 2 "\n"))
                     (insert (vilpy-comment-char 1 "\n")))
                 (progn
                   (unless (vilpy-bolp)
                     (insert "\n"))
                   (insert (vilpy-comment-char 2 "\n"))))
               (when (vilpy--out-forward 1)
                 (vilpy--prettify-1))
               (move-end-of-line 0)
               (insert " "))
              ((vilpy-bolp)
               (let ((bnd (vilpy--bounds-list)))
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
                           (and (vilpy--out-forward 1)
                                (point))))
               (let ((pt (point)))
                 (if (re-search-forward "\n" bnd t)
                     (if (= (count-matches vilpy-left pt (point))
                            (count-matches vilpy-right pt (point)))
                         (progn (comment-region pt (point))
                                (vilpy-forward 1)
                                (vilpy-backward 1))
                       (goto-char pt)
                       (re-search-forward vilpy-left bnd t)
                       (backward-char 1)
                       (forward-list 1)
                       (comment-region pt (point))
                       (vilpy-forward 1)
                       (vilpy-backward 1))
                   (comment-region (point) (1- bnd))
                   (vilpy--out-backward 1))))
              (t
               (self-insert-command 1)))))))

(defun vilpy--quote-string (str &optional quote-newlines)
  "Quote the quotes and backslashes in STR.
Quote the newlines if QUOTE-NEWLINES is t."
  (setq str (replace-regexp-in-string "\\\\" "\\\\\\\\" str))
  (setq str (replace-regexp-in-string "\"" "\\\\\"" str))
  (if quote-newlines
      (replace-regexp-in-string "\n" "\\\\n" str)
    str))

(defun vilpy-stringify (&optional arg)
  "Transform current sexp into a string.
Quote newlines if ARG isn't 1."
  (interactive "p")
  (setq arg (or arg 1))
  (let* ((bnd (vilpy--bounds-dwim))
         (pt (point))
         (str-1 (buffer-substring-no-properties (car bnd) pt))
         (str-2 (buffer-substring-no-properties pt (cdr bnd)))
         (regionp (region-active-p))
         (leftp (vilpy--leftp))
         deactivate-mark)
    (when (and regionp leftp)
      (exchange-point-and-mark))
    (if (vilpy--in-string-p)
        (if regionp
            (progn
              (insert "\\\"")
              (exchange-point-and-mark)
              (insert "\\\"")
              (backward-char 2)
              (unless leftp
                (exchange-point-and-mark)))
          (vilpy--complain "can't do anything useful here"))
      (deactivate-mark)
      (setq str-1 (vilpy--quote-string str-1 (/= arg 1)))
      (setq str-2 (vilpy--quote-string str-2 (/= arg 1)))
      (delete-region (car bnd) (cdr bnd))
      (insert "\"" str-1)
      (save-excursion (insert str-2 "\""))
      (when regionp
        (unless (looking-at "\"")
          (backward-char 1))
        (vilpy-mark-symbol)
        (if (and leftp (= (point) (region-end)))
            (exchange-point-and-mark))))))

(defun vilpy-unstringify ()
  "Unquote string at point."
  (interactive)
  (if (region-active-p)
      (if (vilpy--string-markedp)
          (let (deactivate-mark
                (str (vilpy--string-dwim))
                (leftp (vilpy--leftp)))
            (delete-active-region)
            (set-mark (point))
            (insert (read str))
            (when leftp
              (vilpy-other)))
        (vilpy--complain "the current region isn't a string"))
    (let* ((bnd (vilpy--bounds-string))
           (str (vilpy--string-dwim bnd))
           (str-1 (concat (substring str 0 (- (point) (car bnd))) "\""))
           (offset (length (read str-1))))
      (delete-region (car bnd) (cdr bnd))
      (save-excursion (insert (read str)))
      (forward-char offset))))

(defvar vilpy-teleport-global nil
  "When non-nil, `vilpy-teleport' will consider all open parens in window.
Otherwise, only parens within the current defun are considered.
When you press \"t\" in `vilpy-teleport', this will be bound to t temporarily.")

(defmacro vilpy-quit-and-run (&rest body)
  "Quit the minibuffer and run BODY afterwards."
  `(progn
     (put 'quit 'error-message "")
     (run-at-time nil nil
                  (lambda ()
                    (put 'quit 'error-message "Quit")
                    ,@body))
     (minibuffer-keyboard-quit)))

(defun vilpy-teleport (arg)
  "Move ARG sexps into a sexp determined by `vilpy-ace-paren'."
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
          ((vilpy-left-p)
           (save-excursion
             (unless (vilpy-dotimes arg
                       (forward-list 1))
               (error "Unexpected"))
             (setq end (point))))
          ((vilpy-right-p)
           (save-excursion
             (setq endp t)
             (unless (vilpy-dotimes arg
                       (backward-list arg))
               (error "Unexpected"))
             (setq end (point))))
          (t
           (error "Unexpected")))
    (let* ((vilpy-avy-keys (delete ?t vilpy-avy-keys))
           (avy-handler-function
            (lambda (x)
              (if (eq x ?t)
                  (progn
                    (avy--done)
                    (vilpy-quit-and-run
                     (let ((vilpy-teleport-global t))
                       (when regionp
                         (activate-mark))
                       (vilpy-teleport arg))))
                (avy-handler-default x))))
           (res (vilpy-ace-paren
                 (when vilpy-teleport-global
                   2))))
      (cond ((memq res '(t nil))
             (when regionp
               (vilpy--mark (cons end beg))))
            (t
             (forward-char 1)
             (unless (looking-at "(")
               (ignore-errors
                 (forward-sexp)))
             (backward-char 1)
             (vilpy--teleport beg end endp regionp))))))

;;* Locals: dialect-related
(defcustom vilpy-eval-display-style 'message
  "Choose a function to display the eval result."
  :type '(choice
          (const :tag "message" message)
          (const :tag "overlay" overlay)))

(declare-function cider--display-interactive-eval-result "ext:cider-overlays")
(declare-function eros--eval-overlay "ext:eros")

(define-error 'eval-error "Eval error")

(defvar vilpy-message-limit 4000
  "String length limit for `vilpy-message' to pop up a window.
For smaller strings `message' is used.")

(defun vilpy-message (str &optional popup)
  "Display STR in the echo area.
If STR is too large, pop it to a buffer instead."
  (if (or
       popup
       (> (length str) vilpy-message-limit)
       (> (cl-count ?\n str)
          (or
           14
           (* (window-height (frame-root-window)) max-mini-window-height))))
      (with-current-buffer (pop-to-buffer "*vilpy-message*")
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

(defvar vilpy--pams (make-hash-table))

(defun vilpy-pam-store (sym)
  "Store point and mark to SYM."
  (if (region-active-p)
      (progn
        (puthash sym (cons (point) (mark)) vilpy--pams)
        (deactivate-mark))
    (puthash sym (point) vilpy--pams)))

(defun vilpy-pam-restore (sym)
  "Restore point and mark from FROM."
  (let ((val (gethash sym vilpy--pams)))
    (cond ((consp val)
           (goto-char (car val))
           (set-mark (cdr val)))
          ((numberp val)
           (goto-char val)))))

(defun vilpy-beginning-of-defun (&optional arg)
  "Forward to `beginning-of-defun' with ARG.  Deactivate region.
When called twice in a row, restore point and mark."
  (interactive "p")
  (cond ((and (called-interactively-p 'any)
              (looking-at "^(")
              (let* ((vilpy-bof-last-point (gethash 'vilpy-bof-last-point vilpy--pams))
                     (pt (if (consp vilpy-bof-last-point)
                             (car vilpy-bof-last-point)
                           vilpy-bof-last-point)))
                (and
                 (> pt (point))
                 (<= pt (save-excursion (forward-list) (point))))))
         (vilpy-pam-restore 'vilpy-bof-last-point))
        ((looking-at "^("))
        (t
         (vilpy-pam-store 'vilpy-bof-last-point)
         (beginning-of-defun arg))))

;;* Locals: avy-jump
(declare-function avy--regex-candidates "avy")
(declare-function avy-process "avy")
(declare-function avy--overlay-post "avy")

(defun vilpy-ace-char ()
  "Visually select a char within the current defun."
  (interactive)
  (let ((avy-keys vilpy-avy-keys))
    (avy-with vilpy-ace-char
      (vilpy--avy-do
       (string (read-char "Char: "))
       (save-excursion
         ;; `beginning-of-defun' won't work, since it can change sexp
         (vilpy--out-backward 50)
         (vilpy--bounds-dwim))
       (lambda () t)
       vilpy-avy-style-char))))

(defun vilpy-ace-paren (&optional arg)
  "Jump to an open paren within the current defun.
ARG can extend the bounds beyond the current defun."
  (interactive "p")
  (setq arg (or arg 1))
  (vilpy--remember)
  (deactivate-mark)
  (let ((avy-keys vilpy-avy-keys)
        (bnd (if (eq arg 1)
                 (save-excursion
                   (vilpy--out-backward 50)
                   (vilpy--bounds-dwim))
               (cons (window-start)
                     (window-end nil t)))))
    (avy-with vilpy-ace-paren
      (vilpy--avy-do
       vilpy-left
       bnd
       (lambda () (not (vilpy--in-string-or-comment-p)))
       vilpy-avy-style-paren))))

(defun vilpy-ace-symbol (arg)
  "Jump to a symbol within the current sexp and mark it.
Sexp is obtained by exiting the list ARG times."
  (interactive "p")
  (vilpy--out-forward
   (if (region-active-p)
       (progn (deactivate-mark) arg)
     (1- arg)))
  (let ((avy-keys vilpy-avy-keys)
        res)
    (avy-with vilpy-ace-symbol
      (let ((avy--overlay-offset (if (eq vilpy-avy-style-symbol 'at) -1 0)))
        (setq res (vilpy--avy-do
                   "[([{ ]\\(?:\\sw\\|\\s_\\|[\"'`#~,@]\\)"
                   (vilpy--bounds-dwim)
                   (lambda ()
                     (not (save-excursion
                            (forward-char -1)
                            (vilpy--in-string-or-comment-p))))
                   vilpy-avy-style-symbol))))
    (unless (memq res '(t nil))
      (unless (or (eq (char-after) ?\")
                  (looking-at ". "))
        (forward-char 1))
      (vilpy-mark-symbol))))

(defun vilpy-ace-symbol-beginning-of-defun ()
  (interactive)
  (vilpy-ace-symbol 99))

(defun vilpy-ace-subword (arg)
  "Mark sub-word within a sexp.
Sexp is obtained by exiting list ARG times."
  (interactive "p")
  (if (and (region-active-p)
           (string-match "\\`\\(\\sw+\\)\\s_"
                         (vilpy--string-dwim)))
      (vilpy--mark (cons (region-beginning)
                         (+ (region-beginning) (match-end 1))))
    (vilpy--out-forward
     (if (region-active-p)
         (progn (deactivate-mark) arg)
       (1- arg)))
    (let* ((avy-keys vilpy-avy-keys)
           (res (avy-with 'vilpy-ace-subword
                  (vilpy--avy-do
                   "[([{ -/]\\(?:\\sw\\|\\s_\\|\\s(\\|[\"'`#]\\)"
                   (vilpy--bounds-dwim)
                   (lambda () (or (not (vilpy--in-string-or-comment-p))
                                  (vilpy-looking-back ".\"")))
                   vilpy-avy-style-symbol))))
      (unless (memq res '(t nil))
        (skip-chars-forward "-([{ `'#")
        (mark-word)))))

(defun vilpy--avy-do (regex bnd filter style &optional group)
  "Visually select a match to REGEX within BND.
Filter out the matches that don't match FILTER.
Use STYLE function to update the overlays."
  (vilpy--recenter-bounds bnd)
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

(defun vilpy-tab ()
  "Indent code.
When region is active, call `vilpy-mark-car'."
  (interactive)
  (if (region-active-p)
      (vilpy-mark-car)
    (vilpy--prettify-1)))

;;* Locals: marking
(defun vilpy-mark-right (arg)
  "Go right ARG times and mark."
  (interactive "p")
  (let* ((pt (point))
         (mk (mark))
         (vilpy-ignore-whitespace t)
         (r (vilpy--out-forward arg)))
    (deactivate-mark)
    (if (or (= pt (point))
            (= mk (point))
            (and (region-active-p)
                 (= (region-beginning)
                    (region-end))))
        (progn
          (vilpy--complain "can't go any further")
          (if (> mk pt)
              (vilpy--mark (cons pt mk))
            (vilpy--mark (cons mk pt)))
          nil)
      (vilpy--mark
       (vilpy--bounds-dwim))
      r)))

(defun vilpy-mark-left (arg)
  "Go left ARG times and mark."
  (interactive "p")
  (if (vilpy-mark-right arg)
      (vilpy-other)
    (when (= (point) (region-end))
      (exchange-point-and-mark))))

(defun vilpy-mark-car ()
  "Mark the car of current thing."
  (interactive)
  (vilpy--remember)
  (let ((bnd-1 (vilpy--bounds-dwim))
        bnd-2)
    (cond ((and (eq (char-after (car bnd-1)) ?\")
                (eq (char-before (cdr bnd-1)) ?\")
                (eq 1 (length (read (format "(%s)" (vilpy--string-dwim))))))
           (vilpy--mark (cons (1+ (car bnd-1))
                              (1- (cdr bnd-1)))))

          ((and (eq (char-after (car bnd-1)) ?\`)
                (eq (char-before (cdr bnd-1)) ?\'))
           (vilpy--mark (cons (1+ (car bnd-1))
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
               (vilpy--mark bnd-2)
             (vilpy--complain "can't descend further"))))))

;;* Locals: miscellanea
(declare-function vilpy--eval-python "le-python")

(defun vilpy-undo ()
  "Deactivate region and `undo'."
  (interactive)
  (when (region-active-p)
    (deactivate-mark t))
  (undo))

(unless (fboundp 'macrop)
  (defun macrop (object)
    "Non-nil if and only if OBJECT is a macro."
    (let ((def (indirect-function object)))
      (when (consp def)
        (or (eq 'macro (car def))
            (and (autoloadp def) (memq (nth 4 def) '(macro t))))))))

(defalias 'vilpy--preceding-sexp
    (if (fboundp 'elisp--preceding-sexp)
        'elisp--preceding-sexp
      'preceding-sexp))

(defun vilpy-narrow (arg)
  "Narrow ARG sexps or region."
  (interactive "p")
  (cond ((region-active-p)
         (narrow-to-region (region-beginning) (region-end)))
        ((vilpy-left-p)
         (narrow-to-region (point)
                           (save-excursion
                             (vilpy-forward arg)
                             (point))))
        ((vilpy-right-p)
         (narrow-to-region (point)
                           (save-excursion
                             (vilpy-backward arg)
                             (point))))))

(defun vilpy-widen ()
  "Forward to `widen'."
  (interactive)
  (widen))

(defun vilpy-paste (arg)
  "Forward to `yank'.
If the region is active, replace instead of yanking.
When ARG is given, paste at that place in the current list."
  (interactive "p")
  (cond ((region-active-p)
         (let ((bnd (vilpy--bounds-dwim)))
           (deactivate-mark)
           (vilpy--maybe-safe-delete-region (car bnd)
                                            (cdr bnd))
           (insert (vilpy--maybe-safe-current-kill))))
        ((> arg 1)
         (vilpy-mark-car)
         (vilpy-down (- arg 2))
         (deactivate-mark)
         (just-one-space)
         (insert (vilpy--maybe-safe-current-kill))
         (unless (or (eolp) (looking-at vilpy-right))
           (just-one-space)
           (forward-char -1)))
        ((vilpy-right-p)
         (newline-and-indent)
         (insert (vilpy--maybe-safe-current-kill)))
        ((vilpy-left-p)
         (newline-and-indent)
         (forward-line -1)
         (vilpy--indent-for-tab)
         (insert (vilpy--maybe-safe-current-kill)))
        (t
         (insert (vilpy--maybe-safe-current-kill)))))

(defalias 'vilpy-font-lock-ensure
  (if (fboundp 'font-lock-ensure)
      'font-lock-ensure
    'font-lock-fontify-buffer))

(defun vilpy-map-done ()
  (interactive)
  (vilpy-map-delete-overlay)
  (setq vilpy-bind-var-in-progress nil)
  (vilpy-backward 1))

(defun vilpy-map-delete-overlay ()
  "Delete `vilpy-map-input-overlay'."
  (when (overlayp vilpy-map-input-overlay)
    (delete-overlay vilpy-map-input-overlay)))

;;* Predicates
(defun vilpy--in-string-p ()
  "Test if point is inside a string.
Return start of string it is."
  (let ((syn (syntax-ppss)))
    (or (and (nth 3 syn)
             (nth 8 syn))
        (and (eq (char-after) ?\")
             (not (eq ?\\ (char-before)))
             (point)))))

(defun vilpy--in-comment-p ()
  "Test if point is inside a comment."
  (or
   (save-excursion
     (unless (eolp)
       (forward-char 1))
     (nth 4 (syntax-ppss)))
   (and (bolp) (looking-at vilpy-outline-header))))

(defun vilpy--in-string-or-comment-p ()
  "Test if point is inside a string or a comment."
  (let* ((sp (syntax-ppss))
         (beg (nth 8 sp)))
    (when (or (eq (char-after beg) ?\")
              (nth 4 sp))
      beg)))

(defun vilpy--raw-comment-p (expr)
  "Return t if EXPR is a raw comment."
  (and (listp expr)
       (eq (car expr) 'ly-raw)
       (consp (cdr expr))
       (eq (cadr expr) 'comment)))

(defun vilpy--raw-string-p (expr)
  "Return t if EXPR is a raw comment."
  (and (listp expr)
       (eq (car expr) 'ly-raw)
       (consp (cdr expr))
       (eq (cadr expr) 'string)))

(defun vilpy--leftp ()
  "Return t if at region beginning, or at start of the list."
  (if (region-active-p)
      (= (point) (region-beginning))
    (or (vilpy-left-p)
        (looking-at vilpy-outline))))

(defun vilpy--symbolp (str)
  "Return t if STR is a symbol."
  (string-match "\\`\\(?:\\sw\\|\\s_\\)+\\'" str))

(defun vilpy--string-markedp ()
  "Return t if the current active region is a string."
  (and (region-active-p)
       (eq ?\" (char-after (region-beginning)))
       (eq ?\" (char-before (region-end)))))

(defun vilpy-bolp ()
  "Return t if point is at beginning of line, after optional spaces."
  (save-excursion
    (skip-chars-backward " \t")
    (bolp)))

(defun vilpy-after-string-p (str)
  "Return t if the string before point is STR."
  (string=
   (buffer-substring
    (max
     (- (point) (length str))
     (point-min))
    (point))
   str))

(defun vilpy--empty-line-p ()
  "Test whether the point is on an \"empty\" line.
Return t if the point is by itself on a line with optional whitespace.
Return 'right if the point is on a line with only right delimiters and
whitespace."
  (if (and (looking-at (concat "[[:space:]]*" vilpy-right "*$"))
           (vilpy-looking-back "^[[:space:]]*"))
      (if (looking-at (concat "[[:space:]]*" vilpy-right))
          'right
        t)
    nil))

(defun vilpy--preceding-syntax (preceding-syntax-alist &optional before after)
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
                     (vilpy-interleave
                      "\\|"
                      regexps))
              "\\)"
              after))))

(defun vilpy--in-empty-list-p (preceding-syntax-alist)
  "Test whether the point is in a list with no sexps.
A list with only characters that can precede a delimiter (e.g. \"`(,)\") is
consider an empty list."
  (and (vilpy-looking-back
        (concat vilpy-left
                "[[:space:]]*"
                (vilpy--preceding-syntax preceding-syntax-alist nil "*")))
       (looking-at (concat "[[:space:]]*" vilpy-right))))

(defun vilpy--not-at-sexp-p (preceding-syntax-alist)
  "Test whether the point is at a \"free\" spot and not at a wrappable sexp.
PRECEDING-SYNTAX-ALIST should be an alist of `major-mode' to a list of regexps.
The regexps correspond to valid syntax that can precede an opening delimiter in
each major mode."
  (let* ((space "[[:space:]]")
         (space-or-eol (concat "\\(" space "+\\|" space "*$\\)"))
         (right-or-eol (concat "\\(" vilpy-right "+\\|" space "*$\\)"))
         (special-syntax (vilpy--preceding-syntax preceding-syntax-alist))
         (line (buffer-substring-no-properties
                (line-beginning-position)
                (line-end-position))))
    (or (vilpy--in-empty-list-p preceding-syntax-alist)
        ;; empty line
        (string-match (concat "^" space "*" special-syntax "*" space "*$")
                      line)
        ;; empty position at end of list or line
        (and (looking-at right-or-eol)
             (vilpy-looking-back (concat space "+" special-syntax "*")))
        ;; empty position at beginning of list
        (and (looking-at space-or-eol)
             (vilpy-looking-back (concat vilpy-left special-syntax "*")))
        ;; empty position in middle
        (and (looking-at (concat space "+"))
             (vilpy-looking-back (concat space "+" special-syntax "*"))))))

;;* Pure
(declare-function vilpy-bounds-python-block "le-python")

(defun vilpy--bounds-dwim ()
  "Return a cons of region bounds if it's active.
Otherwise return cons of current string, symbol or list bounds."
  (let (bnd)
    (cond ((region-active-p)
           (cons (region-beginning)
                 (region-end)))
          ((and (setq bnd (vilpy--bounds-string))
                (or (eq (point) (car bnd))
                    (eq (point) (1- (cdr bnd)))))
           bnd)
          ((looking-at vilpy-outline)
           (save-excursion
             (cons
              (progn
                (outline-end-of-heading)
                (1+ (point)))
              (progn
                (outline-end-of-subtree)
                (skip-chars-backward "\n")
                (when (setq bnd (vilpy--bounds-comment))
                  (goto-char (1- (car bnd))))
                (point)))))
          ((save-excursion
             (when (vilpy-right-p)
               (backward-list))
             (and (or (looking-at (concat "[^[:space:]\n]*" vilpy-left))
                      (looking-at "[`'#]"))
                  (setq bnd (bounds-of-thing-at-point 'sexp))))
           (save-excursion
             (goto-char (car bnd))
             (vilpy--skip-delimiter-preceding-syntax-backward)
             (cons (point) (cdr bnd))))
          ((looking-at (vilpy-comment-char 2))
           (vilpy--bounds-comment))
          ((and (eq major-mode 'python-mode)
                (vilpy-bolp))
           (vilpy-bounds-python-block))
          (t
           (let ((res (ignore-errors
                        (bounds-of-thing-at-point
                         (if (looking-at vilpy-right)
                             'symbol
                           'sexp)))))
             (if res
                 (save-excursion
                   (goto-char (cdr res))
                   (vilpy--in-string-or-comment-p)
                   (skip-chars-backward "[.,]")
                   (cons (car res) (point)))
               (or
                (ignore-errors
                  (bounds-of-thing-at-point 'symbol))
                (and (vilpy-looking-back "\" *")
                     (save-excursion
                       (goto-char (match-beginning 0))
                       (vilpy--bounds-string)))
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

(defun vilpy--bounds-list ()
  "Return the bounds of smallest list that includes the point."
  (save-excursion
    (vilpy--exit-string)
    (when (looking-at vilpy-left)
      (forward-char))
    (when (vilpy-looking-back vilpy-right)
      (backward-char))
    (ignore-errors
      (let (beg end)
        (up-list)
        (setq end (point))
        (backward-list)
        (setq beg (point))
        (cons beg end)))))

(defun vilpy--bounds-string ()
  "Return bounds of current string."
  (unless (vilpy--in-comment-p)
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

(defun vilpy--bounds-comment ()
  "Return bounds of current comment."
  (and (vilpy--in-comment-p)
       (save-excursion
         (when (vilpy--beginning-of-comment)
           (let ((pt (point)))
             (while (and (vilpy--in-comment-p)
                         (forward-comment -1)
                         (vilpy-looking-back "^[[:space:]]*")
                         (= 1 (- (count-lines (point) pt)
                                 (if (bolp) 0 1))))
               (setq pt (point)))
             (goto-char pt))
           (if (looking-at "#|")
               (cons (point)
                     (progn
                       (comment-forward)
                       (point)))
             (let ((beg (vilpy--beginning-of-comment))
                   (pt (point))
                   (col (current-column)))
               (while (and (vilpy--in-comment-p)
                           (forward-comment 1)
                           (vilpy--beginning-of-comment)
                           (and (= 1 (- (count-lines pt (point))
                                        (if (bolp) 0 1)))
                                ;; count comments starting in different columns
                                ;; as separate
                                (= col (current-column))
                                ;; if there's code in between,
                                ;; count comments as separate
                                (vilpy-looking-back "^\\s-*")))
                 (setq pt (point)))
               (goto-char pt)
               (end-of-line)
               (cons beg (point))))))))

(defun vilpy--string-dwim (&optional bounds)
  "Return the string that corresponds to BOUNDS.
`vilpy--bounds-dwim' is used if BOUNDS is nil."
  (setq bounds (or bounds (vilpy--bounds-dwim)))
  (buffer-substring-no-properties (car bounds) (cdr bounds)))

(declare-function python-info-current-symbol "python")

(defun vilpy--current-function ()
  "Return current function as string."
  (if (region-active-p)
      (let ((str (vilpy--string-dwim)))
        (if (string-match "\\`[#'`]*\\(.*?\\)'?\\'" str)
            (match-string 1 str)
          nil))
    (save-excursion
      (if (eq major-mode 'python-mode)
          (let ((bnd (bounds-of-thing-at-point 'symbol)))
            (if bnd
                (vilpy--string-dwim bnd)
              (up-list -1)
              (python-info-current-symbol)))
        (vilpy--back-to-paren)
        (when (looking-at "(\\([^ \n)]+\\)[ )\n]")
          (match-string-no-properties 1))))))

;;* Utilities: movement
(defun vilpy--out-forward (arg &optional ignore-ws)
  "Move outside list forwards ARG times.
Return nil on failure, (point) otherwise."
  (vilpy--exit-string)
  (catch 'break
    (dotimes (_i arg)
      (if (ignore-errors (up-list) t)
          (if buffer-read-only
              (deactivate-mark)
            (unless (or ignore-ws vilpy-ignore-whitespace)
              (vilpy--remove-gaps)
              (vilpy--indent-for-tab)))
        (when (vilpy-left-p)
          (forward-list))
        (throw 'break nil)))
    (point)))

(defun vilpy--out-backward (arg &optional ignore-ws)
  "Move outside list forwards ARG times.
Return nil on failure, t otherwise."
  (let ((oldpt (point))
        newpt)
    (vilpy--out-forward arg ignore-ws)
    (when (vilpy-right-p)
      (forward-list -1))
    (if (= oldpt (setq newpt (point)))
        nil
      newpt)))

(defun vilpy--back-to-paren ()
  "Move to ( going out backwards."
  (let ((vilpy-ignore-whitespace t))
    (vilpy--exit-string)
    (while (and (not (looking-at "("))
                (vilpy--out-backward 1)))))

(defun vilpy--exit-string ()
  "When in string, go to its beginning."
  (let ((s (syntax-ppss)))
    (when (nth 3 s)
      (goto-char (nth 8 s)))))

(defun vilpy--beginning-of-comment ()
  "Go to beginning of comment on current line."
  (end-of-line)
  (comment-beginning)
  (let ((cs (comment-search-backward (line-beginning-position) t)))
    (or
     (when cs
       (goto-char cs))
     (and (looking-at (concat "^" vilpy-outline-header))
          (point)))))

(defun vilpy--skip-delimiter-preceding-syntax-backward ()
  "Move backwards past syntax that could precede an opening delimiter such as '.
Specifically, move backwards to the closest whitespace char or opening delimiter
or to the beginning of the line."
  (re-search-backward (concat "[[:space:]]" "\\|"
                              vilpy-left "\\|"
                              "^"))
  (goto-char (match-end 0)))

;;* Utilities: slurping and barfing
(defun vilpy--slurp-forward ()
  "Grow current sexp forward by one sexp."
  (let ((pt (point))
        (char (char-before)))
    (skip-chars-forward " \t")
    (delete-region pt (point))
    (unless (or (vilpy-after-string-p "()")
                (vilpy-after-string-p "[]")
                (vilpy-after-string-p "{}")
                (eolp))
      (insert " "))
    (when (ignore-errors
            (forward-sexp) t)
      (delete-region (1- pt) pt)
      (insert char))))

(defun vilpy--slurp-backward ()
  "Grow current sexp backward by one sexp."
  (let ((pt (point))
        (char (char-after)))
    (backward-sexp)
    (delete-region pt (1+ pt))
    (insert char)
    (backward-char)))

(defun vilpy--barf-forward ()
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
      (vilpy--reindent 1))))

(defun vilpy--barf-backward ()
  "Shrink current sexp backward by one sexp."
  (let ((pt (point))
        (char (char-before)))
    (unless (vilpy-after-string-p "()")
      (backward-char)
      (backward-sexp)
      (skip-chars-backward " \n	")
      (while (vilpy--in-comment-p)
        (goto-char (comment-beginning))
        (skip-chars-backward " \n	"))
      (delete-region (1- pt) pt)
      (insert char)
      (vilpy--indent-region (point) pt))))

(defun vilpy--replace-regexp-in-code (regexp to-string)
  "Replace text matching REGEXP with TO-STRING in whole buffer.
Ignore the matches in strings and comments."
  (goto-char (point-min))
  (while (re-search-forward regexp nil t)
    (unless (vilpy--in-string-or-comment-p)
      (replace-match to-string))))

;;* Utilities: source transformation
(defvar vilpy--braces-table
  (let ((table (make-char-table 'syntax-table nil)))
    (modify-syntax-entry ?\{ "(}  " table)
    (modify-syntax-entry ?\} "){  " table)
    (modify-syntax-entry ?\[ "(]  " table)
    (modify-syntax-entry ?\] ")[  " table)
    (modify-syntax-entry ?\( "()  " table)
    (modify-syntax-entry ?\) ")(  " table)
    table)
  "Syntax table for paired braces.")

(defvar vilpy--insert-replace-alist-clojure
  '(("#object[" "clojure-object")
    ("#?@(" "clojure-reader-conditional-splice")
    ("@(" "clojure-deref-list")
    ("#(" "clojure-lambda")
    ("#{" "clojure-set")
    ("@{" "clojure-deref-map")
    ("@[" "clojure-deref-vector")
    ("{" "clojure-map")
    ("#?(" "clojure-reader-conditional")))

(defvar vilpy--insert-replace-alist-elisp
  '(("#object[" "clojure-object")
    ("#?@(" "clojure-reader-conditional-splice")
    ("#(" "clojure-lambda")
    ("#?(" "clojure-reader-conditional")))

(defun vilpy--read-1 ()
  (let* ((alist (if (member major-mode vilpy-elisp-modes)
                    vilpy--insert-replace-alist-elisp
                  vilpy--insert-replace-alist-clojure))
         (regex (regexp-opt (mapcar #'car alist))))
    (goto-char (point-min))
    (while (re-search-forward regex nil t)
      (let* ((head-beg (match-beginning 0))
             (head-end (match-end 0))
             (head (match-string 0))
             (entry (assoc head alist))
             (class (cadr entry))
             str-mid)
        (unless (vilpy--in-string-or-comment-p)
          (backward-char 1)
          (save-excursion
            (if (save-match-data
                  (looking-at "((ly-raw string"))
                (forward-list 1)
              (with-syntax-table vilpy--braces-table
                (forward-list 1)))
            (setq str-mid (buffer-substring-no-properties head-end (1- (point))))
            (delete-region head-beg (point)))
          (insert "(ly-raw " class " (" str-mid "))")
          (backward-char (+ 3 (length str-mid))))))))

(defvar vilpy--clojure-char-literal-regex
  (format "\\\\\\(\\(?:\\(?:\\sw\\|%s\\)\\b\\)\\|[.,!@#$%%&*]\\|u[A-Za-z0-9]+\\)"
          (regexp-opt '("newline" "space" "tab" "formfeed" "backspace" "return")))
  "Regex for Clojure character literals.
See https://clojure.org/guides/weird_characters#_character_literal.")

(defun vilpy--read-replace (regex class &optional subexp)
  (setq subexp (or subexp 0))
  (goto-char (point-min))
  (while (re-search-forward regex nil t)
    (cond ((string= (match-string 0) "ly-raw")
           (if (looking-at " clojure-\\(map\\|set\\|lambda\\)")
               (goto-char (match-end 0))
             (up-list)))
          ((vilpy--in-string-or-comment-p))
          (t
           (replace-match
            (format "(ly-raw %s %S)"
                    class
                    (substring-no-properties
                     (match-string subexp)))
            t t nil subexp)))))

(defun vilpy--clojure-mode-p ()
  "Return t if the current buffer is derived from `clojure-mode' or `clojure-ts-mode'."
  (or (derived-mode-p 'clojure-mode)
      (derived-mode-p 'clojure-ts-mode)))

;; TODO: Make the read test pass on string with semi-colon
(defun vilpy--read (str)
  "Read STR including comments and newlines."
  (let* ((deactivate-mark nil)
         (mode major-mode)
         cbnd
         (str (with-temp-buffer
                (funcall mode)
                (insert str)
                ;; ——— ly-raw —————————————————
                (vilpy--replace-regexp-in-code "(ly-raw" "(ly-raw raw")
                ;; ——— comments ———————————————
                (goto-char (point-min))
                (while (comment-search-forward (point-max) t)
                  (vilpy--beginning-of-comment)
                  (setq cbnd (cons (point) (line-end-position)))
                  (setq str (vilpy--string-dwim cbnd))
                  (delete-region (car cbnd) (cdr cbnd))
                  (insert (format "(ly-raw comment %S)" str)))
                ;; ——— reader macro syntax (LISP)
                (goto-char (point-min))
                (while (re-search-forward "#[a-z][\"(]" nil t)
                  (forward-char -1)
                  (unless (vilpy--in-string-or-comment-p)
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
                    (setq cbnd (vilpy--bounds-string))
                    (when cbnd
                      (if (or (vilpy-after-string-p "ly-raw comment \"")
                              (vilpy-after-string-p "ly-raw lisp-macro \""))
                          (goto-char (cdr cbnd))
                        (setq str (vilpy--string-dwim cbnd))
                        (delete-region (car cbnd) (cdr cbnd))
                        (insert (format "(ly-raw string %S)" str))))))
                ;; ——— newlines ———————————————
                (vilpy--replace-regexp-in-code "\n" " (ly-raw newline)")
                ;; ——— numbers ————————————————
                (goto-char (point-min))
                (while (re-search-forward "\\b[+-]?[0-9]+\\(?:\\.[0-9]+\\)?\\(?:e[+-]?[0-9]*\\)" nil t)
                  (if (setq cbnd (vilpy--bounds-string))
                      (goto-char (cdr cbnd))
                    (let ((s (match-string-no-properties 0)))
                      (delete-region (match-beginning 0) (match-end 0))
                      (insert (format "(ly-raw float \"%s\")" s)))))
                ;; ——— () —————————————————————
                (goto-char (point-min))
                (while (re-search-forward "\\(?:[^\\]\\|^\\)\\(()\\)" nil t)
                  (unless (vilpy--in-string-or-comment-p)
                    (replace-match "(ly-raw empty)" nil nil nil 1)))
                ;; ——— \ char syntax (Clojure)—
                (when (vilpy--clojure-mode-p)
                  (vilpy--read-replace vilpy--clojure-char-literal-regex "clojure-char"))
                ;; namespaced map #520
                (when (memq major-mode vilpy-clojure-modes)
                  (goto-char (point-min))
                  (while (re-search-forward "#\\(?:js\\|:\\(?:\\sw\\|\\s_\\)+\\) *\\(?:{\\|\\[\\)" nil t)
                    (let* ((head-beg (match-beginning 0))
                           (head-end (match-end 0))
                           (head (match-string 0))
                           str-mid tail)
                      (unless (vilpy--in-string-or-comment-p)
                        (backward-char 1)
                        (save-excursion
                          (with-syntax-table vilpy--braces-table
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
                (vilpy--read-1)
                ;; ——— ? char syntax ——————————
                (goto-char (point-min))
                (if (memq major-mode (cons 'hy-mode vilpy-clojure-modes))
                    (vilpy--read-replace "[[:alnum:]-/*<>_?.,\\\\:!@#=]+" "clojure-symbol")
                  (while (re-search-forward "\\(?:\\s-\\|\\s(\\)\\?" nil t)
                    (unless (vilpy--in-string-or-comment-p)
                      (let ((pt (point))
                            sexp)
                        (vilpy--skip-elisp-char)
                        (setq sexp (buffer-substring-no-properties pt (point)))
                        (delete-region (1- pt) (point))
                        (insert (format "(ly-raw char %S)" sexp))))))
                (when (vilpy--clojure-mode-p)
                  (vilpy--read-replace " *,+" "clojure-commas"))
                ;; ——— \ char syntax (LISP)————
                (goto-char (point-min))
                (while (re-search-forward "#\\\\\\(.\\)" nil t)
                  (unless (vilpy--in-string-or-comment-p)
                    (replace-match (format "(ly-raw lisp-char %S)"
                                           (substring-no-properties
                                            (match-string 0)))
                                   nil t)))
                ;; ——— Clojure gensym —————————
                (goto-char (point-min))
                (while (re-search-forward "\\([a-zA-Z][a-zA-z-/_0-9]*#\\)[ \t\n\r]" nil t)
                  (unless (vilpy--in-string-or-comment-p)
                    (replace-match
                     (format "(ly-raw clojure-gensym %S)"
                             (match-string-no-properties 1))
                     t nil nil 1)))
                ;; ——— Clojure keyword —————————
                (goto-char (point-min))
                (while (re-search-forward "\\(:\\.[^][({}) \t\n\r\"]+\\)" nil t)
                  (unless (vilpy--in-string-or-comment-p)
                    (replace-match (format "(ly-raw clojure-keyword %S)"
                                           (match-string-no-properties 1)))))
                ;; ——— #' —————————————————————
                (goto-char (point-min))
                (while (re-search-forward "#'" nil t)
                  (unless (vilpy--in-string-or-comment-p)
                    (forward-sexp)
                    (insert ")")
                    (replace-match "(ly-raw function ")))
                ;; ——— ,@ —————————————————————
                (goto-char (point-min))
                (while (re-search-forward "\\(?:[^\\]\\|^\\),@" nil t)
                  (unless (vilpy--in-string-or-comment-p)
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
                  (if (setq cbnd (vilpy--bounds-string))
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
                  (unless (vilpy--in-string-p)
                    (replace-match (format "(ly-raw reference %S)"
                                           (substring-no-properties
                                            (match-string 1)))
                                   nil nil nil 1)))
                ;; ——— ' ——————————————————————
                (goto-char (point-min))
                (while (re-search-forward "'" nil t)
                  (unless (vilpy--in-string-or-comment-p)
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
                  (unless (vilpy--in-string-or-comment-p)
                    (cond ((looking-at vilpy-left)
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
                (vilpy--replace-regexp-in-code "\\\\," "(ly-raw comma-symbol)")
                (goto-char (point-min))
                (while (re-search-forward "[^\\]?,[^@\"]" nil t)
                  (unless (vilpy--in-string-or-comment-p)
                    (backward-char 2)
                    (if (memq major-mode vilpy-clojure-modes)
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
                  (unless (vilpy--in-string-or-comment-p)
                    (delete-region (match-beginning 0) (match-end 0))
                    (insert "(ly-raw angle \"")
                    (re-search-forward ">")
                    (backward-delete-char 1)
                    (insert "\")")))
                ;; ——— cons cell syntax ———————
                (vilpy--replace-regexp-in-code " \\. " " (ly-raw dot) ")
                ;; Racket stuff
                (vilpy--replace-regexp-in-code "#t" "(ly-raw racket-true)")
                (vilpy--replace-regexp-in-code "#f" "(ly-raw racket-false)")
                (goto-char (point-min))
                (while (re-search-forward "#:\\(\\(?:\\sw\\|\\s_\\)+\\)" nil t)
                  (unless (vilpy--in-string-or-comment-p)
                    (replace-match (format "(ly-raw racket-option %s)"
                                           (match-string 1)))))
                ;; Clojure # in a symbol
                (goto-char (point-min))
                (while (re-search-forward "\\_<\\(?:\\sw\\|\\s_\\)+\\_>" nil t)
                  (unless (vilpy--in-string-p)
                    (when (cl-position ?# (match-string 0))
                      (let* ((bnd (vilpy--bounds-dwim))
                             (str (vilpy--string-dwim bnd)))
                        (delete-region (car bnd) (cdr bnd))
                        (insert (format "(ly-raw symbol %S)" str))))))
                ;; Clojure (. object method)
                (goto-char (point-min))
                (while (re-search-forward "(\\.[\t\n ]" nil t)
                  (if (setq cbnd (vilpy--bounds-string))
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

(defun vilpy--skip-elisp-char ()
  (unless (vilpy-after-string-p "?")
    (error "unexpected"))
  (if (looking-at "\\\\")
      (forward-sexp 1)
    (forward-char 1)))

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
(defun vilpy--replace (lst from to)
  "Recursively replace elements in LST from FROM to TO."
  (cond ((eq lst from)
         to)
        ((not (consp lst))
         lst)
        (t
         (cons
          (vilpy--replace (car lst) from to)
          (vilpy--replace (cdr lst) from to)))))

;;* Utilities: error reporting
(defun vilpy--complain (msg)
  "Display MSG if `vilpy-verbose' is t."
  (when (and vilpy-verbose
             (null noninteractive))
    (message "[vilpy] %s: %s"
             (prin1-to-string this-command)
             msg)))

(defun vilpy--complain-not-supported ()
  (vilpy--complain "Command not supported for current mode. Please consult the variable `vilpy--handlers-alist`."))

(defun vilpy--complain-missing-eval-handler ()
  (vilpy--complain "Could not find eval handler for current mode."))

(defun vilpy--complain-unrecognized-key ()
  (vilpy--complain "Ignoring unmapped key."))

;;* Utilities: rest
(defun vilpy--indent-region (beg end)
  "Indent region BEG END without reporting progress."
  (save-excursion
    (setq end (copy-marker end))
    (goto-char beg)
    (while (< (point) end)
      (or (and (bolp) (eolp))
          (indent-according-to-mode))
      (forward-line 1))
    (move-marker end nil)))

(defvar vilpy-no-indent-modes '(minibuffer-inactive-mode
                                comint-mode)
  "List of major modes where `indent-for-tab-command' should not be used.
`vilpy--indent-for-tab' will do nothing if the current mode or any of its parent
modes is in this list.")

(defun vilpy--indent-for-tab ()
  "Call `indent-for-tab-command'."
  (unless (or (memq major-mode vilpy-no-indent-modes)
              (apply #'derived-mode-p vilpy-no-indent-modes)
              (= 0 (buffer-size)))
    (let ((tab-always-indent t))
      (vilpy-flet (message (&rest _x))
        (indent-for-tab-command)))))

(defun vilpy--remove-gaps ()
  "Remove dangling `\\s)'."
  (when (and (vilpy-right-p)
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
                (vilpy--in-string-or-comment-p)))
      (delete-region (match-beginning 1)
                     (match-end 1)))))

(defun vilpy--surround-region (alpha omega)
  "Surround active region with ALPHA and OMEGA and re-indent."
  (let ((beg (region-beginning))
        (end (region-end)))
    (goto-char end)
    (insert omega)
    (goto-char beg)
    (insert alpha)
    (deactivate-mark)
    (indent-region beg (+ 2 end))))

(defun vilpy--mark (bnd)
  "Mark BND.  BND is a cons of beginning and end positions."
  (setq deactivate-mark nil)
  (set-mark (car bnd))
  (goto-char (cdr bnd)))

(defun vilpy--space-unless (context)
  "Insert one space.
Unless inside string or comment, or `looking-back' at CONTEXT."
  (let ((inhibit-field-text-motion t))
    (unless (or vilpy-no-space
                (bolp)
                (and (window-minibuffer-p)
                     (eq (point) (minibuffer-prompt-end)))
                (vilpy--in-string-or-comment-p)
                (vilpy-looking-back context))
      (insert " "))))

(defun vilpy--delimiter-space-unless (preceding-syntax-alist)
  "Like `vilpy--space-unless' but use PRECEDING-SYNTAX-ALIST for decision.
PRECEDING-SYNTAX-ALIST should be an alist of `major-mode' to a list of regexps.
When `looking-back' at any of these regexps, whitespace, or a delimiter, do not
insert a space."
  (vilpy--space-unless
   (concat "^\\|\\s-\\|" vilpy-left
           (vilpy--preceding-syntax preceding-syntax-alist "\\|"))))

(defun vilpy--reindent (&optional arg)
  "Reindent current sexp.  Up-list ARG times before that."
  (cond ((region-active-p)
         (indent-region (region-beginning)
                        (region-end)))
        (arg
         (vilpy-save-excursion
           (vilpy--out-forward arg)
           (backward-list)
           (indent-sexp)))

        ((vilpy-right-p)
         (save-excursion
           (backward-list)
           (indent-sexp)))

        ((vilpy-left-p)
         (indent-sexp))

        (t
         (save-excursion
           (vilpy--out-forward 1)
           (backward-list)
           (indent-sexp)))))

(defun vilpy--delete ()
  "Delete one sexp."
  (unless (vilpy-left-p)
    (error "Bad position"))
  (let ((bnd (vilpy--bounds-list)))
    (delete-region (car bnd) (cdr bnd))
    (cond ((looking-at (concat "\n+" vilpy-left))
           (delete-region (match-beginning 0)
                          (1- (match-end 0))))
          ((looking-at "\n\n+"))
          ((looking-at "\\([ ]*\\)\n")
           (delete-region (match-beginning 1)
                          (match-end 1)))
          ((looking-at vilpy-right))
          ((eolp))
          (t
           (just-one-space)
           (when (vilpy-after-string-p "( ")
             (backward-delete-char 1))))))

(defun vilpy--recenter-bounds (bnd)
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

(defun vilpy--prin1-to-string (expr offset mode)
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
        (vilpy--insert expr))
      (buffer-substring-no-properties
       (+ (point-min) offset)
       (point-max)))))

(defun vilpy--splice-to-str (sexp)
  "Return the printed representation of SEXP.
The outer delimiters are stripped."
  (if sexp
      (substring
       (prin1-to-string sexp) 1 -1)
    ""))

(defun vilpy--insert (expr)
  "Insert the EXPR read by `vilpy--read'."
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
           (insert (format "#{%s}" (vilpy--splice-to-str (cl-caddr sxp))))
           (goto-char beg))
          (clojure-map
           (delete-region beg (point))
           (insert (format "{%s}" (vilpy--splice-to-str (cl-caddr sxp))))
           (goto-char beg))
          (clojure-object
           (delete-region beg (point))
           (insert (format "#object[%s]" (vilpy--splice-to-str (cl-caddr sxp))))
           (goto-char beg))
          (splice
           (delete-region beg (point))
           (insert
            (nth 2 sxp)
            (vilpy--splice-to-str (car (nthcdr 4 sxp)))
            (nth 3 sxp))
           (goto-char beg))
          (clojure-deref-map
           (delete-region beg (point))
           (insert (format "@{%s}" (vilpy--splice-to-str (cl-caddr sxp))))
           (goto-char beg))
          (clojure-deref-vector
           (delete-region beg (point))
           (insert (format "@[%s]" (vilpy--splice-to-str (cl-caddr sxp))))
           (goto-char beg))
          (clojure-deref-list
           (delete-region beg (point))
           (insert (format "@(%s)" (vilpy--splice-to-str (cl-caddr sxp))))
           (goto-char beg))
          (clojure-reader-conditional-splice
           (delete-region beg (point))
           (insert (format "#?@(%s)" (vilpy--splice-to-str (cl-caddr sxp))))
           (goto-char beg))
          (clojure-reader-conditional
           (delete-region beg (point))
           (insert (format "#?(%s)" (vilpy--splice-to-str (cl-caddr sxp))))
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
                  (vilpy--in-string-p))
          (replace-match "." nil nil nil 1)))
      (goto-char (point-min))
      (while (re-search-forward "[0-9]+\\(\\\\#\\)" nil t)
        (replace-match "#" nil t nil 1))
      (when vilpy-do-fill
        (goto-char (point-min))
        (while (re-search-forward " " nil t)
          (cond ((vilpy--in-string-p))

                ((vilpy--in-comment-p)
                 (fill-paragraph)
                 (goto-char (cdr (vilpy--bounds-comment))))

                ((> (current-column) fill-column)
                 (newline-and-indent)))))
      (goto-char (point-max))
      (widen)))
  (when (and (vilpy-right-p)
             (not (vilpy--in-comment-p)))
    (backward-list)
    (indent-sexp)
    (forward-list)))


(defun vilpy--trim-whitespace-at-bol ()
  "If the point is at '(', remove whitespace (tab and blank space) before point."
  (when (and (looking-at "(")
             (= (point)
                (save-excursion
                  (vilpy--out-backward 99)
                  (point))))
    (let ((pt (point)))
      (skip-chars-backward " \t")
      (delete-region pt (point)))))

(defun vilpy--get-lisp-indent-function ()
  (if (looking-at "(\\(cl-defun\\|defhydra\\)")
      #'common-lisp-indent-function
    lisp-indent-function))

(defun vilpy--prettify-emacs-lisp-sexp ()
  (interactive)
  (let* ((lisp-indent-function (vilpy--get-lisp-indent-function))
         (bnd (vilpy--bounds-dwim))
         (str (vilpy--string-dwim bnd))
         (offset (save-excursion (goto-char (car bnd)) (current-column)))
         (was-left (vilpy-left-p)))
    (cond ((looking-at (vilpy-comment-char 2)))
          (t
           (let* ((max-lisp-eval-depth 10000)
                  (max-specpdl-size 10000)
                  (res (vilpy--sexp-normalize (vilpy--read str)))
                  (new-str (vilpy--prin1-to-string res offset major-mode)))
             (unless (string= str new-str)
               ;; We should not do this if new-str failed to eval.
               (unless (string= "nil" new-str)
                 (delete-region (car bnd)
                                (cdr bnd))
                 (insert new-str))
               (when was-left
                 (backward-list))))))))

(defun vilpy--sexp-trim-trailing-newlines (foo comment)
  "Trim trailing (ly-raw newline) from FOO.
Treat comments differently when COMMENT is t."
  (if (and (consp foo) (consp (cdr foo)))
      (let ((expr (reverse foo)))
        (while (and (consp expr)
                    (listp expr)
                    (equal (car expr) '(ly-raw newline))
                    (not (and comment
                              (vilpy--raw-comment-p (cadr expr)))))
          (setq expr (cdr expr)))
        (reverse expr))
    foo))

(defun vilpy--sexp-normalize (foo)
  "Return a pretty version of FOO.
Only `ly-raw' lists within FOO are manipulated."
  (cond ((null foo)
         nil)

        ((consp foo)
         (cons (vilpy--sexp-normalize
                (vilpy--sexp-trim-trailing-newlines (car foo) t))
               (vilpy--sexp-normalize
                (vilpy--sexp-trim-trailing-newlines (cdr foo) t))))
        (t
         foo)))

(defun vilpy--teleport (beg end endp regionp)
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
                 (vilpy-bolp))
        (delete-region (line-beginning-position)
                       (1+ (point))))
      (when (> beg1 beg)
        (cl-decf beg1 (- size (buffer-size))))
      (goto-char beg1)
      (when (looking-at vilpy-left)
        (save-excursion
          (newline-and-indent)))
      (unless (vilpy-looking-back "[ ([{]")
        (insert " ")
        (cl-incf beg1))
      (insert str)
      (unless (looking-at "[\n)]")
        (insert "\n")
        (backward-char))
      (vilpy-save-excursion
        (vilpy--reindent 1)
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

(defun vilpy--swap-regions (bnd1 bnd2)
  "Swap buffer regions BND1 and BND2.
Return a cons of the new text cordinates."
  (when (> (car bnd1) (car bnd2))
    (cl-rotatef bnd1 bnd2))
  (let ((str1 (vilpy--string-dwim bnd1))
        (str2 (vilpy--string-dwim bnd2)))
    (goto-char (car bnd2))
    (delete-region (car bnd2) (cdr bnd2))
    (insert str1)
    (when (vilpy--in-comment-p)
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

(defun vilpy--ensure-visible ()
  "Remove overlays hiding point."
  (let ((overlays (overlays-at (point)))
        ov expose)
    (while (setq ov (pop overlays))
      (if (and (invisible-p (overlay-get ov 'invisible))
               (setq expose (overlay-get ov 'isearch-open-invisible)))
          (funcall expose ov)))))

(defun vilpy--delete-pair-in-string (left right)
  "Delete a pair of LEFT and RIGHT in string."
  (let ((bnd (vilpy--bounds-string)))
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

(defvar macrostep-keymap)
(defvar vilpy--compat-cmd nil
  "Store the looked up compat command.")

(defun vilpy--insert-or-call (def plist)
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
       (unless (looking-at vilpy-outline)
         (vilpy--ensure-visible))
       (cond ,@(cond ((null override) nil)
                     ((functionp override)
                      `((funcall ,override)))
                     ((eq (car override) 'cond)
                      (cdr override))
                     (t
                      (error "Unexpected :override %S" override)))

             ,@(when (memq 'god-mode vilpy-compat)
                     '(((and (or (bound-and-true-p god-global-mode)
                                 (bound-and-true-p god-local-mode)))
                        (call-interactively 'god-mode-self-insert))))

             ,@(when (memq 'macrostep vilpy-compat)
                     '(((and (bound-and-true-p macrostep-mode)
                         (setq vilpy--compat-cmd (lookup-key macrostep-keymap (this-command-keys))))
                        (call-interactively vilpy--compat-cmd))))

             ,@(when (memq 'magit-blame-mode vilpy-compat)
                     '(((and (bound-and-true-p magit-blame-mode)
                         (setq vilpy--compat-cmd (lookup-key magit-blame-mode-map (this-command-keys))))
                        (call-interactively vilpy--compat-cmd))))

             ((region-active-p)
              (call-interactively ',def))

             ((vilpy--in-string-or-comment-p)
              (setq this-command 'self-insert-command)
              (call-interactively 'self-insert-command))

             ((or (vilpy-left-p)
                  (vilpy-right-p)
                  (and (vilpy-bolp)
                       (or (looking-at vilpy-outline-header)
                           (looking-at vilpy-outline))))
              (call-interactively ',def))

             (t
              (setq this-command 'self-insert-command)
              (call-interactively
               (quote
                ,(or inserter
                     'self-insert-command))))))))

(defun vilpy--find-unmatched-delimiters (beg end)
  "Return the positions of unmatched delimiters between BEG and END.
When the region is a greater size than `vilpy-safe-threshold', it will not be
checked and nil will be returned."
  (if (> (- end beg) vilpy-safe-threshold)
      nil
    (save-excursion
      (goto-char beg)
      (let ((vilpy-delimiters (concat (substring vilpy-right 0 -1)
                                      "\""
                                      (substring vilpy-left 1)))
            matched-left-quote-p
            string-bounds
            string-end
            comment-end
            left-positions
            right-positions)
        (while (re-search-forward vilpy-delimiters end t)
          (let* ((match-beginning (match-beginning 0))
                 (matched-delimiter (buffer-substring-no-properties
                                     match-beginning
                                     (match-end 0))))
            (cond
              ((and vilpy-safe-actions-ignore-strings
                    (save-excursion
                      (goto-char match-beginning)
                      (setq string-bounds (vilpy--bounds-string))
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
              ((and vilpy-safe-actions-ignore-comments
                    (save-excursion
                      (goto-char match-beginning)
                      (setq comment-end (cdr (vilpy--bounds-comment)))))
               (if (< comment-end end)
                   (goto-char comment-end)
                 (goto-char end)))
              (t
               (unless (looking-back "\\\\." (- (point) 2))
                 (if (or (string-match vilpy-left matched-delimiter)
                         (and (string= matched-delimiter "\"")
                              (vilpy--in-string-p)))
                     (push match-beginning left-positions)
                   (if (> (length left-positions) 0)
                       (pop left-positions)
                     (push match-beginning right-positions))))))))
        (nreverse (append left-positions right-positions))))))

(defun vilpy--maybe-split-safe-region (beg end &optional end-unsafe-p)
  "Return a list of regions between BEG and END that are safe to delete.
It is expected that there are no unmatched delimiters in between BEG and END.
Split the region if deleting it would pull unmatched delimiters into a comment.
Specifically, split the region if all of the following are true:

- `vilpy-safe-actions-no-pull-delimiters-into-comments' is non-nil
- BEG is inside a comment
- END is not in a comment
- Either there are unmatched delimiters on the line after END or END-UNSAFE-P is
  non-nil

Otherwise, just return a list with the initial region. The regions are returned
in reverse order so that they can be easily deleted without recalculation."
  (if (and vilpy-safe-actions-no-pull-delimiters-into-comments
           ;; check that BEG is inside a comment
           ;; `vilpy--in-comment-p' returns t at comment start which is
           ;; unwanted here
           (and (save-excursion
                  (nth 4 (syntax-ppss beg))))
           (save-excursion
             (goto-char end)
             ;; check that END is not inside or a comment and that the
             ;; following line has unmatched delimiters or has been specified
             ;; as unsafe to pull into a comment
             (and (not (vilpy--in-comment-p))
                  (or end-unsafe-p
                      (vilpy--find-unmatched-delimiters
                       end
                       (line-end-position))))))
      ;; exclude newline; don't pull END into a comment
      (let ((comment-end-pos (save-excursion
                               (goto-char beg)
                               (cdr (vilpy--bounds-comment)))))
        (list (cons (1+ comment-end-pos) end)
              (cons beg comment-end-pos)))
    (list (cons beg end))))

(defun vilpy--find-safe-regions (beg end)
  "Return a list of regions between BEG and END that are safe to delete.
The regions are returned in reverse order so that they can be easily deleted
without recalculation."
  (let ((unmatched-delimiters (vilpy--find-unmatched-delimiters beg end))
        (maybe-safe-pos beg)
        safe-regions)
    (dolist (unsafe-pos unmatched-delimiters)
      (unless (= maybe-safe-pos unsafe-pos)
        (setq safe-regions
              (nconc (vilpy--maybe-split-safe-region maybe-safe-pos unsafe-pos
                                                     t)
                     safe-regions)))
      (setq maybe-safe-pos (1+ unsafe-pos)))
    (setq safe-regions
          (nconc (vilpy--maybe-split-safe-region maybe-safe-pos end)
                 safe-regions))))

(defun vilpy--maybe-safe-delete-region (beg end)
  "Delete the region from BEG to END.
If `vilpy-safe-delete' is non-nil, exclude unmatched delimiters."
  (if vilpy-safe-delete
      (let ((safe-regions (vilpy--find-safe-regions beg end)))
        (dolist (safe-region safe-regions)
          (delete-region (car safe-region) (cdr safe-region))))
    (delete-region beg end)))

(defun vilpy--maybe-safe-kill-region (beg end)
  "Kill the region from BEG to END.
If `vilpy-safe-delete' is non-nil, exclude unmatched delimiters."
  (if vilpy-safe-delete
      (let ((safe-regions (vilpy--find-safe-regions beg end))
            safe-strings)
        (dolist (safe-region safe-regions)
          (push (vilpy--string-dwim safe-region) safe-strings)
          (delete-region (car safe-region) (cdr safe-region)))
        (kill-new (apply #'concat safe-strings)))
    (kill-region beg end)))

(defun vilpy--maybe-safe-region (beg end)
  "Return the text from BEG to END.
If `vilpy-safe-copy' is non-nil, exclude unmatched delimiters."
  (if vilpy-safe-copy
      (let ((safe-regions (vilpy--find-safe-regions beg end))
            safe-strings)
        (dolist (safe-region safe-regions)
          (push (vilpy--string-dwim safe-region) safe-strings))
        (apply #'concat safe-strings))
    (vilpy--string-dwim (cons beg end))))

(defvar vilpy--pairs
  '(("(" . ")")
    ("[" . "]")
    ("{" . "}")))

(defun vilpy--balance (text)
  "Return TEXT with unmatched delimiters added to the beginning or end.
This does not attempt to deal with unbalanced double quotes as it is not always
possible to infer which side the missing quote should be added to."
  (let ((old-major-mode major-mode))
    (with-temp-buffer
      (funcall old-major-mode)
      (insert text)
      (let ((unmatched-positions
             (vilpy--find-unmatched-delimiters (point-min) (point-max)))
            add-to-beginning
            add-to-end
            delim)
        (dolist (pos unmatched-positions)
          (setq delim (buffer-substring pos (1+ pos)))
          (cond ((string-match vilpy-left delim)
                 (push (cdr (assoc delim vilpy--pairs))
                       add-to-end))
                ((string-match vilpy-right delim)
                 (push (car (rassoc delim vilpy--pairs))
                       add-to-beginning))))
        (when add-to-beginning
          (goto-char (point-min))
          (insert (apply #'concat add-to-beginning)))
        (when add-to-end
          (goto-char (point-max))
          (when (and vilpy-safe-actions-no-pull-delimiters-into-comments
                     (vilpy--in-comment-p))
            (push "\n" add-to-end))
          (insert (apply #'concat add-to-end)))
        (buffer-substring (point-min) (point-max))))))

(defun vilpy--maybe-safe-current-kill ()
  "Return the most recent kill.
If `vilpy-safe-paste' is non-nil, any unmatched delimiters will be added to it."
  (if vilpy-safe-paste
      (vilpy--balance (current-kill 0))
    (current-kill 0)))

(defun vilpy-insert-at-end-of-sexp ()
  (interactive)
  (when (vilpy-left-p)
    (progn (vilpy-other)
           (backward-char))))


;;; Handlers helpers

(defun vilpy--get-handlers ()
  "Return the appropriate handlers for the current buffer.
This is done by iterating over `vilpy--handlers-alist` and finding
the first value for which `decider-fn` returns a truthy value, or `nil`
if there is no such value."
  (cl-find-if (lambda (e)
                (let* ((config (cdr e))
                       (decider-fn (assoc-default :decider-fn config)))
                  (funcall decider-fn)))
              vilpy--handlers-alist))


;;; Evaluation

(defun vilpy-eval ()
  "Evaluate the current sexp after point (if the point is right before a sexp),
before it (if the point is right after a sexp) or the current region (if the region is active).

The evaluation function is defined by `vilpy--handlers-alist`."
  (interactive)
  (let ((eval-last-sexp-handler (assoc-default :eval-last-sexp (vilpy--get-handlers)))
        (eval-region-handler (assoc-default :eval-region (vilpy--get-handlers))))
    (cond
     ((and (region-active-p)
           (not eval-region-handler))
      (vilpy--complain-missing-eval-handler))
     ((and (region-active-p)
           eval-region-handler)
      (call-interactively eval-region-handler))
     ((and (not (region-active-p))
           (not eval-last-sexp-handler))
      (vilpy--complain-missing-eval-handler))
     ((vilpy-left-p)
      (save-excursion
        (vilpy-forward 1)
        (call-interactively eval-last-sexp-handler)))
     ((vilpy-right-p)
      (call-interactively eval-last-sexp-handler)))))

(defun vilpy-eval-buffer ()
  "Evaluate the buffer.

The evaluation function is defined by `vilpy--handlers-alist`."
  (interactive)
  (if-let ((handler (assoc-default :eval-buffer (vilpy--get-handlers))))
      (call-interactively handler)
    (vilpy--complain-not-supported)))

(defun vilpy-eval-defun ()
  "Evaluate the top level form.

The evaluation function is defined by `vilpy--handlers-alist`."
  (interactive)
  (if-let ((handler (assoc-default :eval-defun (vilpy--get-handlers))))
      (call-interactively handler)
    (vilpy--complain-not-supported)))


;;; Describe symbol

(declare-function cider-doc-lookup "ext:cider-doc")
(defun vilpy--cider-describe-symbol ()
  (interactive)
  (require 'cider-doc)
  (cider-doc-lookup (vilpy--current-function)))

(defun vilpy-describe ()
  "Describes the symbol at point.

The function used for describing is defined by `vilpy--handlers-alist`."
  (interactive)
  (if-let ((handler (assoc-default :describe-symbol (vilpy--get-handlers))))
      (call-interactively handler)
    (vilpy--complain-not-supported)))

(defun vilpy--inf-clojure-describe-symbol ()
  (interactive)
  (save-excursion
    (forward-char 1)
    (call-interactively 'inf-clojure-show-var-documentation)))

(defun vilpy--emacs-lisp-describe-symbol ()
  (interactive)
  (let ((symbol (intern-soft (vilpy--current-function))))
    (cond ((fboundp symbol)
           (describe-function symbol))
          ((boundp symbol)
           (describe-variable symbol)))))


;;; Recentering

;; mostly taken from evil-scroll-line-to-{top, center, bottom}
(defun vilpy-scroll-line-to-top ()
  "Scrolls the current line to the top the window."
  (interactive)
  (let ((line (line-number-at-pos (point)))
        (col (current-column)))
    (goto-char (point-min))
    (forward-line (1- line))
    (recenter (1- (max 1 scroll-margin)))
    (move-to-column col)))

(defun vilpy-scroll-line-to-center ()
  "Scrolls the current line to the top the window."
  (interactive)
  (let ((col (current-column)))
    (recenter nil)
    (move-to-column col)))

(defun vilpy-scroll-line-to-bottom ()
  "Scrolls the current line to the bottom of the window."
  (interactive)
  (let ((line (line-number-at-pos (point)))
        (col (current-column)))
    (goto-char (point-min))
    (forward-line (1- line))
    (recenter (- (max 1 scroll-margin)))
    (move-to-column col)))

(defun vilpy-scroll-actions ()
  (interactive)
  (cl-case (read-char-from-minibuffer "Actions:\n
z: Scroll line to center
t: Scroll line to top
b: Scroll line to bottom
\n")
    (?z (progn (call-interactively 'vilpy-scroll-line-to-center)
               (message nil)))
    (?t (progn (call-interactively 'vilpy-scroll-line-to-top)
               (message nil)))
    (?b (progn (call-interactively 'vilpy-scroll-line-to-bottom)
               (message nil)))
    (t (vilpy--complain-unrecognized-key))))


;;; Pretty printing

(defun vilpy--prettify-1 ()
  "Normalize/prettify current sexp."
  (vilpy--trim-whitespace-at-bol)
  (when-let ((handler (assoc-default :indent-sexp (vilpy--get-handlers))))
    ;; if `handler` is not set, simply do nothing - no need to make vilpy complain,
    ;; since this function is used internally and we don't want to spam the *Messages* buffer.
    (call-interactively handler)))

(declare-function clojure-indent-region "ext:clojure-mode")

(defun vilpy-clojure-indent ()
  "Indents the next or previous sexp, depending on the point."
  (interactive)
  (cond ((derived-mode-p 'clojure-mode)
         (if (not (or (vilpy-right-p) (vilpy-left-p)))
             (vilpy-complain "Point is not at beginning or end of sexp.")
           (let* ((beg (if (vilpy-right-p)
                           (save-excursion (backward-sexp) (point))
                         (point)))
                  (end (if (vilpy-right-p)
                           (point)
                         (save-excursion (forward-sexp) (point)))))
             (clojure-indent-region beg end))))
        ((derived-mode-p 'clojure-ts-mode)
         (message "This functionality depends on `clojure-indent-region', which is not yet implemented on clojure-ts-mode."))
        (message "Ignoring command. Expected `clojure-mode'.")))

;;* Key definitions
(defvar ac-trigger-commands '(self-insert-command))

(defadvice ac-handle-post-command (around ac-post-command-advice activate)
  "Don't `auto-complete' when region is active."
  (unless (region-active-p)
    ad-do-it))

(defun vilpy--delsel-advice (orig-fun)
  "Advice for `delete-selection-mode'.
Usage:
 (advice-add 'delete-selection-pre-hook :around 'vilpy--delsel-advice)"
  (if (and (use-region-p)
           (string-match-p "^special" (symbol-name this-command)))
      (progn
        (delete-active-region)
        (setq this-command 'ignore)
        (self-insert-command 1))
    (funcall orig-fun)))

(defun vilpy--undo-tree-advice (&optional _arg)
  "Advice to run before `undo-tree-undo'.

Otherwise, executing undo in middle of a vilpy overlay operation
irreversibly corrupts the undo tree state. "
  (vilpy-map-delete-overlay))

(advice-add 'undo-tree-undo :before 'vilpy--undo-tree-advice)

(defun vilpy-define-key (keymap key def &rest plist)
  "Forward to (`define-key' KEYMAP KEY FUNC).
FUNC is obtained from (`vilpy--insert-or-call' DEF PLIST)."
  (declare (indent 3))
  (require 'eldoc)
  (let ((func (defalias (intern (concat "special-" (symbol-name def)))
                (vilpy--insert-or-call def plist))))
    (add-to-list 'ac-trigger-commands func)
    (eldoc-add-command func)
    (define-key keymap (kbd key) func)))

(defun vilpy-move-and-slurp-actions ()
  (interactive)
  (cl-case (read-char-from-minibuffer "Actions:\n
h: Move left
l: Move right
j: Slurp down
k: Slurp up
\n")
    (?h (progn (call-interactively 'vilpy-move-left)
               (message nil)))
    (?j (progn (call-interactively 'vilpy-down-slurp)
               (message nil)))
    (?k (progn (call-interactively 'vilpy-up-slurp)
               (message nil)))
    (?l (progn (call-interactively 'vilpy-move-right)
               (message nil)))
    (t (vilpy--complain-unrecognized-key))))

(defun vilpy-go-actions ()
  (interactive)
  (cl-case (read-char-from-minibuffer "Actions:\n
g: Go to first defun
n: Narrow
w: Widen
\n")
    (?g (call-interactively 'vilpy-go-to-first-defun))
    (?n (progn (call-interactively 'vilpy-narrow)
               (message nil)))
    (?w (progn (call-interactively 'vilpy-widen)
               (message nil)))
    (t (vilpy--complain-unrecognized-key))))

(defvar vilpy-mode-map-special
  (let ((map (make-sparse-keymap)))
    ;; getting out of special mode
    (vilpy-define-key map "A" 'vilpy-insert-at-end-of-sexp)
    (vilpy-define-key map "a" 'forward-char)
    (vilpy-define-key map "o" 'vilpy-open-line-below)
    (vilpy-define-key map "O" 'vilpy-open-line-above)
    ;; navigation
    (vilpy-define-key map "h" 'vilpy-step-out)
    (vilpy-define-key map "l" 'vilpy-step-in)
    (vilpy-define-key map "j" 'vilpy-down)
    (vilpy-define-key map "k" 'vilpy-up)
    (vilpy-define-key map "W" 'vilpy-knight-up)
    (vilpy-define-key map "S" 'vilpy-knight-down)
    (vilpy-define-key map "I" 'vilpy-beginning-of-defun)
    (vilpy-define-key map "b" 'vilpy-back)
    (vilpy-define-key map "L" 'vilpy-right)
    (vilpy-define-key map "G" 'vilpy-go-to-last-defun)
    ;; code actions
    (vilpy-define-key map "=" 'vilpy-tab)
    (vilpy-define-key map "e" 'vilpy-eval)
    (vilpy-define-key map "D" 'vilpy-eval-defun)
    (vilpy-define-key map "B" 'vilpy-eval-buffer)
    (vilpy-define-key map "K" 'vilpy-describe)
    ;; transformations
    (vilpy-define-key map "r" 'vilpy-raise)
    (vilpy-define-key map "R" 'vilpy-raise-some)
    (vilpy-define-key map "p" 'vilpy-move-up)
    (vilpy-define-key map "n" 'vilpy-move-down)
    (vilpy-define-key map "x" 'vilpy-splice)
    (vilpy-define-key map "+" 'vilpy-join)
    (vilpy-define-key map "C" 'vilpy-convolute)
    (vilpy-define-key map "J" 'vilpy-oneline)
    (vilpy-define-key map "M" 'vilpy-alt-multiline)
    ;; barfing & slurping
    (vilpy-define-key map ">" 'vilpy-slurp)
    (vilpy-define-key map "<" 'vilpy-barf)
    (vilpy-define-key map "s" 'vilpy-move-and-slurp-actions)
    ;; acing
    (vilpy-define-key map "f" 'vilpy-ace-symbol)
    (vilpy-define-key map "-" 'vilpy-ace-subword)
    (vilpy-define-key map "F" 'vilpy-ace-symbol-beginning-of-defun)
    (vilpy-define-key map "Q" 'vilpy-ace-char)
    (vilpy-define-key map "q" 'vilpy-ace-paren
      :override '(cond ((bound-and-true-p view-mode)
                        (View-quit))))
    ;; copying & yanking
    (vilpy-define-key map "y" 'vilpy-copy)
    (vilpy-define-key map "w" 'vilpy-clone)
    (vilpy-define-key map "P" 'vilpy-paste)
    ;; marking
    (vilpy-define-key map "m" 'vilpy-mark-list)
    ;; misc
    (vilpy-define-key map "_" 'vilpy-underscore)
    (vilpy-define-key map "g" 'vilpy-go-actions)
    (define-key map (kbd "SPC") 'vilpy-space)
    (vilpy-define-key map "u" 'vilpy-undo)
    (vilpy-define-key map "z" 'vilpy-scroll-actions)
    (vilpy-define-key map "." 'vilpy-repeat)
    ;; magic
    (vilpy-define-key map "t" 'vilpy-teleport)
    ;; TODO: other
    ;; (vilpy-define-key map "S" 'vilpy-stringify)
    ;; (vilpy-define-key map "D" 'pop-tag-mark)
    ;; (define-key map (kbd "C-8") 'vilpy-parens-down)
    ;; (define-key map (kbd "C-9") 'vilpy-out-forward-newline)
    ;; digit argument
    (mapc (lambda (x) (vilpy-define-key map (format "%d" x) 'digit-argument))
          (number-sequence 0 9))
    map))

(defvar vilpy-mode-map-base
  (let ((map (make-sparse-keymap)))
    ;; killing
    (define-key map (kbd "C-k") 'vilpy-kill)
    (define-key map (kbd "M-d") 'vilpy-kill-word)
    (define-key map (kbd "M-DEL") 'vilpy-backward-kill-word)
    ;; misc
    (define-key map (kbd ";") 'vilpy-comment)
    (define-key map (kbd "(") 'vilpy-parens)
    (define-key map (kbd "[") 'vilpy-brackets)
    (define-key map (kbd "C-j") 'vilpy-newline-and-indent)
    (define-key map (kbd "RET") 'vilpy-newline-and-indent-plain)
    map))

(declare-function View-quit "view")

(defun vilpy-special ()
  (interactive)
  (cond ((vilpy-left-p) (vilpy-other))
        ((vilpy-right-p) (vilpy-other))
        ('t (vilpy-backward 1))))

(defvar vilpy-mode-map-vilpy
  (let ((map (copy-keymap vilpy-mode-map-base)))
    ;; navigation
    (define-key map (kbd "<backtab>") 'vilpy-special)
    ;; deleting
    (define-key map (kbd "C-d") 'vilpy-delete)
    (define-key map (kbd "DEL") 'vilpy-delete-backward)
    ;; transformation
    (define-key map (kbd "M-j") 'vilpy-split)
    (define-key map (kbd "M-J") 'vilpy-join)
    ;; marking
    (define-key map (kbd "M-m") 'vilpy-mark-symbol)
    (define-key map (kbd "C-M-,") 'vilpy-mark)
    ;; insert
    (define-key map (kbd "{") 'vilpy-braces)
    (define-key map (kbd "\"") 'vilpy-quotes)
    (define-key map (kbd "'") 'vilpy-tick)
    (define-key map (kbd "`") 'vilpy-backtick)
    (define-key map (kbd "#") 'vilpy-hash)
    map))

(defcustom vilpy-key-theme '(special vilpy)
  "List of key themes used to compose `vilpy-mode-map'."
  :type
  '(set
    (const special)
    (radio
     (const vilpy))))

(defun vilpy-set-key-theme (theme)
  "Set `vilpy-mode-map' for according to THEME.
THEME is a list of choices: 'special, 'vilpy,"
  (setq vilpy-mode-map
        (make-composed-keymap
         (delq nil
               (list
                (when (memq 'special theme) vilpy-mode-map-special)
                (when (memq 'vilpy theme) vilpy-mode-map-vilpy)))))
  (setcdr
   (assq 'vilpy-mode minor-mode-map-alist)
   vilpy-mode-map))

(vilpy-set-key-theme vilpy-key-theme)

(provide 'vilpy)

;;; vilpy.el ends here
