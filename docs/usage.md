# Usage

In all the following examples, the character `|` denotes the point.

## Example 1: Navigation, deletion, evaluation and basic transformation

### Commands

| command                    | binding            |
|----------------------------|--------------------|
| `vilpy-special`            | <kbd>backtab</kbd> |
| `vilpy-ace-paren`          | <kbd>q</kbd>       |
| `vilpy-ace-subword`        | <kbd>-</kbd>       |
| `vilpy-ace-symbol`         | <kbd>f</kbd>       |
| `vilpy-beginning-of-defun` | <kbd>I</kbd>       |
| `vilpy-delete`             | <kbd>C-d</kbd>     |
| `vilpy-down`               | <kbd>j</kbd>       |
| `vilpy-eval-buffer`        | <kbd>B</kbd>       |
| `vilpy-eval`               | <kbd>e</kbd>       |
| `vilpy-step-in`            | <kbd>l</kbd>       |

### Code

We will start with the following snippet:

```
(defun plus-one (n)
  (+ 1 n)
  (message "debug"))

(plus-one 2)
```

This will be the final result:

```
(def plus-two (n)
  (+ 2 n))
```

### Steps

We start with this code. Note that the point is in the middle of `defun`.

```
(de|fun plus-one (n)
  (+ 1 n)
  (message "done"))

(plus-one 2)
```

**Evaluating `(plus-one 2)`**

- Press <kbd>backtab</kbd> (that is, <kbd>shift</kbd> and <kbd>tab</kbd>) to move the point to the beginning of the defun and enter special mode.
- First, evaluate the defun with <kbd>e</kbd> (`vilpy-eval`).
- Then, use <kbd>j</kbd> (`vilpy-down`) and <kbd>e</kbd> for evaluating `(plus-one 2)` and getting 3.
- Use <kbd>k</kbd> for getting back to the defun. Note that this entire sequence is equivalent to simply using <kbd>B</kbd> (`vilpy-eval-buffer`).

**Change `plus-one` to `plus-two`**

- With the point in the beginning of the defun, press <kbd>-</kbd> (`vilpy-ace-subword`). This will display a letter in each subword, alphabetically, like `(adefun bplus-cone ...)`. Press `c` for jumping to `one`, which will be marked.
- Then, use <kbd>C-d</kbd> (`vilpy-delete`) for removing `one` and write `two`.
- Press <kbd>backtab</kbd> for getting back to special mode.

**Remove `message`**

- Use  <kbd>l</kbd> (`vilpy-step-in`) three times for getting to the `message` form. This is equivalent to <kbd>3l</kbd>. Alternatively, use <kbd>q</kbd> (`vilpy-ace-paren`) and `d` for directly jumping to `message`.
- Then, press <kbd>C-d</kbd> (`vilpy-delete`) twice for removing the form and the blank line. The point will jump to the beginning of the defun.

**Change `(+ 1 n)` to `(+ 2 n)`**

- Press <kbd>f</kbd> (`vilpy-ace-symbol`). This command will add a letter before each symbol. Use `e` for directly jumping to `1`.
- Use <kbd>C-d</kbd> for deleting it and write `2`.
- Use <kbd>backtab</kbd> for getting back to special mode.
- <kbd>I</kbd> (`vilpy-beginning-of-defun`) for jumping to the beginning of the defun.
- <kbd>e</kbd> for evaluating it.

**Deleting `(plus-one 2)`**

- Finally, press <kbd>j</kbd> for moving the point to `(plus-one 2)`.
- <kbd>C-d</kbd> for deleting it.


## Example 2: Killing, copying and yanking

### Commands
| command                 | binding            |
|-------------------------|--------------------|
| `vilpy-step-in`         | <kbd>l</kbd>       |
| `vilpy-clone`           | <kbd>w</kbd>       |
| `vilpy-down`            | <kbd>j</kbd>       |
| `vilpy-ace-subword`     | <kbd>-</kbd>       |
| `vilpy-special`         | <kbd>backtab</kbd> |
| `vilpy-up`              | <kbd>k</kbd>       |
| `forward-char`          | <kbd>a</kbd>       |
| `vilpy-space`           | <kbd>SPC</kbd>     |
| `vilpy-kill`            | <kbd>C-k</kbd>     |
| `vilpy-step-out`        | <kbd>h</kbd>       |
| `vilpy-open-line-below` | <kbd>o</kbd>       |

### Code

We will starting with this snippet of code:

```
(defun foo
  ()
  (do-bar)
  (do-baz (a) (b) (c)))
```

This will be the result:

```
(defun foo
  ()
  (with-foo do-bar)
  (do-foo)
  (do-baz (a))
  (a)))
```

### Steps

Consider that the point starts just before `defun`, in special mode.

**Insert `(do-foo)`**

- First, let's insert `(do-foo)` by duplicating `(do-bar)` and replacing `bar`.
- Use <kbd>l</kbd> (`vilpy-step-in`) twice for getting to `do-bar`. This is equivalent to <kbd>2l</kbd> or <kbd>qc</kbd> (`vilpy-ace-paren` followed by `c`).
- Then, press <kbd>w</kbd> (`vilpy-clone`), which will duplicate `(do-foo)`.
- Use <kbd>j</kbd> (`vilpy-down`) for moving to the second `(do-bar)`.
- Press <kbd>-b</kbd> for marking `bar` and delete it with <kbd>C-d</kbd>. Type `foo`
- Use <kbd>backtab</kbd> for getting back to special mode (that is, <kbd>shift</kbd> and <kbd>tab</kbd>).

**Change `(do-bar)` to `(with-foo do-bar)`**

- Use <kbd>k</kbd> (`vilpy-up`) for getting to `do-bar`.
- Then, press <kbd>a</kbd> (`forward-char`).This is equivalent to the usual `C-f` Emacs binding. The point will be just after the paren: `(|do-bar)`.
- Press <kbd>SPC</kbd> (`vilpy-space`) for inserting a whitespace character and type `with-foo`.
- Use <kbd>backtab</kbd> (`vilpy-special`) for getting back to special mode. The point will be just before the paren, like this: `|(with-foo do-bar)`.

**Remove `(b)` and `(c)` from `do-baz`**

- Press <kbd>j</kbd> for getting to `do-baz`.
- Use <kbd>l</kbd> twice for getting to `(b)`.
- Then, use <kbd>C-k</kbd> (`vilpy-kill`) for killing until the end of the parent sexp.

**Copy `(a)` to the line below**

- Use <kbd>backtab</kbd> for getting back to special mode.
- <kbd>y</kbd> for copying `(a)`.
- Press <kbd>h</kbd> (`vilpy-step-out`) for getting to the paren sexp.
- Use <kbd>backtab</kbd> for switching to the matching paren and <kbd>RET</kbd> for opening a new line. This is equivalent to just using <kbd>o</kbd> (`vilpy-open-line-below`).
- Finally, press <kbd>C-y</kbd> (the standard `yank` command) for pasting the previously copied sexp.
