# Usage

In all the following examples, the character `|` denotes the point.

## Example 1: Navigation, evaluation and basic transformation

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

Press <kbd>backtab</kbd> (that is, <kbd>shift</kbd> and <kbd>tab</kbd>) to move the point to the beginning of the defun and enter special mode.

```
|(defun plus-one (n)
  (+ 1 n)
  (message "done"))

(plus-one 2)
```

Let's eval `(plus-one 2)` and see what's the current value. First, evaluate the defun with <kbd>e</kbd> (`vilpy-eval`). Then, use <kbd>j</kbd> (`vilpy-down`) and <kbd>e</kbd> for evaluating `(plus-one 2)` and getting 3. Use <kbd>k</kbd> for getting back to the defun. Note that this entire sequence is equivalent to simply using <kbd>B</kbd> (`vilpy-eval-buffer`).

Now, let's change `plus-one` to `plus-two`. With the point in the beginning of the defun, press <kbd>-</kbd> (`vilpy-ace-subword`).
This will display a letter in each subword, alphabetically, like `(adefun bplus-cone ...)`. Press `c` for jumping to `one`, which will be marked. Then, use <kbd>C-d</kbd> (`vilpy-delete`) for removing `one` and write `two`. Press <kbd>backtab</kbd> for getting back to special mode.

Let's remove `message`. Use  <kbd>l</kbd> three times for getting to the `message` form. This is equivalent to <kbd>3l</kbd>. Alternatively, use <kbd>q</kbd> (`vilpy-ace-paren`) and `d` for directly jumping to `message`. Then, press <kbd>C-d</kbd> (`vilpy-delete`) twice for removing the form and the blank line. The point will jump to the beginning of the defun.

Now, let's change `(+ 1 n)` to `(+ 2 n)`. Press <kbd>f</kbd> (`vilpy-ace-symbol`). This command will add a letter before each symbol. Use `e` for directly jumping to `1`. Use <kbd>C-d</kbd> for deleting it and write `2`. Use <kbd>backtab</kbd> for getting back to special mode and <kbd>I</kbd> (`vilpy-beginning-of-defun`) for jumping to the beginning of the defun and <kbd>e</kbd> for evaluating it.

Finally, press <kbd>j</kbd> for moving the point to `(plus-one 2)` and <kbd>C-d</kbd> for deleting it.
