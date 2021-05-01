## Function reference (work in progress)

### Special mode
`vilpy` commands operate when special mode is active.

If you know `vi`, think of special mode as `vilpy`'s normal mode.

<details>
Special-mode is activated when:

- the point is before an open paren: `(`, `[` or `{`
- the point is after a close paren: `)`, `]` or `}`
- the region is active

In the examples below, consider that the point is represented by `|`.

As the point is just before the parenthesis, keys will invoke `vilpy` commands.
If you press `A`, for example, it will call `vilpy-insert-at-end-of-sexp`.

```
|(foo)
```

After <kbd>A</kbd>:

```
(foo|)
```

However, if the point is not at a position that activates special-mode, pressing `A` will self-insert the letter `A`, as usual.

```
(|foo)
```

After <kbd>A</kbd>:
```
(A|foo)
```
</details>

#### Getting in special mode
| command         | binding            |
|-----------------|--------------------|
| `vilpy-special` | <kbd>backtab</kbd> |

Note that <kbd>backtab</kbd> is <kbd>shift</kbd> and <kbd>tab</kbd>.

<details>

**If not in special mode** 

Move the point to the nearest leftmost paren.

Starting with:

```
(foo |bar)
```

after <kbd>backtab</kbd>:

```
|(foo bar)
```

**If already in special mode** 

Cycle through parens.

Starting with:

```
|(foo bar)
```

after <kbd>backtab</kbd>:

```
(foo bar)|
```

after <kbd>backtab</kbd>:

```
|(foo bar)
```

</details>

#### Getting out of special mode

| command                       | binding        |
|-------------------------------|----------------|
| `forward-char`                | <kbd>a</kbd>   |
| `vilpy-insert-at-end-of-sexp` | <kbd>A</kbd>   |
| `vilpy-open-parens-below`     | <kbd>o</kbd>   |
| `vilpy-open-parens-above`     | <kbd>O</kbd>   |
| `backward-char`               | <kbd>C-b</kbd> |
| `forward-char`                | <kbd>C-f</kbd> |

<details>

##### `forward-char` (<kbd>a</kbd>)
Starting with

```
|(foo)
```

after <kbd>a</kbd>:

```
(|foo)
```

##### `vilpy-insert-at-end-of-sexp` (<kbd>A</kbd>)
Starting with

```
|(foo bar)
```

after <kbd>A</kbd>:
```
(foo bar|)
```

##### `vilpy-open-parens-below` (<kbd>o</kbd>)
Starting with

```
|(foo)
```

after <kbd>o</kbd>:
```
(foo)
(|)
```

##### `vilpy-open-parens-above` (<kbd>O</kbd>)
Starting with

```
|(foo)
```

after <kbd>O</kbd>:
```
(|)
(foo)
```

</details>

### Navigation
| command                    | binding      |
|----------------------------|--------------|
| `vilpy-step-out`           | <kbd>h</kbd> |
| `vilpy-step-in`            | <kbd>l</kbd> |
| `vilpy-down`               | <kbd>j</kbd> |
| `vilpy-up`                 | <kbd>k</kbd> |
| `vilpy-beginning-of-defun` | <kbd>B</kbd> |
| `vilpy-knight-down`        | <kbd>S</kbd> |
| `vilpy-knight-up`          | <kbd>W</kbd> |
| `vilpy-beginning-of-defun` | <kbd>B</kbd> |
| `vilpy-back`               | <kbd>b</kbd> |
| `vilpy-right`              | <kbd>L</kbd> |

<details>

#### `vilpy-step-out` (<kbd>h</kbd>)

Starting with

```
(foo (bar |(baz))
```

after <kbd>h</kbd>:

```
(foo |(bar (baz))
```

after <kbd>h</kbd>:

```
|(foo (bar (baz))
```

#### `vilpy-step-in` (<kbd>l</kbd>)

Starting with

```
|(foo (bar (baz))
```

after <kbd>l</kbd>:

```
(foo |(bar (baz))
```

after <kbd>l</kbd>:

```
(foo (bar |(baz))
```

#### `vilpy-down` (<kbd>j</kbd>)

Starting with:

```
|(foo)
(bar)
```

after <kbd>j</kbd>:

```
(foo)
|(bar)
```

#### `vilpy-up` (<kbd>k</kbd>)

Starting with:

```
(foo)
|(bar)
```

after <kbd>k</kbd>:

```
|(foo)
(bar)
```

#### `vilpy-knight-down` (<kbd>S</kbd>)
Navigate to the next line disregarding syntax.

Starting with:

```
|(foo (bar)
      (xum))
```

after <kbd>S</kbd>:

```
(foo (bar)
     |(xum))
```

#### `vilpy-knight-up` (<kbd>W</kbd>)
Navigate to the previous line disregarding syntax.

Starting with:

```
(foo (bar)
     |(xum))
```

after <kbd>W</kbd>:

```
|(foo (bar)
      (xum))
```
#### `vilpy-beginning-of-defun` (<kbd>B</kbd>)

Starting with this top level form:

```
(defun abc ()
  (interactive)
  |(foo))
```

after <kbd>B</kbd>as

```
|(defun abc ()
  (interactive)
  (foo))
```

#### `vilpy-back` (<kbd>b</kbd>)
Moves the point to the previous position in `vilpy-back` history. The following functions write to this history:
function name

| command           | binding      |
|-------------------|--------------|
| `vilpy-step-out`  | <kbd>h</kbd> |
| `vilpy-step-in`   | <kbd>l</kbd> |
| `vilpy-down`      | <kbd>j</kbd> |
| `vilpy-up`        | <kbd>k</kbd> |
| `vilpy-right`     | <kbd>L</kbd> |
| `vilpy-mark-list` | <kbd>v</kbd> |
| `vilpy-ace-paren` | <kbd>q</kdb> |

#### `vilpy-right` (<kbd>L</kbd>)

Moves forward out of arg (default 1) levels of parentheses.

Works as replacement for the standard `up-list`.

Takes a numeric prefix arg and moves up forward list arg times or until error.

Unlike up-list, no error will be reported if it's not possible to move up arg times. It that case, move as many times as possible.

Return point if could move arg times, otherwise return nil.

Unlike up-list, parens in strings and comments are ignored.

</details>

### Code actions
| command            | binding      |
|--------------------|--------------|
| `vilpy-eval`       | <kbd>e</kbd> |
| `vilpy-eval-defun` | <kbd>E</kbd> |
| `vilpy-tab`        | <kbd>=</kbd> |
| `vilpy-describe`   | <kbd>K</kbd> |

#### `vilpy-eval` (<kbd>e</kbd>)
Eval current region or sexp.

Emacs Lisp and Clojure (`cider` and `inf-clojure`) are supported.

The evaluation function is defined in `vilpy--handlers-alist`.

#### `vilpy-eval-defun` (<kbd>E</kbd>)
Eval top level form.

Emacs Lisp and Clojure (`cider` and `inf-clojure`) are supported.

The evaluation function is defined in `vilpy--handlers-alist`.

#### `vilpy-tab` (<kbd>=</kbd>)
If before left paren or after right paren, indent the current sexp.

Emacs Lisp and Clojure (`cider` and `inf-clojure`) are supported.

The indentation function is defined in `vilpy--handlers-alist`.

#### `vilpy-describe` (<kbd>K</kbd>)
Describe the symbol at point.

Emacs Lisp and Clojure (`cider` and `inf-clojure`) are supported.

The function for describing the symbol is defined in `vilpy--handlers-alist`.

### Transformation

| command                        | binding      |
|--------------------------------|--------------|
| `vilpy-raise`                  | <kbd>r</kbd> |
| `vilpy-raise-some`             | <kbd>R</kbd> |
| `vilpy-move-up`                | <kbd>p</kbd> |
| `vilpy-move-down`              | <kbd>n</kbd> |
| `vilpy-slurp`                  | <kbd>></kbd> |
| `vilpy-barf`                   | <kbd><</kbd> |
| `vilpy-move-and-slurp-actions` | <kbd>/</kbd> |
| `vilpy-splice`                 | <kbd>x</kbd> |
| `vilpy-join`                   | <kbd>+</kbd> |
| `vilpy-convolute`              | <kbd>C</kbd> |
| `vilpy-convolute-left`         | <kbd>X</kbd> |
| `vilpy-oneline`                | <kbd>J</kbd> |
| `vilpy-alt-multiline`          | <kbd>M</kbd> |
| `vilpy-teleport`               | <kbd>t</kbd> |

#### `vilpy-raise` (<kbd>r</kbd>)
Starting with

```
(foo |(bar))
```

after <kbd>r</kbd>:

```
(bar)
```
#### `vilpy-raise-some` (<kbd>R</kbd>)

Starting with:

```
(foo
  |(bar)
  (xum))
```

after <kbd>R</kbd>:

```
(bar)
(xum)
```

#### `vilpy-move-up` (<kbd>p</kbd>)

Starting with:

```
(foo)
|(bar)
```

after <kbd>p</kbd>:

```
|(bar)
(foo)
```

#### `vilpy-move-down`(<kbd>n</kbd>)

Starting with:

```
|(foo)
(bar)
```

after <kbd>n</kbd>:

```
(bar)
|(foo)
```

#### `vilpy-slurp`(<kbd>></kbd>)

Starting with:

```
(foo)| (bar) (xum)
```

after <kbd>></kbd>:

```
(foo (bar))| (xum)
```

after <kbd>></kbd>:

```
(foo (bar) (xum))
```

#### `vilpy-barf` (<kbd><</kbd>)

Starting with:

```
(foo) (bar) |(xum)
```

after <kbd><</kbd>:

```
(foo) |((bar) xum)
```

after <kbd><</kbd>:

```
|((foo) (bar) xum)
```

#### `vilpy-move-and-slurp-actions`(<kbd>/</kbd>)

Groups some less frequent slurping actions.

| command | binding            |
|---------|--------------------|
| h       | `vilpy-move-left`  |
| l       | `vilpy-move-right` |
| j       | `vilpy-down-slurp` |
| k       | `vilpy-up-slurp`   |


##### `vilpy-move-left` (<kbd>/h</kbd>)

Move current expression to the left, outside the current list.

```
(require 'ob-python)
(let ((color "Blue"))
  |(message "What... is your favorite color?")
  (message "%s. No yel..." color))
```

after <kbd>/h</kbd>:

```
(require 'ob-python)
|(message "What... is your favorite color?")
(let ((color "Blue"))
  (message "%s. No yel..." color))
```

##### `vilpy-move-right` (<kbd>/l</kbd>)

Move current expression to the right, outside the current list.

```
(require 'ob-python)
(message "What... is your favorite color?")
(let ((color "Blue"))
  (message color)
  |(message "Go on. Off you go."))
```

after <kbd>/l</kbd>:

```
(require 'ob-python)
(message "What... is your favorite color?")
(let ((color "Blue"))
  (message color))
|(message "Go on. Off you go.")
```

##### `vilpy-down-slurp` (<kbd>/j</kbd>)
Move current expression to become the first element of the first list below.

```
(list 'my-sword
      'my-bow)
|(my-axe)
```

after <kbd>/j</kbd>:

```
'(|(first!)
  foo bar)
```

#### `vilpy-splice`(<kbd>x</kbd>)

Starting with:

```
(foo |(bar))
```

after <kbd>x</kbd>:

```
(foo bar)
```

#### `vilpy-join`(<kbd>+</kbd>)

Starting with:

```
(foo)
|(bar)
```

after <kbd>+</kbd>:

```
|(foo
bar)
```

#### `vilpy-convolute`(<kbd>C</kbd>)
#### `vilpy-convolute-left`(<kbd>X</kbd>)
#### `vilpy-oneline`(<kbd>J</kbd>)
#### `vilpy-alt-multiline`(<kbd>M</kbd>)
#### `vilpy-teleport`(<kbd>t</kbd>)

### Acing
| command                               | binding      |
|---------------------------------------|--------------|
| `vilpy-ace-symbol`                    | <kbd>f</kbd> |
| `vilpy-ace-subword`                   | <kbd>-</kbd> |
| `vilpy-ace-symbol-beginning-of-defun` | <kbd>F</kbd> |
| `vilpy-ace-char`                      | <kbd>Q</kbd> |
| `vilpy-ace-pare`                      | <kbd>q</kbd> |
	
### Yanking
| command          | binding      |
|------------------|--------------|
| `vilpy-new-copy` | <kbd>y</kbd> |
| `vilpy-clone`    | <kbd>w</kbd> |
| `vilpy-kill`     | <kbd>D</kbd> |
| `vilpy-delete`   | <kbd>d</kbd> |
| `vilpy-paste`    | <kbd>P</kbd> | 

