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
| `vilpy-open-line-below`       | <kbd>o</kbd>   |
| `vilpy-open-line-above`       | <kbd>O</kbd>   |
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

##### `vilpy-open-line-below` (<kbd>o</kbd>)
Starting with

```
|(foo)
```

after <kbd>o</kbd>:
```
(foo)
|
```

##### `vilpy-open-line-above` (<kbd>O</kbd>)
Starting with

```
|(foo)
```

after <kbd>O</kbd>:
```
|
(foo)
```

</details>

### Navigation
| command                    | binding       |
|----------------------------|---------------|
| `vilpy-step-out`           | <kbd>h</kbd>  |
| `vilpy-step-in`            | <kbd>l</kbd>  |
| `vilpy-down`               | <kbd>j</kbd>  |
| `vilpy-up`                 | <kbd>k</kbd>  |
| `vilpy-knight-down`        | <kbd>S</kbd>  |
| `vilpy-knight-up`          | <kbd>W</kbd>  |
| `vilpy-beginning-of-defun` | <kbd>I</kbd>  |
| `vilpy-back`               | <kbd>b</kbd>  |
| `vilpy-right`              | <kbd>L</kbd>  |
| `vilpy-go-to-first-defun`  | <kbd>gg</kbd> |
| `vilpy-go-to-last-defun`   | <kbd>G</kbd>  |

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
#### `vilpy-beginning-of-defun` (<kbd>I</kbd>)

Starting with this top level form:

```
(defun abc ()
  (interactive)
  |(foo))
```

after <kbd>I</kbd>as

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

#### `vilpy-go-to-first-defun` (<kbd>gg</kbd>)
Starting with

```
(a)
(b |(c))
```

after <kbd>gg</kbd>:

```
|(a)
(b (c))
```

#### `vilpy-go-to-last-defun` (<kbd>G</kbd>)
Starting with

```
(a |(b))
(c)
```

after <kbd>G</kbd>:

```
(a (b))
|(c)
```

</details>

### Code actions
| command             | binding      |
|---------------------|--------------|
| `vilpy-eval`        | <kbd>e</kbd> |
| `vilpy-eval-defun`  | <kbd>D</kbd> |
| `vilpy-eval-buffer` | <kbd>B</kbd> |
| `vilpy-tab`         | <kbd>=</kbd> |
| `vilpy-describe`    | <kbd>K</kbd> |

<details>

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

</details>

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
| `vilpy-oneline`                | <kbd>J</kbd> |
| `vilpy-alt-multiline`          | <kbd>M</kbd> |
| `vilpy-teleport`               | <kbd>t</kbd> |

<details>

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

Starting with:

```
(foo
 (bar
  |(xum)))
```

after <kbd>C</kbd>:

```
(bar
 (foo
  (xum)))
```

after <kbd>C</kbd>:


```
(foo
 (bar
  |(xum)))
```

#### `vilpy-oneline`(<kbd>J</kbd>)

Starting with:

```
|(foo
 (bar
  (xum)))
```

after <kbd>J</kbd>:

```
(foo (bar (xum)))
```

#### `vilpy-split` (<kbd>M-j</kbd>)
#### `vilpy-join` (<kbd>M-J</kbd>)

</details>

### Barfage & Slurpage

| command            | binding       |
|--------------------|---------------|
| `vilpy-slurp`      | <kbd>></kbd>  |
| `vilpy-barf`       | <kbd><</kbd>  |
| `vilpy-move-left`  | <kbd>/h</kbd> |
| `vilpy-move-right` | <kbd>/l</kbd> |
| `vilpy-down-slurp` | <kbd>/j</kbd> |
| `vilpy-up-slurp`   | <kbd>/k</kbd> |

<details>

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

</details>

### Acing
| command                               | binding      |
|---------------------------------------|--------------|
| `vilpy-ace-symbol`                    | <kbd>f</kbd> |
| `vilpy-ace-subword`                   | <kbd>-</kbd> |
| `vilpy-ace-symbol-beginning-of-defun` | <kbd>F</kbd> |
| `vilpy-ace-paren`                     | <kbd>q</kbd> |
| `vilpy-ace-char`                      | <kbd>Q</kbd> |

<details>

#### `vilpy-ace-symbol` (<kbd>f</kbd>)
Marks symbol in the current form.
This can be followed up with eval, raise, deletion etc.
For unmarking the symbol afterwards, as usual, press <kbd>C-g</kbd>.

Starting with:

```
(foo bar baz)
```

After <kbd>f</kbd>, each symbol will be annotated with a character:

```
(afoo bbar cbaz)
```

In this example, say you press `c`. Then, the cursor will jump to `baz` and it will be marked.

#### `vilpy-ace-subword` (<kbd>-</kbd>)

Marks subword.

Starting with:

```
(foo-bar-baz)
```

After <kbd>-</kbd>:

```
(afoo-bbaz-cbaz)
```

In this example, use `a`, `b` or `c` for jumping and marking a subword.

#### `vilpy-ace-symbol-beginning-of-defun` (<kbd>F</kbd>)

Same as `vilpy-ace-symbol`, but the range of acing is the current defun rather than the current form.

#### `vilpy-ace-paren` (<kbd>q</kbd>)

Marks form.

Starting with:

```
(a (b) (c) d)
```

After <kbd>q</kbd>:

```
a((bb) (cc) d)
```

In this example, use `a`, `b` or `c` for jumping and marking a form.

#### `vilpy-ace-char` (<kbd>Q</kbd>)

Asks for a char and jumps to it in the current form.



</details>

### Deleting & killing
| command                 | binding          |
|-------------------------|------------------|
| `vilpy-delete`          | <kbd>C-d</kbd>   |
| `vilpy-kill`            | <kbd>C-k</kbd>   |
| `vilpy-kill-word`       | <kbd>M-DEL</kbd> |
| `vilpy-delete-backward` | <kbd>DEL</kbd>   |

<details>

#### `vilpy-delete` (<kbd>C-d</kbd>)

Deletes region, form or string, depending on the position of the point. Accepts numerical argument.

For the complete list of behaviours, please refer to the `lispy` [documentation](http://oremacs.com/lispy/#lispy-delete).

**Example 1: In region**

Starting with (`~` denotes a marked region):

```
(~foo~)
```

After <kbd>C-d</kbd>:

```
()
```

**Example 2: Before form**

Starting with

```
|(foo) (bar)
```

after <kbd>C-d</kbd>:

```
|(bar)
```

#### `vilpy-kill` (<kbd>C-k</kbd>)

Similar to `kill-line`, but keeps parens balanced.

**Example 1: Before form**

Starting with:

```
(foo |(bar) (baz))
```

after <kbd>C-k</kbd>:

```
(foo )
```

**Example 2: In string**

Starting with:

```
"foo |bar"
```

after <kbd>C-k</kbd>:

```
"foo "
```

#### `vilpy-kill-word` (<kbd>M-DEL</kbd>)

Kill words. Accepts numerical argument.

Starting with:

```
|(foo bar)
```

After <kbd>M-DEL</kbd>:

```
(| bar)
```

After <kbd>M-DEL</kbd>:

```
(|)
```

#### `vilpy-delete-backward` (<kbd>DEL</kbd>)

Bound to <kbd>DEL</kbd>.

Replaces `backward-delete-char`, keeping parens balanced.

The result depends on the following conditions, each tried one by one until one that holds true is found:

**Active region**

Delete region.

**At first char of the string**

Move to the end of the string. This allows to delete the whole string with the next DEL.

```
(message "|more gold is required")
```

after <kbd>DEL</kbd>:

```
(message "more gold is required"|)
```

**In string near `\\(` or `\\)`**

Remove `\\(` and `\\)`.

Starting with:
```
(looking-at "\\([a-z]+\\)|")
```

After <kbd>DEL</kbd>:

```
(looking-at "[a-z]+")

```

Starting with:

```
(looking-at "\\(|[a-z]+\\)")
```

after <kbd>DEL</kbd>:

```
(looking-at "|[a-z]+")
```

**In string or comment**

Call `backward-delete-char`.

**After right paren**

Delete arg sexps.

Starting with:

```
(foo (bar) (baz)|)
```

after <kbd>2 DEL</kbd>:

```
(foo)
```

**After left paren**

Delete containing sexp.

```
(foo (|bar) (baz))
```

After <kbd>DEL</kbd>:

```
(foo| (bar))
```

**After a string**

Delete string.

```
(message "more gold is required"|)
```

After <kbd>DEL</kbd>:

```
(message |)
```

**Otherwise**

Call `backward-delete-char`.

</details>

### Copying & yanking

| command       | binding      |
|---------------|--------------|
| `vilpy-copy`  | <kbd>y</kbd> |
| `vilpy-clone` | <kbd>w</kbd> |
| `vilpy-paste` | <kbd>P</kbd> |

<details>

#### `vilpy-copy`(<kbd>y</kbd>)

Copy the current sexp or region to kill ring.

#### `vilpy-clone` (<kbd>w</kbd>)

Copy current sexp or region and paste it below.
With a numerical argument, copy that many times.

Starting with:

```
|(foo)
```

after <kbd>w</kbd>:

```
|(foo)
(foo)
```

#### `vilpy-paste` (<kbd>P</kbd>)

When region is active, replace it with current kill. Otherwise, forward to yank.

</details>

### Marking
| command             | binding          |
|---------------------|------------------|
| `vilpy-mark-list`   | <kbd>m</kbd>     |
| `vilpy-mark-symbol` | <kbd>M-m</kbd>   |
| `vilpy-mark`        | <kbd>C-M-,</kbd> |

<details>

#### `vilpy-mark-list` (<kbd>m</kbd>)

Mark the current sexp. When the mark is already active, deactivate it instead.

#### `vilpy-mark-symbol` (<kbd>M-m</kbd>)

Marks the symbol at point, comment or symbol in the next or previous list.

**Point is under symbol**

Mark the symbol.

**In comment**

Mark the comment.

**In special mode, before paren**

Marks the next symbol.

**In special mode, after paren**

Marks the previous symbol.

**Region is active**

Call `forward-sexp`.

**Otherwise**

Forward to `lispy-mark`.

#### `vilpy-mark` (<kbd>C-M-,</kbd>)

Mark the smallest comment, string or sexp that includes point.

</details>

### Misc
| command                       | binding        |
|-------------------------------|----------------|
| `vilpy-comment`               | <kbd>;</kbd>   |
| `vilpy-space`                 | <kbd>SPC</kbd> |
| `vilpy-narrow`                | <kbd>gn</kbd>  |
| `vilpy-widen`                 | <kbd>gw</kbd>  |
| `vilpy-undo`                  | <kbd>u</kbd>   |
| `vilpy-scroll-line-to-top`    | <kbd>zt</kbd>  |
| `vilpy-scroll-line-to-center` | <kbd>zz</kbd>  |
| `vilpy-scroll-line-to-bottom` | <kbd>zb</kbd>  |
| `vilpy-repeat`                | <kbd>.</kbd>   |

### Magic
#### `vilpy-teleport`(<kbd>t</kbd>)
