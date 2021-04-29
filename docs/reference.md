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

After `A`:

```
(foo|)
```

However, if the point is not at a position that activates special-mode, pressing `A` will self-insert the letter `A`, as usual.

```
(|foo)
```

After `A`:
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

#### `vilpy-knight-down`
#### `vilpy-knight-up`

</details>

### Code actions
| command            | binding      |
|--------------------|--------------|
| `vilpy-eval`       | <kbd>e</kbd> |
| `vilpy-eval-defun` | <kbd>E</kbd> |
| `vilpy-tab`        | <kbd>=</kbd> |
| `vilpy-describe`   | <kbd>K</kbd> |

### Transformation
| command                        | binding      |
|--------------------------------|--------------|
| `vilpy-raise`                  | <kbd>r</kbd>  |
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

