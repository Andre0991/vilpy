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

### Evaluation
| command            | binding      |
|--------------------|--------------|
| `vilpy-eval`       | <kbd>e</kbd> |
| `vilpy-eval-defun` | <kbd>E</kbd> |

### Transformation
