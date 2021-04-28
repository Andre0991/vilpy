# vilpy

<p align="center">
<img src="imgs/torta-holandesa.jpg"
   alt="vilpy logo"/>
</p>

> creamy & even shorter lisp editing

**Important: Work in progress. Expect things to break and keybindings to change. I don't even recommend using this right now, but stay tuned!**

This is a stripped-down fork of the excellent [lispy](https://github.com/abo-abo/lispy), a paredit-like mode. Think of it as a `lispy-core`, which is lighter, but no less sweet. `vilpy` strives to be vimmier (though no less suited for non-Evil users) and smaller (no refactoring and debugging features, fewer external dependencies). For more differences with `lispy`, please consult the [alternatives](#alternatives) section.

I prefer forking the original code because (1) the author is happy with the current approach of bundling everything in the same package, and that's perfectly fine ([issue](https://github.com/abo-abo/lispy/issues/74)) and (2) `lispy` is critical for getting things done in my job, but the Clojure-specific parts are not important for me and they do interfere with some projects.

## Supported languages & compatibility with other modes
The navigation features are tested with emacs-lisp and Clojure, but they are likely to work with other lisps.

emacs-lisp and Clojure (`cider` and `inf-clojure`) also support evaluation, describing the symbol at point and identation.
These features can be added to other languages by setting the proper handlers in the variable `vilpy--handlers-alist`.

`vilpy` defines its own keybindings that might conflict with `parinfer`, `paredit` and other structural diting odes

## Installation
`vilpy` must be installed manually as of now.

Note that `vilpy` requires [`avy`](https://github.com/abo-abo/avy), you might need to get it as well.

``` emacs-lisp
;; replace `vilpy-load-path` by the directory that has `vilpy.el`
(let ((vilpy-path "vilpy-load-path"))
  (add-to-list 'load-path vilpy-path)
  (require 'vilpy))
(add-hook 'emacs-lisp-mode-hook (lambda () (vilpy-mode 1)))
(add-hook 'clojure-mode-hook (lambda () (vilpy-mode 1)))

;; recommended settings for `evil` users
(vilpy-define-key vilpy-mode-map "m" 'evil-set-marker)
(vilpy-define-key vilpy-mode-map "`" 'evil-goto-mark)
(vilpy-define-key vilpy-mode-map "c" 'vilpy-execute-in-normal-state)
```

## Usage (work in progress)

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

</details>

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

## Evil

`vilpy` disputes keybindings with `evil-mode`, so in some cases, its commands are overwritten.
Using [lispyville](https://github.com/noctuid/lispyville) is a typical solution for making both packages work together.
Personally, I'm fine with the default `vilpy` bindings working only in `insert-mode`.
I simply use these customizations for making `vilpy` take precedence over `evil` in some specific keys:

``` emacs-lisp
(defun raise-minor-mode (mode)
  "Make MODE the first on `minor-mode-map-alist'."
  (let ((x (assq mode minor-mode-map-alist)))
    (when x
      (setq minor-mode-map-alist
            (cons x (delq mode minor-mode-map-alist))))))
            
(raise-minor-mode 'vilpy-mode)

;; TODO: `map!` is a Doom-emacs macro - not sure how
;; to do this with vanilla Evil.
(map! :map vilpy-mode-map :i "C-d" 'vilpy-delete)
(map! :map vilpy-mode-map
      :i "C-k" 'vilpy-kill)
(map! :map vilpy-mode-map
      :i "C-y" 'vilpy-yank)
```

## Customization
This section presents some common ways that `vilpy` can be configured.

There is also the [wiki](https://github.com/Andre0991/vilpy/wiki), with additional configuration examples and tips.

### Keybindings
#### Single-key bindings
Use `vilpy-define-key` for overriding a default binding.

For calling `my-function` with the key `H`, use

``` emacs-lisp
(vilpy-define-key vilpy-mode-map "H" 'apt-vilpy-describe)
```

#### Multi-key bindings
You may wish to override keybinding that reads two characters - for example, `wo`, which goes to the other window.

There is no way to do that without modifying the whole prefix (`w`, in this example).

That said, you can simply copy the current function that lists those bindings to your init file and modify it as you wish - and there is nothing wrong with that.

For doing that, please consult `vilpy-mode-map-special` and check which function is called for the prefix you are going to change. Then, copy it to your configuration, as exemplified below.

``` emacs-lisp
(defun vilpy-window-actions ()
  (interactive)
  (cl-case (read-char-from-minibuffer "Actions:\n
o: Select other window.
h: Select left window.
\n")
    (?o (other-window 1))
    (?h (windmove-left))
    (t (vilpy--complain-unrecognized-key))))
    
(vilpy-define-key map "w" 'vilpy-window-actions)
```

#### Overring behavior
You may want to override some default behavior based on some conditions.
For example, suppose you want to change `H`, which is originally bound to `vilpy-foo`, to `my-function`, but only if `inf-clojure-minor-mode` is enabled.

You would use this:

``` emacs-lisp
(defun my-override-vilpy-describe ()
  (interactive)
  (if (bound-and-true-p inf-clojure-minor-mode)
      (call-interactively 'my-function)
    (call-interactively 'vilpy-foo)))

(vilpy-define-key vilpy-mode-map "H" 'my-override-vilpy-describe)
```

## Alternatives
### `lispy`
`vilpy` has some important differences from its big brother.

In particular, it attempts to respect the following design goals (mostly not taken into practice yet):
- Be vimmier: avoid using modifier keys and prefer composability over specialization
- Reduce the feature set to navigation and evaluation functions. We don't need debugging and tags features. Refactoring commands can be leveraged from other packages.
- Prefer to leverage Emacs built-in functions whenever possible.
- Implement a uniform API that works well with all supported languages. Alternatively: avoid language-specific commands.
- Rely on fewer external dependencies.
- Drop support for non-lisps languages.
- Use less magic. For example, in the original `lispy`, the function for evaluating the last sexp also uses `setq` when evaluating `defvars`. `vilpy` does not try to guess your intent. `vilpy` is intentionally dumber.
- For evaluation features, just depend on public functions from external packages. For example, `lispy` loads `le-clojure.clj` when you use `cider` for providing some features such as getting bindings from `let` when evaluating forms. This relies on injecting dependencies to `cider` and will not work on some project setups (see https://github.com/abo-abo/lispy/issues/552). `vilpy` simple uses `cider-eval*` functions.

That said, `lispy` has more features, including debugging and refactoring capabilities and support more languages. If you want a more featureful package, go for it.
