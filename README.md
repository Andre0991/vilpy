# vilpy

<p align="center">
<img src="imgs/torta-holandesa.jpg"
   alt="vilpy logo"/>
</p>

> creamy & even shorter lisp editing

<!-- For regenerating the table of contents, run: -->
<!-- gh-md-toc README.md -->

* [vilpy](#vilpy)
  * [Differences with lispy](#differences-with-lispy)
    * [Keybinding changes](#keybinding-changes)
    * [Behavior changes](#behavior-changes)
      * [lispy\-eval (e)](#lispy-eval-e)
      * [lispy\-eval\-and\-insert (E)](#lispy-eval-and-insert-e)
      * [lispy\-follow (F)](#lispy-follow-f)
      * [lispy\-goto (g)](#lispy-goto-g)
      * [lispy\-tab (i)](#lispy-tab-i)
    * [Removed functions](#removed-functions)
    * [Other](#other)
  * [Installation](#installation)
    * [with load\-path](#with-load-path)
    * [with use\-package](#with-use-package)
  * [Usage](#usage)
  * [TODO: Special mode](#todo-special-mode)
  * [TODO: Navigation](#todo-navigation)
  * [TODO: Transformation](#todo-transformation)
    * [Evil](#evil)
  * [Customization](#customization)
    * [Keybindings](#keybindings)
      * [Single\-key bindings](#single-key-bindings)
      * [Multi\-key bindings](#multi-key-bindings)
      * [Overring behavior](#overring-behavior)
  * [Alternatives](#alternatives)
    * [lispy](#lispy)

**Important: Work in progress. Expect things to break.**

This is a stripped-down fork of the excellent [lispy](https://github.com/abo-abo/lispy), a paredit-like mode. Think of it as a `lispy-core`, which is lighter, but no less sweet. `vilpy` strives to be vimmier (though no less suited for non-Evil users) and smaller (no refactoring and debugging features, fewer external dependencies). For a complete description, please consult the [alternatives](#alternatives) section.

I prefer forking the original code because (1) the author is happy with the current approach of bundling everything in the same package, and that's perfectly fine ([issue](https://github.com/abo-abo/lispy/issues/74)) and (2) `lispy` is critical for getting things done in my job, but the Clojure-specific parts are not important for me and they do interfere with some projects.

## Supported languages
The navigation features are tested with `emacs-lisp` and `clojure`, but they are likely to work with other lisps.

`emacs-lisp` and `Clojure` (`cider` and `inf-clojure`) also support evaluation, describing the symbol at point and identation.
These features can be added to other languages by setting the proper handlers in the variable `vilpy--handlers-alist`.

## Differences with `lispy`

### Keybinding changes
The new bindings are still changing, so they are not listed here yet. For the time being, please consult `vilpy-mode-map-special`.

### Behavior changes

#### `lispy-eval` (e)
Calls `eval-last-sexp` (`emacs-lisp-mode`), `cider-eval-last-sexp` (`cider-mode`) or `inf-clojure-eval-last-sexp` (`inf-clojure-minor-mode`) rather than directly using the cider middleware functions.

#### `lispy-eval-and-insert` (E)
Removed.
New command added to the same binding: `vilpy-eval-defun`.

#### `lispy-follow` (F)
Uses `xref-find-definitions`.

#### `lispy-goto` (g)
Uses `imenu`.

You may want to call something fancier:

``` emacs-lisp
(vilpy-define-key vilpy-mode-map "g" 'counsel-imenu)
```

#### `lispy-tab` (i)
Does not deal with outlines.
In `clojure-mode`, only calls `clojure-align` and trims whitespace at beginning of line.

### Removed functions
- `vilpy-goto-projectile` (0g and ogp).
- `vilpy-goto-elisp-commands` (oge)
- `vilpy-goto-local` (G)
- `lispy-goto-recursive` (ogr)
- `lispy-goto-def-down`
- `lispy-goto-def-ace`
- `lispy-debug-step-in` (xj)
- `lispy-goto-symbol-elisp`
- `lispy-goto-symbol` (M-.)
- `lispy-shifttab` (I)
- `lispy-iedit` (M-i)
- `lispy-bind-variable` (xb)
- `lispy-unbind-variable` (xu)
- `lispy-to-defun` (xd)
- `lispy-extract-block` (xk)
- All `lispy-outline-*`functions
- And many more...

### Other
For a complete diff with the original file, compare HEAD with the first commit, which contains the original code.
    
## Installation
As usual, you can simply download the `.el` file and put it into your load path.

Note that `vilpy` has some dependencies, you might need to get them as well.
You can consult all dependencies in the list of `requires` in `vilpy.el`.

### with `load-path`

``` emacs-lisp
;; replace `vilpy-load-path` by the directory that has `vilpy.el`
(let ((vilpy-path "vilpy-load-path"))
  (add-to-list 'load-path vilpy-path)
  (require 'vilpy))
```

### with `use-package`

``` emacs-lisp
;; replace `vilpy-load-path` by the directory that has `vilpy.el`
(use-package vilpy
   :hook ((lisp-mode . vilpy-mode)
          (emacs-lisp-mode . vilpy-mode)
          (clojure-mode . vilpy-mode))
   :load-path "~/vilpy-load-path"
   :config (add-hook 'vilpy-mode-hook #'turn-off-smartparens-mode))
```

## Usage
You can refer to the original [lispy documentation](https://github.com/abo-abo/lispy) and [function reference](http://oremacs.com/lispy/).
However, some keybindings have been changed. For the time being, please consult `vilpy-mode-map-special`.

## TODO: Special mode
## TODO: Navigation
## TODO: Transformation

### Evil
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
(map! :map vilpy-mode-map
      :i "C-d" 'vilpy-delete)
(map! :map vilpy-mode-map
      :i "C-k" 'vilpy-kill)
(map! :map vilpy-mode-map
      :i "C-y" 'vilpy-yank)
```

## Customization
This section presents some common ways that `vilpy` can be configured.

There is also the [wiki](https://github.com/Andre0991/vilpy/wiki), with additional configuration examples.

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
- Do not load `le-clojure.clj` (or any other language-specific file), which relies on injecting dependencies to cider and will not work on some project setups (see https://github.com/abo-abo/lispy/issues/552).

That said, `lispy` has more features, including debugging and refactoring capabilities and support more languages. If you want a more featureful package, go for it.
