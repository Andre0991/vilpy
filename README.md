# lispy-lite

**Important: Work in progress. Expect things to break.**

This is a stripped-down fork of the excellent [lispy](https://github.com/abo-abo/lispy), a paredit-like mode.
The motivation is not loading `le-clojure.clj` (or any other language-specific file), which relies on injecting dependencies to cider and will not work on some project setups (see https://github.com/abo-abo/lispy/issues/552).

I prefer forking the original code because (1) the author is happy with the current approach of bundling everything in the same package, and that's perfectly fine ([issue](https://github.com/abo-abo/lispy/issues/74)) and (2) `lispy` is critical for getting things done in my job, but the Clojure-specific parts are not important for me and they do interfere with some projects.

Design goals (mostly not taken into practice yet):
- Reduce the feature set to navigation and evaluation functions. We don't need debugging and tags features. Refactoring commands can be leveraged from other packages.
- Prefer to leverage Emacs built-in functions whenever possible.
- Implement a uniform API that works well with all supported languages. Alternatively: avoid language-specific commands.
- Rely on fewer external dependencies.
- Drop support for non-lisps languages.

## Differences with `lispy`

### `lispy-eval` (e)
Calls `eval-last-sexp` (`emacs-lisp-mode`), `cider-eval-last-sexp` (`cider-mode`) or `inf-clojure-eval-last-sexp` (`inf-clojure-minor-mode`) rather than directly using the cider middleware functions.

### `lispy-eval-and-insert` (E)
Removed.
New command added to the same binding: `lispy-eval-defun`.

### `lispy-follow` (F)
Uses `xref-find-definitions`.

### `lispy-goto` (g)
Uses `imenu`.

You may want to call something fancier:

``` emacs-lisp
(lispy-define-key lispy-mode-map "g" 'counsel-imenu)
```

### `lispy-tab` (i)
Does not deal with outlines.
In `clojure-mode`, only calls `clojure-align` and trims whitespace at beginning of line.

### `lispy-splice` (/)
Changed to `x`.

### `lispy-ace-symbol-replace` (H)
Unmapped. `H` is mapped to `lispy-describe`.

### Removed functions
- `lispy-goto-projectile` (0g and ogp).
- `lispy-goto-elisp-commands` (oge)
- `lispy-goto-local` (G)
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

### Other
For a complete diff with the original file, compare HEAD with the first commit, which contains the original code.
    
## Installation
As usual, you can simply download the `.el` file and put it into your load path.

Note that `lispy` has some dependencies, you might need to get them as well.
You can consult all dependencies in the list of `requires` in `lispy.el`.

### with `load-path`

``` emacs-lisp
;; replace `lispy-load-path` by the directory that has `lispy.el`
(let ((lispy-path "lispy-load-path"))
  (add-to-list 'load-path lispy-path)
  (require 'lispy))
```

### with `use-package`

``` emacs-lisp
;; replace `lispy-load-path` by the directory that has `lispy.el`
(use-package lispy
   :hook ((lisp-mode . lispy-mode)
          (emacs-lisp-mode . lispy-mode)
          (clojure-mode . lispy-mode))
   :load-path "~/lispy-load-path"
   :config (add-hook 'lispy-mode-hook #'turn-off-smartparens-mode))
```

## Usage
Refer to the original [lispy documentation](https://github.com/abo-abo/lispy) and [function reference](http://oremacs.com/lispy/).

### Evil
`lispy` disputes keybindings with `evil-mode`, so in some cases its commands are overwritten.
Using [lispyville](https://github.com/noctuid/lispyville) is a typical solution for making both packages work together.
Personally, I'm fine with the default `lispy` bindings working only in `insert-mode`.
I simply use these customizations for making `lispy` take precedence over `evil` in some specific keys:

``` emacs-lisp
(defun raise-minor-mode (mode)
  "Make MODE the first on `minor-mode-map-alist'."
  (let ((x (assq mode minor-mode-map-alist)))
    (when x
      (setq minor-mode-map-alist
            (cons x (delq mode minor-mode-map-alist))))))
            
            
;; prevents `/` (`lispy-splice`) from being overriden
(raise-minor-mode 'lispy-mode)

;; TODO: `map!` is a Doom-emacs macro - not sure how
;; to do this with vanilla Evil.
(map! :map lispy-mode-map
      :i "C-d" 'lispy-delete)
(map! :map lispy-mode-map
      :i "C-k" 'lispy-kill)
(map! :map lispy-mode-map
      :i "C-y" 'lispy-yank)
```

## Customization
### Defining bindings
Use `lispy-define-key` for overriding a default binding.

For example, let's say you want to override `H`, which is originally bound to `lispy-describe`, to `my-function`, but only if `inf-clojure-minor-mode` is enabled.

You would use this:

``` emacs-lisp
(defun apt-lispy-describe ()
  (interactive)
  (if (bound-and-true-p inf-clojure-minor-mode)
      (call-interactively 'my-function)
    (call-interactively 'lispy-describe)))

(lispy-define-key lispy-mode-map "H" 'apt-lispy-describe)
```

