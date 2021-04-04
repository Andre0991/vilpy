# lispy-lite

This is a stripped-down fork of the excellent [lispy](https://github.com/abo-abo/lispy), a paredit-like mode.
The motivation is not loading `le-clojure.clj` (or any other language-specific file), which relies on injecting dependencies to cider and will not work on some project setups (see https://github.com/abo-abo/lispy/issues/552).

I prefer forking the original code because (1) the author is happy with the current approach of bundling everything in the same package, and that's perfectly fine ([issue](https://github.com/abo-abo/lispy/issues/74)) and (2) `lispy` is critical for getting things done in my job, but the Clojure-specific parts are not important for me and they do interfere with some projects.

Design goals (mostly not taken into practice yet):
- Reduce the feature set to navigation and evaluation functions. We don't need debugging and tags features.
- Prefer to leverage Emacs built-ins function when possible.
- Implement a uniform API that works well with all supported languages. Alternatively: avoid commands that are language-specific.
- Rely on fewer external dependencies.
- Drop support for non-lisps languages.

## Differences with `lispy`

### `lispy-eval` (e)
Calls `eval-last-sexp` (`emacs-lisp-mode`), `cider-eval-last-sexp` (`cider-mode`) or `inf-clojure-eval-last-sexp` (`inf-clojure-minor-mode`) rather than directly using the cider middleware functions.

### `lispy-eval-and-insert` (E)
Renamed to `lispy-eval-last-sexp-and-insert-comment`.
Adds `;; =>` in front of the result

### `lispy-follow` (F)
Uses `xref-find-definitions`.

### `lispy-goto` (g)
Uses `imenu`.

### `lispy-goto-symbol` (M-.)

You may want to call something fancier:

``` emacs-lisp
(lispy-define-key lispy-mode-map "g" 'counsel-imenu)
```

### Removed functions
- `lispy-goto-projectile` (0g and ogp).
- `lispy-goto-elisp-commands` (oge)
- `lispy-goto-local` (G)
- `lispy-goto-recursive` (ogr)
- `lispy-goto-def-down`
- `lispy-goto-def-ace`
- `lispy-debug-step-in` (xj)
- `lispy-goto-symbol-elisp`

### Other
Some functions have been removed.
For a complete diff with the original file, compare HEAD with the first commit, which contains the original code.

## Usage
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

## Roadmap
The current version works fine for my use case, but I'd love to remove some dependencies (looking at you, `ivy`, `swiper`, `hydra`, `avy`, etc), have a uniform set of operations that work on all supported lisps and leverages some operations to language-specific packages.
