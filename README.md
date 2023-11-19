# vilpy

<p align="center">
<img src="imgs/torta-holandesa.jpg"
   alt="vilpy logo"/>
</p>

> creamy & even shorter lisp editing

`vilpy` is a vi-like `paredit`.
It is modal in the sense that when the point is exactly before or after a paren, keys run commands rather than inserting text. For more details, see [documentation](#documentation).

`vilpy` is a stripped-down fork of the excellent [lispy](https://github.com/abo-abo/lispy).
Think of it as a `lispy-core`, which is lighter, but no less sweet.
`vilpy` keybindings are vimmier and it is considerably smaller (no refactoring and debugging features, fewer dependencies).
For more differences with `lispy`, please consult the [alternatives](#alternatives) section.

I prefer forking the original code because (1) the author is happy with the current approach of bundling everything in the same package, and that's perfectly fine ([issue](https://github.com/abo-abo/lispy/issues/74)) and (2) `lispy` is critical for getting things done in my job, but the Clojure-specific parts are not important for me and they do interfere with some projects.

## Supported languages & compatibility with other modes
The navigation features are tested with emacs-lisp and Clojure, but they are likely to work with other lisps.

emacs-lisp and Clojure (`cider` and `inf-clojure`) also support evaluation, describing the symbol at point and indentation.
These features can be added to other languages by setting the proper handlers in the variable `vilpy--handlers-alist` (see the customization section).

`vilpy` defines its own keybindings that might conflict with `parinfer`, `paredit` and other structural editing modes.

## Installation
`vilpy` must be installed manually as of now.

Note that `vilpy` requires [`avy`](https://github.com/abo-abo/avy).
You might need to install it as well.

### Vanilla

``` emacs-lisp
;; replace `vilpy-load-path` by the directory that has `vilpy.el`
(let ((vilpy-path "~/path/to/vilpy"))
  (add-to-list 'load-path vilpy-path)
  (require 'vilpy))
(add-hook 'emacs-lisp-mode-hook (lambda () (vilpy-mode 1)))
(add-hook 'clojure-mode-hook (lambda () (vilpy-mode 1)))
; if using the treesitter version of clojure-mode:
(add-hook 'clojure-ts-mode-hook (lambda () (vilpy-mode 1)))

;; recommended settings for `evil` users
;; those are the default keybindings, but they are overriden by `evil`
(evil-define-key 'insert 'vilpy-mode-map (kbd "C-k") 'vilpy-kill)
(evil-define-key 'insert 'vilpy-mode-map (kbd "C-d") 'vilpy-delete)
```

### `use-package`

``` emacs-lisp
(use-package vilpy
  :load-path
  "~/path/to/vilpy/"
  :hook
  ((emacs-lisp-mode clojure-mode clojure-ts-mode) . vilpy-mode))
```

## Documentation

You can find workflow examples in [usage](/docs/usage.md).

For a complete list of commands, see the [function reference](/docs/reference.md).

## Customization
This section presents some common ways that `vilpy` can be configured.

There is also the [wiki](https://github.com/Andre0991/vilpy/wiki), with additional configuration examples and tips.

### Adding support to other modes

By default, `vilpy` supports Emacs Lisp and Clojure.
These are the official supported modes because I use them everyday and the test suite ensures that their syntax is well handled by `vilpy`.
That said, you can easily use `vilpy` with other languages (presumably lisps) and it should work without major issues.

For example, for using `vilpy` with [sly](https://github.com/joaotavora/sly), customize `vilpy--handlers-alist` like this:

```emacs-lisp
(add-to-list 'vilpy--handlers-alist
	     '(:sly . ((:decider-fn . (lambda () (bound-and-true-p sly-mode)))
		       (:eval-last-sexp . sly-eval-last-expression)
		       (:eval-defun . sly-eval-defun)
		       (:eval-region . sly-eval-region)
		       (:eval-buffer . sly-eval-buffer)
		       (:describe-symbol . sly-describe-symbol))))
```

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

#### Overriding behavior
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
### [`lispy`](https://github.com/abo-abo/lispy/)
`vilpy` has some important differences from its big brother.

In particular, it attempts to respect the following design goals:
- **Be vimmier**: the default keybindings resemble vim more than `lispy` (note that if you do not know vim, it's okay. `lispy` keybindings are arguably not super emacs-y either, so either way you would have to invest some time in learning some keys.)
- **Reduce the feature set to navigation and evaluation functions**. We don't need debugging and tags features. Refactoring commands can be leveraged from other packages. This makes `vilypy` easier to maintain and understand.
- **Have a reduced, but reliable set of commands that works well with all supported languages.** The original `lispy` has some nice refactoring tools, but they mostly works for Emacs Lisp. This adds confusion for people trying to use it, as it is not always clear if some command will work for a given language.
- **Rely on fewer external dependencies**. This also makes `vilpy` easier to maintain. Currently, its only dependency is `avy`, which is essential for navigation commands.
- **Drop support for non-lisps languages**. While the prospect of using `vilpy` in other languages is interesting, this is not very well tested and not a priority. That said, you can always invoke `vilpy-mode` manually and see what happens.
- **Be less magic**. For example, in the original `lispy`, the function for evaluating the last sexp also uses `setq` when evaluating `defvars`. `vilpy` does not try to guess your intent. `vilpy` is intentionally dumber.
- **Do not inject language-specific code**. For example, `lispy` loads `le-clojure.clj` when you use `cider` for providing some features such as getting bindings from `let` when evaluating forms. This relies on injecting dependencies to `cider` and will not work on some project setups (see https://github.com/abo-abo/lispy/issues/552). `vilpy` simple uses `cider-eval*` functions.

That said, `lispy` has more features, including debugging and refactoring capabilities and support more languages. If you want a more featureful package, go for it.

### [`lispyville`](https://github.com/noctuid/lispyville)

`lispyville` is a layer on top of `lispy` that adapts some `evil` commands for working consistently with `lispy`.
Personally, I never felt the need for `lispyville` when using `evil`: using vim commands while in normal mode and `lispy` commands in insert mode worked fine for me.
I found `lispyville` quite difficult to grasp, and I simply got used to the raw `lispy` commands, avoiding this extra indirection for `lispy` commands.

That said, it's clear that `lispyville` is useful for lots of people., so would be great if it worked with `vilpy`.
I believe this should not be too difficult: maybe simply forking it and renaming `lispy` to `vilpy` and fixing some renamed functions will do it.
Another simpler possibility is aliasing `vilpy` functions to their `lispy` counterparts.
However, I do not have the motivation for experimenting with this now.
