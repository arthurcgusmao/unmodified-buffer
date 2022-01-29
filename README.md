# unmodified-buffer.el

A minor mode that automatically restores an Emacs buffer modified state in case
its contents match the original file it is visiting.


## Installation and Usage

### Manual installation

Clone the repository somewhere in your machine, add its location to the Emacs
path, require it, and enable the minor mode:

```lisp
(add-to-list 'load-path "/path/to/unmodified-buffer/")
(require 'unmodified-buffer)
(add-hook 'after-init-hook 'unmodified-buffer-global-mode) ;; Optional
```

### Installation using [straight.el](https://github.com/raxod502/straight.el) and [use-package](https://github.com/jwiegley/use-package):

```lisp
(use-package unmodified-buffer
  :straight (:host github :repo "arthurcgusmao/unmodified-buffer")
  :hook (after-init . unmodified-buffer-global-mode)) ;; Optional
```

### Usage within a single buffer

Turn the mode locally by issuing the command `unmodified-buffer-mode` on the
desired buffer.


## Additional configurations

The following customizable variables exist, please check their docstrings for
more detail.

- `unmodified-buffer-size-limit`
- `unmodified-buffer-check-period`
- `unmodified-buffer-ignore-remote`
