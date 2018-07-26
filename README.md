# Alan For Emacs

A major mode for editing Alan files. It provides:
- Syntax highlighting.
- Display the current context (path) at point.
- Automatic indentation.
- Flycheck integration.
- Simple xref backend.

## Installation

- Put alan-mode.el in your load-path.
- Install it from [![MELPA](https://melpa.org/packages/alan-mode-badge.svg)](https://melpa.org/#/alan-mode) using package-install  <kbd>M-x package-install [ret] alan-mode</kbd>

## Configuration

To setup up FlyCheck add the following snippet to your init.

```Emacs Lisp
(add-hook 'alan-mode-hook
	(lambda ()
		(flycheck-mode +1)
	))
```

When in an empty Alan block (e.g. `{ }`) a newline yields the following result:
```
{
	|}
```

In most cases this is more useful:
```
{
	|
}
```

Alan mode provides an electric layout rule (`alan-add-line-in-braces-rule`). Set it up in your init file, preferably in the `alan-mode-hook`.
```
(electric-layout-mode 1)
(set (make-variable-buffer-local 'electric-layout-rules) (list alan-add-line-in-braces-rule))
```

![screenshot](/screenshot.png)
