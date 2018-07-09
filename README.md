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

To setup up FlyCheck integration add the following snippet to your init.

```Emacs Lisp
(add-hook 'alan-fabric-mode-hook
	(lambda ()
		(flycheck-mode +1)
	))
```

![screenshot](/screenshot.png)
