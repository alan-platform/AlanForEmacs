# FabricForEmacs

A major mode for editing alan files. It provides:
- Syntax highlighting.
- Display the current context (path) at point.
- Automatic indentation.
- Flycheck integration.

## Installation

Put alan-fabric-mode.el in your load-path.

To associate the mode with alan file add this to your init.

```Emacs Lisp
(add-to-list `auto-mode-alist '("\\.alan" . alan-fabric-mode))
```

To setup up flycheck integration add the following snippet to your init.

```Emacs Lisp
(add-hook 'alan-fabric-mode-hook
	(lambda ()
		(flycheck-mode +1)
	))
```

![screenshot](/screenshot.png)
