# FabricForEmacs

A major mode for editing alan files. It provides:
- Syntax highlighting.
- Display the current context (path) at point.
- Automatic indentation.
- Flycheck integration.

To setup up flycheck integration add the following snippit to your init.

```Emacs Lisp
(add-hook 'alan-fabric-mode-hook
	(lambda ()
		(flycheck-mode +1)
	))
```

![screenshot](/screenshot.png)
