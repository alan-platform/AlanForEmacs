;;; alan-fabric-mode.el Major mode for editing M-industries alan files
;; This package adds support for editing alan fabric files.

;; Author: Paul van Dam <pvandam@m-industries.com>
;; URL: https://github.com/M-industries/FabricForEmacs
;; Keywords: alan fabric

;; This file is not part of GNU Emacs.

;;; Code

(require 'flycheck)
(require 'timer)

(defvar alan-fabric-mode-syntax-table
  (let ((alan-fabric-mode-syntax-table (make-syntax-table)))
    (modify-syntax-entry ?* ". 23b" alan-fabric-mode-syntax-table)
	(modify-syntax-entry ?/ ". 124" alan-fabric-mode-syntax-table)
	(modify-syntax-entry ?\n ">" alan-fabric-mode-syntax-table)
	(modify-syntax-entry ?' "\"" alan-fabric-mode-syntax-table)
	(modify-syntax-entry ?} "_" alan-fabric-mode-syntax-table)
	(modify-syntax-entry ?{ "_" alan-fabric-mode-syntax-table)
	(modify-syntax-entry ?\] "_" alan-fabric-mode-syntax-table)
	(modify-syntax-entry ?\[ "_" alan-fabric-mode-syntax-table)
	alan-fabric-mode-syntax-table)
  "Syntax table for alan-fabric-mode")

(defconst alan-fabric-mode-font-lock-keywords
  `(
	'(
	  ("'[^']*?'" . font-lock-variable-name-face)
	  ("\\(->\\|~>\\)\\s-+\\(stategroup\\|component\\|group\\|file\\|dictionary\\|command\\|matrix\\|reference\\|natural\\|integer\\|text\\)\\(\\s-+\\|$\\)" 2 font-lock-type-face)
	  (,(regexp-opt '(
					  "add" "branch" "ceil" "convert" "count" "division" "floor" "increment" "max" "min" "remainder" "subtract" "sum" "sumlist" "base" "diff"
					  ))
	   . font-lock-function-name-face)
	  (,(regexp-opt '(
					  "!"
					  "#"
					  "$"
					  "%"
					  "%^"
					  "%}"
					  "&"
					  "("
					  ")"
					  "*"
					  "*&"
					  "*&#"
					  "+"
					  "+^"
					  "-"
					  "->"
					  ".>"
					  "~>"
					  "."
					  ".^"
					  ":"
					  "<"
					  "<="
					  "=="
					  ">"
					  ">="
					  "?"
					  "?^"
					  "@"
					  "add"
					  "allow"
					  "anonymous"
					  "as"
					  "branch"
					  "ceil"
					  "command"
					  "component"
					  "constrain"
					  "contains"
					  "convert"
					  "count"
					  "creation"
					  "densematrix"
					  "derived"
					  "dictionary"
					  "division"
					  "dynamic"
					  "empty"
					  "execute"
					  "external"
					  "floor"
					  "forward"
					  "from"
					  "group"
					  "id"
					  "implementation"
					  "in"
					  "increment"
					  "integer"
					  "interface"
					  "lifetime"
					  "link"
					  "log"
					  "map"
					  "matrix"
					  "max"
					  "min"
					  "mutation"
					  "natural"
					  "no"
					  "now"
					  "number"
					  "numerical"
					  "on"
					  "ontimeout"
					  "or"
					  "ordered"
					  "password"
					  "path"
					  "product"
					  "property"
					  "read"
					  "reference"
					  "remainder"
					  "remove"
					  "rename"
					  "requires"
					  "resolvable"
					  "roles"
					  "root"
					  "set"
					  "setto"
					  "sparsematrix"
					  "stategroup"
					  "stopwatch"
					  "subtract"
					  "sum"
					  "sumlist"
					  "text"
					  ".self"
					  "time"
					  "timer"
					  "to"
					  "types"
					  "unconstrained"
					  "update"
					  "users"
					  "with"
					  "workfor"
					  "yes"
					  "new"
					  "~"
					  "}"
					  "/"
					  "{"
					  "@?^"
					  "@%^"
					  "^"
					  "`"
					  "="
					  "ignore"
					  "file"
					  "create"
					  "delete"
					  "created"
					  "entry"
					  "acyclic"
					  "space"
					  "TEMP"
					  "TEMP2"
					  "relative"
					  "preceding"
					  "base"
					  "diff"
					  "std"
					  "deprecated"
					  "zero"
					  "one"
					  "$^"
					  "bidirectional"
					  "<~"
					  "invalidate"
					  "referencer"
					  "separator"
					  "FIXME"
					  ))
	   . font-lock-builtin-face)
	  )
	nil nil nil nil
	(font-lock-syntactic-face-function . alan-font-lock-syntactic-face-function))
  "Highlighting for alan mode")

(defvar alan-parent-regexp "\\s-*\\('[^']*?'\\)")

(defvar-local alan-update-headline-timer nil)

(defun alan-has-parent ()
  "Returns point of parent or nil otherwise"
  (save-excursion
	(defvar line-to-ignore-regex "^\\s-*\\(//.*\\)?$")
	(move-beginning-of-line 1)
	(while (and (not (bobp))
				(looking-at line-to-ignore-regex))
	  (forward-line -1)
	  )

	(let ((start-indent (current-indentation))
		  (curr-indent (current-indentation))
		  (curr-point (point))
		  (start-line-number (line-number-at-pos))

		  )
	  (defvar new-point)
	  (while (and (not (bobp))
				  (> start-indent 0)
				  (or (not (looking-at "\\s-*'[^']*?'"))
					  (looking-at line-to-ignore-regex)
					  (> (current-indentation) curr-indent)
					  (<= start-indent (current-indentation))
					  ))
		(forward-line -1)
		(unless  (looking-at line-to-ignore-regex)
		  (setq curr-indent (min curr-indent (current-indentation)))
		  )
		(setq new-point (point))
		)
	  (if
		  (and
		   (looking-at alan-parent-regexp)
		   (not (equal start-line-number (line-number-at-pos)))
		   )
		  (match-beginning 1)
		)
	  )))

(defun alan-goto-parent ()
  "Goto the parent of this property"
  (interactive)
  (let ((parent-position (alan-has-parent)))
	(when (alan-has-parent)
		(push-mark (point) t)
		(goto-char parent-position))))

(defun alan-copy-path-to-clipboard ()
  "Copies the path to the clipboard. Usefull for writing long
paths."
  (interactive)
  (let ((path (alan-path)))
	(when path
	  (kill-new path)
	  )
	)
)

(defun alan-path ()
  "Gives the location as a path of where you are in a file.
E.g. 'views' . 'queries' . 'context' . 'candidates' . 'of'"
  (setq path-list '())
  (save-excursion
	(while (setq has-parent (alan-has-parent))
	  (goto-char has-parent)
	  (add-to-list 'path-list (match-string 1))
	  )
	)
  (if (> (length path-list) 0)
	  (mapconcat 'identity path-list " . ")
	""
	)
  )

(defun alan-fabric-mode-indent-line ()
  "Indentation based mostly on parens. Sometimes you want to
insert a tab manually to fix top level indentations. But from
then on this works fine."
  (interactive)
  (let (new-indent)
	(save-excursion
	  (beginning-of-line)
	  (if (bobp)
		  ;;at the beginning indent to 0
		  (indent-line-to 0)
		)
	  ;; take the current indentation of the enclosing expression
	  (let ((parent-position (nth 1 (syntax-ppss)))
			(previous-line-indentation (and
										(not (bobp))
										(save-excursion
										  (forward-line -1)
										  (current-indentation))))
			)
		(cond
		 (parent-position
		  (let ((parent-indent
				 (save-excursion
				   (goto-char parent-position)
				   (current-indentation))))
			(if (looking-at "\\s-*\\s)") ;; looking at closing paren.
				(setq new-indent parent-indent)
			  (setq new-indent ( + parent-indent default-tab-width)))))
		 (previous-line-indentation
		  (setq new-indent previous-line-indentation)
		  )
		 )
		;; check for single block and add a level of indentation.
		(save-excursion
		  (back-to-indentation)
		  (if (and (looking-at "\\s(")
			  (eq
			   (line-number-at-pos)
			   (progn
				 (forward-sexp)
				 (line-number-at-pos)
				 )
			   ))
			  (setq new-indent (min (if previous-line-indentation (+ previous-line-indentation default-tab-width) default-tab-width )
									(+ new-indent default-tab-width)))
			)
		  )
		))
	(when new-indent
	  (indent-line-to new-indent))))

(flycheck-define-checker alan-fabric-compiler
  "The compiler for alan."
  :command ("fabric" "validate" "emacs" )
  :error-patterns
  (
   (error line-start (file-name) ":" line ":" column ": error:" (zero-or-one " " (one-or-more digit) ":" (one-or-more digit)) "\n"
   		  ;; Messages start with a white space after the error.
   		  (message " " (zero-or-more not-newline)
   				   (zero-or-more "\n " (zero-or-more not-newline)))
   		  line-end)
   (warning line-start (file-name) ":" line ":" column ": warning:" (zero-or-one " " (one-or-more digit) ":" (one-or-more digit)) "\n"
   		  (message " " (zero-or-more not-newline)
   				   (zero-or-more "\n " (zero-or-more not-newline)))
   		  line-end)
   )
  :modes (alan-fabric-mode)
)
(add-to-list 'flycheck-checkers 'alan-fabric-compiler)

(defun alan-font-lock-syntactic-face-function (state)
  "Do not fontify single quoted strings."
  (if (nth 3 state)
      (let ((startpos (nth 8 state)))
        (if (eq (char-after startpos) ?')
            ;; This is not a string, but an identifier.
            nil
		  font-lock-string-face
          ))
    font-lock-comment-face))

(defun alan-schedule-timer ()
  (when (timerp alan-update-headline-timer)
	(cancel-timer alan-update-headline-timer))
  (setq alan-update-headline-timer (run-with-idle-timer
   0.5 nil
   (lambda ()
	 (setq header-line-format (format " %s  " (alan-path)))
	 (force-mode-line-update)
	 )
   ))
  )

(define-derived-mode alan-fabric-mode prog-mode "fabric"
  "Major mode for editing m-industries alan fabric files."
  :syntax-table alan-fabric-mode-syntax-table
  (setq comment-start "//")
  (setq comment-end "")
  (setq block-comment-start "/*")
  (setq block-comment-end "*/")
  (setq font-lock-defaults alan-fabric-mode-font-lock-keywords)
  (set (make-local-variable 'compilation-error-screen-columns) nil)
  (set (make-local-variable 'indent-line-function) 'alan-fabric-mode-indent-line)
  (add-hook 'post-command-hook #'alan-schedule-timer nil t)
  (setq header-line-format "")
  )

(provide 'alan-fabric)

;;; alan-fabric-mode.el ends here
