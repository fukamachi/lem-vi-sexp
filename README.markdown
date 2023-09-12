# lem-vi-sexp

[Vim-sexp](https://github.com/guns/vim-sexp) port for Lem.

## Keybindings

The `Leader` key is `\` (backslash) by default. It is configurable to set `leader-key` in `~/.lem/init.lisp`.

```common-lisp
(setf (lem:variable-value 'lem-vi-mode:leader-key) "Space")
```

### Normal mode

| Key | Description |
|-----|-------------|
|`=-`   | Indent the current top-level COMPOUND FORM. |
|`(`    | Move to the nearest open parenthesis. |
|`)`    | Move to the nearest close parenthesis. |
|`M-b`  | Move backward ELEMENT-wise. |
|`M-w`  | Move forward ELEMENT-wise. |
|`Leader @`| Splice the current COMPOUND FORM into its parent. |
|`Leader w`| Wrap the current ELEMENT with `(` and `)` and start insert mode at the beginning. |
|`Leader W`| Wrap the current ELEMENT with `(` and `)` and start insert mode at the end. |
|`Leader i`| Wrap the current COMPOUND FORM with `(` and `)` and start insert mode at the beginning. |
|`Leader I`| Wrap the current COMPOUND FORM with `(` and `)` and start insert mode at the end. |
|`Leader o`| Raise the current COMPOUND FORM to replace the enclosing COMPOUND FORM. |
|`Leader O`| Raise the current ELEMENT to replace the enclosing COMPOUND FORM. |
|`Leader h`| Insert the cursor at the head of the current COMPOUND FORM. |
|`Leader l`| Insert the cursor at the tail of the current COMPOUND FORM. |
|`M-H`    | Barf the last form from the current COMPOUND FORM. |
|`M-L`    | Slurp the next form into the current COMPOUND FORM. |

### Text objects

| Key | Description |
|-----|-------------|
|`af`,`if`|Select COMPOUND FORMS.|
|`aF`,`iF`|Select top-level COMPOUND FORMS.|
|`as`,`is`|Select STRINGS.|
|`ae`,`ie`|Select ELEMENTS.|
