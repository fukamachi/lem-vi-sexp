# lem-vi-sexp

[Vim-sexp](https://github.com/guns/vim-sexp) port for Lem.

## Keybindings

### Normal mode

| Key | Description |
|-----|-------------|
|`=-`   | Indent the current top-level COMPOUND FORM. |
|`(`    | Move to the nearest open parenthesis. |
|`)`    | Move to the nearest close parenthesis. |
|`M-b`  | Move backward ELEMENT-wise. |
|`M-w`  | Move forward ELEMENT-wise. |
|`Space @`| Splice the current COMPOUND FORM into its parent. |
|`Space w`| Wrap the current ELEMENT with `(` and `)` and start insert mode at the beginning. |
|`Space W`| Wrap the current ELEMENT with `(` and `)` and start insert mode at the end. |
|`Space i`| Wrap the current COMPOUND FORM with `(` and `)` and start insert mode at the beginning. |
|`Space I`| Wrap the current COMPOUND FORM with `(` and `)` and start insert mode at the end. |
|`Space o`| Raise the current COMPOUND FORM to replace the enclosing COMPOUND FORM. |
|`Space O`| Raise the current ELEMENT to replace the enclosing COMPOUND FORM. |
|`Space h`| Insert the cursor at the head of the current COMPOUND FORM. |
|`Space l`| Insert the cursor at the tail of the current COMPOUND FORM. |
|`M-H`    | Barf the last form from the current COMPOUND FORM. |
|`M-L`    | Slurp the next form into the current COMPOUND FORM. |

### Text objects

| Key | Description |
|-----|-------------|
|`af`,`if`|Select COMPOUND FORMS.|
|`aF`,`iF`|Select top-level COMPOUND FORMS.|
|`as`,`is`|Select STRINGS.|
|`ae`,`ie`|Select ELEMENTS.|
