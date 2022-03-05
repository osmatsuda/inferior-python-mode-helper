# inferior-python-mode-helper

Utilities for inferior-python-mode

## Installation

1. Clone this repository
2. Visit the directory with emacs
3. `M-x load-file` and load the `make.el`
   - This script is setting a module name that used on the top level of the inferior-python-mode (defaults to `_el`), and merging the file `inferior-python-mode-helper.py` into the file `inferior-python-mode-helper.el`, and then executing `(byte-compile-file THAT-FILE)`.
4. Move the built file `build/inferior-python-mode-helper.elc` to your library directory specified by the variable `load-path`.
5. Add the followings to your initialization file:
   - `(autoload 'inferior-python-mode-helper "inferior-python-mode-helper")`
   - `(add-hook 'python-shell-first-prompt-hook #'inferior-python-mode-helper)`

## Usage

inferior-python-mode-helper.el is used with inferior-python-mode to add extensions that integrate the Emacs-Lisp commands and functions with the Python REPL.

There are two types of features:

- First, commands works independently in a REPL input.
- Second, commands works in a Python expression, that is expanded into some Python expressions and then sended to the Python inferior process.

### Single use commands

#### pwd

```
>>> _el.pwd
```

This command executes `print(os.getcwd())` in the Python and then executes `(cd-absolute <PRINTED_DIR>)` in the REPL buffer.

#### cd

```
>>> _el.cd


---------- Buffer: Minibuffer ----------
Change working directory: <DEFAULT-DIRECORY>
 ⋮
Change working directory: <SELECTED_DIR> <RET>
---------- Buffer: Minibuffer ----------
```

This command reads a directory path selected in the minibuffer, and executes `os.chdir(<SELECTED_DIR>)` in the Python, and then executes `(cd-absolute <SELECTED_DIR>)` in the REPL buffer.

#### cd_b

```
>>> _el.cd_b


---------- Buffer: Minibuffer ----------
Change to Buffer’s directory: 
 ⋮
Change to Buffer’s directory: <SELECTED_BUFFER> <RET>
---------- Buffer: Minibuffer ----------
```

This command reads a directory path from a buffer selected in the minibuffer, that buffer should have a `default-directory` value. Next, that command executes `os.chdir(<BUFFER'S_DIR>)` in the Python, and then executes `(cd-absolute <BUFFER'S_DIR>)` in the REPL buffer.

### Commands expanding into Python expressions

#### open_b

```
>>> with _el.open_b as b:
...     for line in b:
...         print(line[:-1])


---------- Buffer: Minibuffer ----------
Source Buffer: 
 ⋮
Source Buffer: <SELECTED_BUFFER> <RET>
---------- Buffer: Minibuffer ----------
```

This command lets you select a buffer in the minibuffer, and replace to a expression `io.StringIO(<SELECTED_BUFFER_CONTENTS>)` or `open(<TEMP_FILENAME>)`, that is a text readable file-like object. When the number of bytes in the buffer contents is less than 1024, this command is translated into `io.StringIO`. In another case, this module creates a temporary file by `(python-shell--save-temp-file <CONTENTS>)` in python.el module, and return that file object.

#### open_f

```
>>> file_handle = _el.open_f


---------- Buffer: Minibuffer ----------
Find file: 
 ⋮
Find file: <SELECTED_FILE> <RET>
 ⋮
Mode (default r): <SELECTED_MODE> <RET>
---------- Buffer: Minibuffer ----------
```

This command lets you select a file and specify a mode in the minibuffer, and translates to a expression `open(<SELECTED_FILE>, <SELECTED_MODE>)`. You can select a mode from "r", "rb", "w", "wb", "a", and "ab".

#### eval_expr

```
>>> for i in _el.eval_expr:
...    do_something_with(i)


---------- Buffer: Minibuffer ----------
Eval: 
 ⋮
Eval: (list 1 2 3 4) <RET>
---------- Buffer: Minibuffer ----------
```

This command translates a Emacs-Lisp value that you type in the minibuffer into a corresponding Python object. You can enter a ELisp expression in the minibuffer, and that is evaluated by `(eval-expression <EXP>)`. The above example `(list 1 2 3 4)` is translated into a list object and send as `[1, 2, 3, 4]`. Other than that:

- Hash tables and list of cons are converted to the `dict[str, Any]`
- Other lists and vector like objects are converted to the `list[Any]`
- Symbols are converted `str`

#### write_b(VALUE, end='\n')

```
>>> _el.write_b(file_handle.read())


---------- Buffer: Minibuffer ----------
Destination Buffer: 
 ⋮
Destination Buffer: <SELECTED_BUFFER> <RET>
---------- Buffer: Minibuffer ----------
```

This function inserts a value of its argument into your selected buffer in the minibuffer. You cannot select a buffer-read-only buffer and the Python REPL buffer.  That value is inserted at the current point of the buffer and point and markers move forward to the end of the inseted text. `end` keyword argument is same as `print()` built-in function.

## Future considered

- Helper commands should be more modulable and easy to add.
- I have tested only with Emacs 27.2 and Python 3.9 on macOS Big Sur.
