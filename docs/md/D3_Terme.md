% Lisp Function: terme

Npt documentation.

Reference: [ANSI Common Lisp npt](index.html)


# Lisp Function Specification

The following functions of the `npt-system` package are described.

- [sysctl](#terme-1)

```lisp
defun terme
```

- [`terme-enable`](#terme-2)
- [`terme-begin`](#terme-3)
- [`terme-end`](#terme-4)
- [`terme-input`](#terme-5)
- [`terme-output`](#terme-6)
- [`terme-move`](#terme-7)
- [`terme-clear`](#terme-8)
- [`terme-delete`](#terme-9)
- [`terme-size`](#terme-10)
- [`terme-scroll`](#terme-11)
- [`terme-font`](#terme-12)


# <a id="terme-1">Function `terme`</a>

This function manipulates the input and output of the terminal.

To use terme, you need to specify the compile option.  
You can check whether it is available or not
with the command line argument `--version`.

```
$ npt --version | grep Prompt
Prompt mode          terme
```

The function specification is shown below.

```lisp
(defun terme (symbol &rest args) ...) -> *

Input: symbol
Input: args
OOutput: *
```

`symbol` can have the following contents

- `terme-enable`
- `terme-begin`
- `terme-end`
- `terme-input`
- `terme-output`
- `terme-move`
- `terme-clear`
- `terme-delete`
- `terme-size`
- `terme-scroll`
- `terme-font`

For example, when you run `terme-enable`, do the following

```lisp
(terme 'terme-enable)
```

Unlike `sysctl`, the argument is `symbol` compared with `eq`.  
If you do not inherit from the `npt-system` package, you can run

```lisp
(npt-system:terme 'npt-system:terme-enable)
```

The following explanation assumes that the package is inherited.


# <a id="terme-2">Function `terme`: `terme-enable`</a>

Returns whether the terme module is available or not.

```lisp
(defun terme (terme-enable) ...) -> boolean
```

If the return is `t`, the `terme` function is available.  
If the return is `nil`, the `terme` function is not available.

The following is an example of execution.

```lisp
(terme 'terme-enable)
```


# <a id="terme-3">Function `terme`: `terme-begin`</a>

Start the terme operation.

```lisp
(defun terme (terme-begin &optional mode) ...) -> paper
```

For internal operation, the terminal is set to raw mode.  
The raw mode is different from the normal mode in the following ways

- Ctrl+C, etc. are treated as input.
- Input and output is done in bytes, not lines.
- Line feeds are handled differently than usual.

The return value `paper` is a Paper object
with the terminal information before the change.  
By passing it as an argument of `terme-end`,
you can restore the state of the terminal.  
Be sure to run `terme-end` before exiting.

The `mode` argument can be `nil` or `:default`.  
If it is `nil`, it sets the terminal to raw mode.  
If `mode` is `:default`, it will change the terminal to the startup configuration.  
The return value is the terminal information before it is changed, as in raw mode.

See `terme-end` for an execution example.


# <a id="terme-4">Function `terme`: `terme-end`</a>

Terminate the terme operation.

```lisp
(defun terme (terme-end paper) ...) -> null
```

For internal operation, restores the terminal to its normal state
based on the information in the argument.

It is recommended to run `terme-end` in the cleanup of `unwind-protect`.  
For example, the following will work.

```lisp
(let ((x (terme 'terme-begin)))
  (unwind-protect
    (progn ...)
    (terme 'terme-end x)))
```

# <a id="terme-5">Function `terme`: `terme-input`</a>

Receives input.

```lisp
(defun terme (terme-input &optional (block t)) ...) -> (values symbol value)
  block   (or null (eql t) integer float)  ;; default T
  symbol  symbol
  value   (or null integer)
```

This function will stop execution until an input is received.  
The input can be any of the following

- Input from the keyboard
- Terminal resizing notification

The correspondence between the input and returned values is shown below.

|Input|Return `symbol`|Return `value`|
|----|----|----|
|Character|`terme-code`|Code|
|Up key|`terme-up`|`nil`|
|Down key|`terme-down`|`nil`|
|Left key|`terme-left`|`nil`|
|Right key|`terme-right`|`nil`|
|Function key|`terme-function`| 1, 2, ...|
|Home key|`terme-home`|`nil`|
|End key|`terme-end`|`nil`|
|Page Up key|`terme-page-up`|`nil`|
|Page Down key|`terme-page-down`|`nil`|
|Insert key|`terme-insert`|`nil`|
|Esc key|`terme-escape`|`nil`|

The correspondence between events and returned values is shown below.

|Event|Return `symbol`|Return `value`|
|----|----|----|
|Input error|`nil`|`nil`|
|Resizing|`terme-signal`|`nil`|
|Time out|`terme-hang`|`nil`|

An input error is when an unrecognized input
or an unsupported escape sequence is received.  
This may occur frequently, so it is recommended to ignore it.

A resize is when the process receives a signal.  
When the size of the terminal is changed,
it also receives a signal, which triggers the screen to be redrawn.

Describes timeouts.

`terme-input` takes an optional argument `block`.  
When omitted, it is `t`.  
If `block` is `t`, it will wait indefinitely.  
If `block` is `nil` or `0`, it will return immediately without waiting.  
If `block` is an integer or a float, it is the number of seconds until timeout.  
If a timeout occurs, `terme-hang` will be returned.

The following is an example of execution.

```lisp
(terme 'terme-input)
-> TERME-CODE, 3       ;; Ctrl+C received.

(terme 'terme-input 0.01)
-> TERME-HANG, NIL     ;; Time out.
```

# <a id="terme-6">Function `terme`: `terme-output`</a>

Output data to the terminal.

```lisp
(defun terme (terme-output &optional value) ...) -> null
  value  (or null charcter string integer array)  ;; default nil
```

The `value` argument is optional and defaults to `nil`.  
If the argument `value` is a character, it will be output in UTF-8 encoding.  
If the argument `value` is a string, the output will be UTF-8 encoded.  
If the argument `value` is an integer,
it will be assumed to be Unicode code and output with UTF-8 encoding.  
If the argument `value` is an array, output will be performed according to the content.  
If the argument `value` is `nil`, data in the cache will be flushed.

If the argument is an array, it must be one-dimensional.  
An array outputs from the first element to the value of the `fill-pointer`.  
You can prepare a large buffer in advance and manipulate its size
with `fill-pointer` to save memory space and improve speed.

When output is performed with this function,
the output data will be pending in the internal buffer.  
If you want to reflect the data on the screen,
be sure to flush the content with `(terme 'terme-output)`.  
Other operation functions that are not `terme-output` (such as `terme-move`) are
realized by outputting the escape sequence with `terme-output`.  
If you want such functions to be reflected on the screen immediately,
flush the buffer by executing `(terme 'terme-output)`
as well as the character output.

The following is an example of execution.

```lisp
;;  13, 10, Output newline by flush.
(terme 'terme-output 13)
(terme 'terme-output 10)
(terme 'terme-output)

;;  Moving the cursor.
(terme 'terme-move 10 20 :absolute)
(terme 'terme-output)
```


# <a id="terme-7">Function `terme`: `terme-move`</a>

Move the cursor.

```lisp
(defun terme (terme-move x y mode) ...) -> null
  x     integer
  y     (or null integer)
  mode  (member :absolute :relative)
```

If `mode` is `:relative`, specifies the distance to move from the current cursor position.  
If `mode` is `:absolute`, it specifies the absolute coordinates from the top-left corner.  
`(0, 0)` is the origin.  
You can set `y` to `nil` only when `:absolute`.

The following is an example of execution.

```lisp
(terme 'terme-move 0 0 :absolute)
(terme 'terme-output)
```


# <a id="terme-8">Function `terme`: `terme-clear`</a>

Erase the text of the entire terminal.

```lisp
(defun terme (terme-clear &optional value) ...) -> null
  value  (member nil :before :after)  ;; default nil
```

The `value` argument is optional and defaults to `nil`.  
If the `value` argument is `nil`, the entire screen will be deleted.  
If the `value` argument is `:before`, the entire top and bottom of the line
including the cursor position will be deleted.  
If the `value` argument is `:after`, it will delete the end of the line
including the cursor position and the whole bottom part.  

The following is an example of `:before` execution.

```lisp
(terme 'terme-clear)
(terme 'terme-move 0 0 :absolute)
(terme 'terme-output "ABCDEF")
(terme 'terme-move 0 1 :absolute)
(terme 'terme-output "GHIJKL")
(terme 'terme-move 0 2 :absolute)
(terme 'terme-output "MNOPQR")

(terme 'terme-move 3 1 :absolute)
(terme 'terme-clear :before)
(terme 'terme-move 0 3 :absolute)
(terme 'terme-output)
```

The execution results are as follows.

```

    KL
MNOPQR
```

The following is an example of `:after` execution.

```lisp
(terme 'terme-clear)
(terme 'terme-move 0 0 :absolute)
(terme 'terme-output "ABCDEF")
(terme 'terme-move 0 1 :absolute)
(terme 'terme-output "GHIJKL")
(terme 'terme-move 0 2 :absolute)
(terme 'terme-output "MNOPQR")

(terme 'terme-move 3 1 :absolute)
(terme 'terme-clear :after)
(terme 'terme-move 0 3 :absolute)
(terme 'terme-output)
```

The execution results are as follows.

```
ABCDEF
GHI

```


# <a id="terme-9">Function `terme`: `terme-delete`</a>

Erase a line of text.

```lisp
(defun terme (terme-delete &optional value) ...) -> null
  value  (member nil :before :after)  ;; default nil
```

The `value` argument is optional and defaults to `nil`.  
If the `value` argument is `nil`, the entire line will be deleted.  
If the `value` argument is `:before`, the entire line up to and
including the cursor position will be deleted.  
If the `value` argument is `:after`, the end of the line
including the cursor position will be deleted.  

The following is an example of `:after` execution.

```lisp
(terme 'terme-clear)
(terme 'terme-move 0 0 :absolute)
(terme 'terme-output "ABCDEF")
(terme 'terme-move 0 1 :absolute)
(terme 'terme-output "GHIJKL")
(terme 'terme-move 0 2 :absolute)
(terme 'terme-output "MNOPQR")

(terme 'terme-move 3 1 :absolute)
(terme 'terme-delete :before)
(terme 'terme-move 0 3 :absolute)
(terme 'terme-output)
```

The execution results are as follows.

```
ABCDEF
    KL
MNOPQR
```


# <a id="terme-10">Function `terme`: `terme-size`</a>

Gets the width and height of the terminal.  
The unit is the number of characters.

```lisp
(defun terme (terme-size) ...) -> x, y
  x  Number of horizontal characters. Width.
  y  Number of vertical characters. Height.
```

Execution example

```lisp
* (terme 'terme-size)
80
25
```


# <a id="terme-11">Function `terme`: `terme-scroll`</a>

Scroll up and down the screen.

```lisp
(defun terme (terme-scroll value) ...) -> null
  value  integer
```

If `value` is positive, it will scroll up.  
If `value` is negative, it will scroll down.  

When `value` is `1`, the behavior is the same as
when you press Enter at the bottom of the screen.


# <a id="terme-12">Function `terme`: `terme-font`</a>

Changes the text type, text color, and background color.

```lisp
(defun terme (terme-font &rest args) ...) -> null
  value  (member nil :before :after)  ;; default nil
```

This function will not display correctly depending on
the performance of the terminal being displayed.  

Text type refers to attributes such as bold, italic, etc.  
The text and background colors can be set to the default 8 colors,
 256 palettes, or RGB specification.  

The following order explains how to use this function.

- Resetting characters
- Setting the text type
- Setting text color
- Setting background color
- Composite settings


## Resetting characters

You can reset the character by doing one of the following

```lisp
(terme 'terme-font)
(terme 'terme-font nil)
(terme 'terme-font 'code 'reset)
(terme 'terme-font 'code 0)
```

The text type, text color, and background color will be
restored to their default settings.


## Setting the text type

The text type can be set with `'code`.

```lisp
(terme 'terme-font 'code x)
```

`x` can be an integer or a symbol.  
If it is an integer, it is the operation number of the escape sequence.  
If `x` is a symbol, the following contents can be specified.

|symbol|Description|Code|
|---|---|---|
|`reset`       |Reset or normal              |0 |
|`bold`        |Bold or increased intensity  |1 |
|`faint`       |Faint, decreased intensity   |2 |
|`italic`      |Italic                       |3 |
|`underline`   |Underline                    |4 |
|`slow-blink`  |Slow blink                   |5 |
|`rapid-blink` |Rapid blink                  |6 |
|`reverse`     |Reverse video or invert      |7 |
|`hide`        |Conceal or hide              |8 |
|`strike`      |Crossed-out, or strike       |9 |

The following is an example of execution.

```lisp
(terme 'terme-font 'code 'italic)
(terme 'terme-output)
```


## Setting text color

The text color can be specified in the following three ways.

- Standard 8 colors
- 256 palettes
- RGB full color

The standard 8-color specification can be set with the `fore` identifier.  
It takes the following values as arguments.

|Color| Value | Value (Dark) | Value (Bright) |
|---|---|---|---|
|Black   | `black`   | `dark-black`   | `bright-black`   |
|Red     | `red`     | `dark-red`     | `bright-red`     |
|Green   | `green`   | `dark-green`   | `bright-green`   |
|Yeloow  | `yellow`  | `dark-yellow`  | `bright-yellow`  |
|Blue    | `blue`    | `dark-blue`    | `bright-blue`    |
|Magenta | `magenta` | `dark-magenta` | `bright-magenta` |
|Cyan    | `cyan`    | `dark-cyan`    | `bright-cyan`    |
|White   | `white`   | `dark-white`   | `bright-white`   |
|Default | `default` | -              | -                |

For identifications with neither `dark-` nor `bright-`,
the lightness or darkness will be selected
according to the value of `*prompt-bright*`.  
For example, if `*prompt-bright*` is `t`,
 `yellow` is the same as `bright-yellow`.

The following is an example of execution.

```lisp
(terme 'terme-font 'fore 'dark-magenta)
(terme 'terme-output)
```

The 256-palette specification can be set with the `palfore` identifier.  
The argument can be an integer from `#x00` to `#xFF`.

The following is an example of execution.

```lisp
(terme 'terme-font 'palfore #xAA)
(terme 'terme-output)
```

The full RGB color specification can be set with the identifier `rgbfore`.  
It takes three consecutive integers from `#x00` to `#xFF` as arguments.

The following is an example of execution.

```lisp
(terme 'terme-font 'rgbfore #xFF #xFF #x80)
(terme 'terme-output)
```


## Setting background color

The setting method is the same as for text color.  
The identifiers are different and are shown below.

- Standard 8 colors, `back`.
- 256 palettes, `palback`.
- RGB full color, `rgbback`.

The following is an example of execution.

```lisp
(terme 'terme-font 'back 'dark-magenta)
(terme 'terme-font 'palback #xAA)
(terme 'terme-font 'rgbback #xFF #xFF #x80)
```


## Composite settings

The text type, text color, and background color can be set in combination.  
If you want to set them at the same time, enter them consecutively.

An execution example is shown below.

```lisp
(terme 'terme-font 'code 'underline 'fore 'magenta 'rgbback #xFF #xFF #x80)
(terme 'terme-output)
```
