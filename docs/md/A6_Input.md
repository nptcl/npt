% Input Module

Npt documentation.

Reference: [ANSI Common Lisp npt](index.html)  
Prev: [5. Specific Features](A5_Features.html)


# 6.1 Input Module `TERME`

`TERME` is a module to assist input, like `readline` and `editline`.

Currently, npt has external modules `readline` and `editline` available.  
These modules are not internal to npt,
so you will need to install them separately to use them.  
On the other hand, `TERME` is included in the npt source,
so you don't need to prepare anything additional.

The development policy of npt is to build Common Lisp by itself,
without relying on any modules.  
Nevertheless, `readline` and `editline` are available
simply because they are convenient.  
The purpose of the development of `TERME` is to replace these modules.

Currently, `TERME` is available for FreeBSD and Linux.


# 6.2 Using `TERME`

On FreeBSD and Linux, `TERME` is available by default.  
The following is an example of compilation on FreeBSD.

```
$ cc -o npt -DLISP_FREEBSD *.c -lm
$ ./npt --version-script | grep prompt-mode
prompt-mode     terme
```

The value of `prompt-mode` is now `terme`.

It is also possible to specify the mode explicitly at compile time,
like `readline` or `editline`.  
Define `LISP_TERME` and compile.

```
$ cc -o npt -DLISP_FREEBSD -DLISP_TERME *.c -lm
```

The following two options are available for `TERME`.

- Color mode
- Light/Dark mode

Color mode, as the name implies,
selects whether or not to add color to the prompt.  
The light/dark mode selects whether to make the colors lighter or darker.  
If the terminal you are using has a white background,
the default light mode may be difficult to read.  
In such a case, consider using the dark mode.

The default is to use color, bright mode.  
You can change these modes even after compiling.


## Color mode

The color mode can be changed with the following define values.

- `LISP_TERME_COLOR`  (Default)
- `LISP_TERME_MONOCHROME`

If you do not want to use colors,
compile with `LISP_TERME_MONOCHROME`.  
For example,

```
$ cc -o npt -DLISP_FREEBSD -DLISP_TERME_MONOCHROME *.c -lm
```

You can check the mode specification with the following command.

```
$ ./npt --version-script | grep prompt-color
prompt-color     off
```

The mode can be changed in the options of the command.  
To start with a specific color mode, do the following

```
$ npt --color
```

To start the program in monochrome mode,
without using colors, do the following

```
$ npt --monochrome
```


While running, you can check and switch modes
by manipulating the value of `npt-system:*prompt-color*`.

```lisp
* npt-system:*prompt-color*
NIL
* (setq npt-system:*prompt-color* t)
T
```


## Light/Dark mode

The light/dark modes can be specified only when the color mode is specified.  
The light/dark mode can be changed with the following define values

- `LISP_TERME_BRIGHT`  (Default)
- `LISP_TERME_DARK`

If you want to use darker colors, compile with `LISP_TERME_DARK`.  
For example

```
$ cc -o npt -DLISP_FREEBSD -DLISP_TERME_DARK *.c -lm
```

You can check the mode specification with the following command.

```
$ ./npt --version-script | grep prompt-bright
prompt-bright   dark
```

The mode can be changed with the options of the command.  
To start with a bright mode, do the following

```
$ npt --bright
```

To start with a dark mode specified, do the following

```
$ npt --dark
```

While running, you can check and switch modes by manipulating the value of `npt-system:*prompt-bright*`.

```lisp
* npt-system:*prompt-bright*
NIL
* (setq npt-system:*prompt-bright* t)
T
```


# 6.3 Operation Method

The operation method is shown below.

- Move
  - Move to the left: `Ctrl+B`, Left.
  - Move to the right: `Ctrl+F`,Right.
  - Move to the start of the text: `Ctrl+A`.
  - Move to the end of the text: `Ctrl+E`.

- Confirmation and history
  - Confirm of input: `Ctrl+J`, `Ctrl+M`, Enter, Return.
  - Previous history: `Ctrl+P`, Up cursor key.
  - Next history: `Ctrl+N`, Down cursor key.

- Delete
  - Delete: `Ctrl+D`.
  - Backspace: `Ctrl+H`, BS key.
  - Delete to stsart of the text: `Ctrl+U`.
  - Delete to end of text: `Ctrl+K`.

- Others
  - Refresh: `Ctrl+L`.
  - Break: `Ctrl+C`.
  - Stopping the process: `Ctrl+Z`.
  - `EOF`: `Ctrl+D` with no input.

Other characters are added to the input string.  
Only UTF-8 input characters are accepted.  
Any invalid UTF-8 characters are ignored.


# 6.4 Other Information

`Ctrl+D` has two functions.  
If any character is typed, it will delete the single character at the cursor.  
However, if you press `Ctrl+D` when there is no input,
it will return `EOF` and the prompt will be killed.

If you use `Ctrl+D` and type `EOF` at the `restart` selection
when the debugger is running,
it will be the same as selecting `abort` restart.  
If you type `:exit`, it will be `abort` as well.

If you type `Ctrl+D` in `eval-loop`,
you will escape from the `eval-loop`,
which will probably kill the process.  
If you don't want to exit `eval-loop`,
you can assign `t` to the `npt-system:*eval-loop-exit*` variable.  
You will get an `Exit?` prompt to confirm the exit.

An execution example is shown below.

```
$ npt
* (setq npt-system:*eval-loop-exit* t)
T
* ^D
Exit? ^D
* ^D
Exit? zzz
* ^D
Exit? y
$
```

If you want to set it at startup, it is convenient to use `initfile`.  
Create the file by the following operations.

```
$ vi $HOME/.npt.lisp
```

The file to be created will be automatically `loaded` when npt is started.  
An example is shown below.

```lisp
;; Prompt for confirmation when finished.
(setq npt-system:*eval-loop-exit* t)
;; Darken the color of the prompt.
(setq npt-system:*prompt-bright* nil)
```
