% Arguments

Npt documentation.

Reference: [ANSI Common Lisp npt](index.html)  
Prev: [2. Compilation](A2_Compilation.html)  
Next: [4. Amalgamation](A4_Amalgamation.html)


# 3.1 Arguments
## --help

The following message is displayed

```
$ npt --help
npt -- ANSI Common Lisp Programming Language.

USAGE:
  npt [options] [inputs] [--] [arguments]

OPTIONS:
  --help             Print this message.
  --version          Print the version infomation.
  --core             Core mode.
  --standalone       Standalone mode.
  --heap <size>      Heap memory size.
  --local <size>     Local memory size.
  --corefile <file>  Core file instead of default file used.
  --initfile <file>  Init file instead of default file used.
  --nocore           Don't load a default core file.
  --noinit           Don't load a default init file.
  --debugger         Enable debugger.
  --nodebugger       Disable debugger.
  --quit             Exit after load and eval processing.

INPUTS:
  --load <file>      Load source file.
  --script <file>    Load script file.
  --eval <cmd>       Execute command.

If inputs aren't appeared, load from a standard-input.
```


## --version

Outputs compile information for npt.  
It is not shown in the `--help`,
but there is a `--version-script` argument to make it easier to read in scripts.  
The output is almost the same, but the treatment of spaces and tabs is different.


## --core

Mode for reading core files.  
Cannot be specified with `--standalone`.
The location of the core file is specified by the `--corefile` argument.
If it is not specified, the standard core file is searched for in the following order.

```
FreeBSD / Linux
	$NPT_HOME/npt.core
	$NPT_HOME/lib/npt.core
	$HOME/.npt/npt.core
	/usr/lib/npt/npt.core
	/usr/local/lib/npt/npt.core
	/opt/npt/npt.core
	/opt/lib/npt/not/npt.core

Windows
	%NPT_HOME%\npt.core
	%NPT_HOME%\lib\npt.core
	%USERPROFILE%\npt.core
	%ProgramData%\npt\npt.core
	%PROGRAMFILES%\npt\npt.core
	%ProgramFiles(x86)%\npt\npt.core
```

If the `--nocore` argument is specified, no standard core file is read.
If the core file is not found, an error is returned.


## --standalone

Mode for creating a Lisp image.  
If this argument is specified, the image of Common Lisp
is created from the beginning without reading the core file.  
Cannot be specified with `--core`.

Because this mode is specified by default,
the argument `----standalone` can be omitted. 
Since some npt compilations do not default to `----standalone`,
this argument is intended for such special commands.


## --heap `<size>`

Specify the size of the heap region.  
The argument `<size>` specifies a decimal value.
You can give the unit of K, M, G, T, P, or E. 
For example, if a user wants to specify 1 GByte,
the following expression is used.

```
--heap 1G
```

If omitted, it is 1G.


## --local `<size>`

Specify the size of the local region.  
As with `--heap`, the size of the region can be
specified in decimal and the unit can be specified.  
If omitted, it is 512M.


## --initfile `<file>`

Specifies a Lisp file to be loaded as an initialization file at startup.  
It works the same way as ``----load``,
but this argument loads the file for the purpose of initialization.
If it is not specified,
the standard initialization file is searched in the following order.

```
FreeBSD / Linux
	$HOME/.npt/npt.lisp
	$NPT_HOME/npt.lisp
	$NPT_HOME/lib/npt.lisp
	/usr/lib/npt/npt.lisp
	/usr/local/lib/npt/npt.lisp
	/opt/npt/npt.lisp
	/opt/lib/npt/not/npt.lisp

Windows
	%USERPROFILE%\npt.lisp
	%NPT_HOME%\npt.lisp
	%NPT_HOME%\lib\npt.lisp
	%ProgramData%\npt\npt.lisp
	%PROGRAMFILES%\npt\npt.lisp
	%ProgramFiles(x86)%\npt\npt.lisp
```

If the `--noinit` argument is specified, no standard initialization file is read.
If the initialization file is not found,
the command does not read the file and moves to the next step.


## --debugger / --nodebugger

Enable/Disable the debugger.  
If enabled, the debugger is started when an error occurs.
If disabled, the program is aborted when an error occurs.
The enable/disable of the debugger is set to the `boolean` value of `lisp-system::*enable-debugger*`.


## --quit

This argument determines whether to exit or go to `eval-loop`
after processing the INPUTS argument.
If `----quit` is specified, the terminal exits.
If it is not specified, the mode is changed to
`eval-loop` and the program waits for input.


## INPUTS

INPUTS consists of the following three arguments

- `--eval <cmd>`
- `--load <file>`
- `--script <file>`

These arguments can be listed many times.


### --eval `<cmd>`

The `--eval` argument executes the next argument.  
The following is an example.

```
$ npt --eval '(format t "Hello~%")'
Hello
*
```

After the statement of `----eval` is executed, the system is waiting for input.  
If you want to exit immediately after `--eval`, the `--quit` argument is specified.


```
$ npt --quit --eval '(format t "Hello~%")'
Hello
$
```


### --load / --script

There are two ways to load a Lisp file: `--load` and `--script`.  
The command `----load` loads the file and transitions to the input mode.  
The command `----script` exits immediately after loading the file.

`----script` is similar to the combination of `--quit` and `--load`,
except that `--script` disables the Lisp debugger,
so if an error occurs, it doesn't wait for input, but exits immediately.

The `----script`, as the name implies, is intended to be run in a script,
so it will stop as little as possible when an error occurs.  

The `----script` means that `--nodebugger` and `--quit` are specified at the same time.

Here's an example.

```
$ cat > aaa.lisp
(format t "Hello~%")
^D
$ npt --script aaa.lisp
Hello
$ npt --load aaa.lisp
Hello
* (quit)
$
```


# 3.2 Arguments `--`

If a `--` argument is specified, the following arguments are
recognized as arguments to be passed to the npt program.

Arguments are stored as an array in `npt-system::*arguments*`.  

Here's an example.

```
$ npt -- 10 20 30
* npt-system::*arguments*
#("npt" "10" "20" "30")
*
```


# 3.3 Arguments for Development

The argument ``--build`` is the same as ``--standalone``.

If only the `--degrade` argument is given, the test case is executed.  
The argument `--core` and `--standalone` cannot be specified at the same time.

The `--version-script` argument outputs a tabbed representation of the contents of the `--version` argument, which can be easily read by the script.

