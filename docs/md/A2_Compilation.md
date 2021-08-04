% Compilation

Npt documentation.

Reference: [ANSI Common Lisp npt](index.html)  
Prev: [1. Installation](A1_install.html)  
Next: [3. Arguments](A3_Arguments.html)


# 2.1 Compile manually

Compiling all the `*.c` files in the `src` directory creates the executable file.  
A simple example is as follows.

```
$ cc src/*.c -lm
$ ./a.out --version
npt Version 1.0.2
...
Lisp mode            ANSI-C
...
```

However, if you compile in this way,
the program will be compiled in `ANSI-C` mode,
which is a feature-reduced mode.
Because of the reduced functionality,
It is not possible to use all the features of Common Lisp.

To use all the features, you need to specify the environment type at compile time.
The available environments are as follows

| Environment| #define | Features |
| --- | --- | --- |
| FreeBSD | `LISP_FREEBSD` | All available |
| Linux | `LISP_LINUX` | All available |
| Windows | `LISP_WINDOWS` | All available |
| ANSI-C | `LISP_ANSIC` (default) | Reduced |

When compiling with FreeBSD environment, run the following

```
$ cc -DLISP_FREEBSD src/*.c -lm
$ ./a.out --version
npt Version 1.0.2
...
Lisp mode            FreeBSD
...
```


# 2.2 Debug and Release

In a normal compilation, npt is created in release mode.
If you specify `LISP_DEBUG`, then the npt is compiled in debug mode.

For example

```
$ cc -DLISP_DEBUG -DLISP_FREEBSD src/*.c -lm
$ ./a.out --version
npt Version 1.0.2
...
Release mode         debug
...
```

The difference between the debug and release modes is the number of checks.  
In the debug mode, there are codes of checks everywhere
and if the checks are violated, the program is forced to stop.

Release mode, on the other hand,
runs faster than debug mode because of its fewer checks.

You don't need debug mode to use it as Lisp.
If you are going to embed npt into C,
it is better to specify `LISP_DEBUG` at least at the development stage.


# 2.3 editline / readline

`editline` and `readline` are modules
that are used to receive input from the prompt.  
`editline` is installed by default on FreeBSD.
`readline` is not included by default, but `readline` is often used on Linux.

The values of `#define` are as follows.

| Module | #define | Link |
| --- | --- | --- |
| terme | `LISP_TERME` |  |
| editline | `LISP_EDITLINE` | -ledit |
| readline | `LISP_READLINE` | -lreadline |
| stdin | `LISP_STDIN`|  |

Terme is a feature of the npt prompt.  
It is used by default on FreeBSD and Linux.

The editline and readline are external modules that
require installation to use.

stdin simply reads from standard input.  
It cannot use history or cursor movement.

Here's an example of a compilation.

```
$ cc -DLISP_FREEBSD -DLISP_EDITLINE src/*.c -lm -ledit
$ ./a.out --version
npt Version 1.0.2
...
Prompt mode          editline
...
```


# 2.4 Local memory implementation

Local memory is a memory stack which is different from the heap area.
The memory in the heap region is allocated in bulk at startup,
but local memory allows you to specify how to allocate memory.

Normally, allocating local memory as well as heap is done in one step.  
If you specify `LISP_MEMORY_MALLOC` at compile time,
local memory allocation will be done by `malloc` each time it is requested.

Bulk allocation is faster,
and `LISP_MEMORY_MALLOC` seems to be a little slower.  
If `LISP_MEMORY_MALLOC` is specified,
`Debug Memory true` appears in the `--version` of the `npt` command.


# 2.5 Garbage collector forced mode

The garbage collector is a function to clean out the memory in the heap area.

`npt` monitors the usage of the heap area and
if it is determined that the memory is being overwhelmed,
the garbage collector will be executed at some point.

If `LISP_DEBUG_FORCE_GC` is given at compile time,
then it will go into garbage collector forced execution mode and
the garbage collector will run at all possible times.

This mode is very slow.  
The significance of this mode is to check
if memory corruption occurs while developing in C language.


# 2.6 Windows ANSI-C mode

If you use ANSI-C mode on Windows,
there is a problem that ANSI C language features are
not enough to handle Unicode filenames.

In such a case, use `LISP_ANSIC_WINDOWS` mode instead of `LISP_ANSIC`.
This mode uses `_wfopen` instead of `fopen` to open files,
so it can handle Unicode filenames without problems.


# 2.7 The main function of Windows

On Windows, you can choose whether the startup function is set to `main` (the standard C language) or `WinMain` (Win32API).
`mpt` uses the `main` function usually, but you can change it by defining.


| Function | #define |
| --- | --- |
| `main` | LISP_CONSOLE (default) |
| `WinMain` | LISP_WINMAIN |

It just switches the startup function and is not different as a function.


# 2.8 Compilation in C++


The npt source code can be compiled with a C++ compiler.  
The compilation is done as follows

```
$ c++ -Wno-deprecated src/*.c
```

The argument, `-Wno-deprecated`, is used to suppress the warning
when compiling the file `*.c` with the C++ compiler.
Confirmation can be done with `*features*`.

```
$ ./a.out
*features*
(:LONG-FLOAT-80 :CPLUSPLUS :MATH-INACCURACY :NPT-64-BIT :NPT :64-BIT
 :ARCH-64-BIT :NPT-ANSI-C :ANSI-C :COMMON-LISP :ANSI-CL)
```

The `*features*` contains `:CPLUSPLUS`.  
Although there is almost no difference between C compiler and C++ compiler,
some parts of the code using `setjmp` are changed to `try / catch`.


# 2.9 Degrade mode

Degrade mode is a mode for testing in C.
Although this mode is activated by specifying `LISP_DEGRADE`,
it is not possible to compile simply
because the source files are not only in `src`, but also in `test`.

Normally, you'll rarely need this mode.
If you want to compile it, consider using the `debug`
specification such as `bsd_debug.sh`.


# 2.10 The maximum value of the function number

The function number is a number to register the function pointer of C language.
The number of function pointers that can be registered is 32 by default,
but can be changed by specifying `LISP_POINTER_EXTEND`.
The following is an example of how to change
the number of function numbers to 128.

```
$ cc -DLISP_POINTER_EXTEND=128 src/*.c -lm
```

In this example, the range of function numbers is 0-127.
For more information on how to use function numbers, please refer to
[4. Registering Functions](B4_Registering.html).
