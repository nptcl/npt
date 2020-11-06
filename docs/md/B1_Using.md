% Using Npt in C

Npt documentation.

Reference: [ANSI Common Lisp npt](index.html)  
Next: [2. Hold Variable](B2_Hold.html)


# 1.1 Introduction

npt was developed with the intention of embedding it in the C language.  
The purpose of this chapter is to show how npt can be developed as a module
and to run a simple example.

Caution.  
The functions for npt modules are still under development
and are subject to change.


# 1.2 Creating an amalgamation

The amalgamation source is always needed in development.
(Learn more about npt amalgamation, [4. Amalgamation](A4_Amalgamation.html))  
Especially the header file `lisp.h` is required.  
First, here's how to make an amalgamation from the npt source.

```
$ cd github/npt
$ cd develop/amalgamation/
$ npt --script amalgamation.lisp
```

The `amalgamation.lisp` works with all Common Lisp programs.  
If you don't have the `npt` command, do one of the following instead.

```
$ sbcl --script amalgamation.lisp
$ ccl -l amalgamation.lisp
$ clisp amalgamation.lisp
```

The file to be created is as follows

- `lisp.c`
- `lisp.h`
- `shell.c`


# 1.3 Choose how to link your npt sources

There are three major types of amalgamation.

1. Using amalgamation.
2. Use the npt source as it is.
3. Create the `lisp.a` file.

Regardless of the method, the header file `lisp.h` is needed for development,
so please run the creation of amalgamation.

The first one, using amalgamation, is to use the source file `lisp.c`.  
Although this is the easiest way to use amalgamation,
it has the disadvantage that it is very slow for the C language debugger
because the `lisp.c` file is too big.

The second one, using the npt source as-is, as the name suggests,
is to copy a set of `src/*.c` files.  
This can be done by compiling all the files except for `main.c` in npt.  
The downside is that when npt is upgraded, the npt source needs to be updated.  
It's fine if the number of npt files only increases,
but it can decrease, which makes it more complicated to manage.

The third `lisp.a` file is a collection of npt object files, `*.o`,
for development purposes.  
This method is useful and will be used to explain it.

The following are simple steps to compile these three methods.


# 1.4 Example sentences for compiling

The example sentence to be executed this time is as follows

```c
int main_lisp(void *ignore)
{
    lisp_format8_(NULL, "Hello~%", NULL);
    return 0;
}
```

This statement, when expressed in Common Lisp, is the same as the following

```lisp
(format t "Hello~%")
```

To actually compile the code, you have to include not only this,
but also the code to initialize npt.  

Here's the complete code, save it as `main.c`.

```c
/* main.c */
#include <stdio.h>
#include "lisp.h"

int main_lisp(void *ignore)
{
    lisp_format8_(NULL, "Hello~%", NULL);
    return 0;
}

static int main_argv(struct lispargv *args)
{
    /* mode */
    if (args->mode_help)
        return lisp_main_help(stdout);
    if (args->mode_version)
        return lisp_main_version(args, stdout);
    if (args->mode_degrade)
        return lisp_main_degrade(args);

    /* execute */
    args->call = main_lisp;
    lisp_argv_init(args);
    lisp_argv_run(args);

    return lisp_code? 1: lisp_result;
}

int main(int argc, char *argv[], char *env[])
{
    int result;
    struct lispargv *args;

    /* initialize */
    lisp_init();
    args = lispargv_main(argc, argv, env);
    if (args == NULL) {
        fprintf(stderr, "argv error\n");
        return 1;
    }

    /* main_argv */
    args->mode_core = 0;
    args->mode_degrade = 0;
    args->mode_standalone = 1;
    args->nocore = 1;
    args->noinit = 1;
    args->debugger = 1;
    args->debuggerp = 0;
    args->quit = 1;
    result = main_argv(args);

    /* free */
    lispargv_free(args);
    lisp_free();

    return result;
}
```


# 1.5 Using amalgamation

First of all, let me explain how to use only amalgamation.  
Put the following file in the same directory as `main.c` that you created.

- `lisp.c`
- `lisp.h`

No need for `shell.c`.

Next, you need to compile the program.  
The compilation method varies depending on the environment,
so please refer to this page, [2. Compilation](A2_Compilation.html).  
The example shows how to run it on FreeBSD.

```
$ cc lisp.c main.c -lm
$ ./a.out
Hello
$
```

# 1.6 Use the npt source as is.

Create a working directory.

```
$ mkdir $HOME/libnpt1
```

Copy `main.c` and `lisp.h`.

```
$ cp -i main.c lisp.h $HOME/libnpt1/
```

Next, copy the npt source from github.  
First, go to the npt directory.

```
$ cd github/npt
```

Copy the source.

```
$ cp -i src/*.[ch] $HOME/libnpt1/
overwrite .../main.c? (y/n [n]) n  ;; Do not overwrite by entering n.
not overwritten
$
```

Compilation.

```
$ cd $HOME/libnpt1
$ cc *.c -lm
$ ./a.out
Hello
$
```


# Create a `lisp.a` file

Create a working directory.

```
$ mkdir $HOME/libnpt2
```

Copy `main.c` and `lisp.h`.

```
$ cp -i main.c lisp.h $HOME/libnpt2/
```

Create a `lisp.a` file on github npt.  
First, move it.

```
$ cd github/npt
```

Compile and generate an object file.  
You can run the script `bsd_debug.sh` to generate the object file,
but you should remember the compile option.  
In this case, we are going to do a manual compilation.

```
$ cc -c src/*.c
```

Create a `lisp.a` file.

```
$ ar -rc lisp.a *.o
$ ar -d lisp.a main.o
```

The generated `lisp.a` is now copied.

```
$ cp -i lisp.a $HOME/libnpt2/
```

Compile.

```
$ cd $HOME/libnpt2/
$ cc main.c lisp.a -lm
$ ./a.out
Hello
$
```


# Contents of the main_lisp function

In this example sentence, it was simple because we just executed the `format`.
However, the development can be very complex and confusing
because you need to express the equivalent of a Common Lisp statement in C.

The next chapter explains in more detail how to create them.
