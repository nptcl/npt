% Start Up

Npt documentation.

Reference: [ANSI Common Lisp npt](index.html)  
Prev: [6. Escape Operations](B6_Operations.html)  
Next: [8. Paper Object](B8_Paper.html)

# 7.1 Start Up

This chapter explains how to start npt in C.  
In normal C, the `main` function is the start of execution, and in Windows, it is the `WinMain` function.  
Here's how npt is initialized in both cases.


# 7.2 Start and end

To start npt, first execute the following function

```c
void lisp_init(void);
```

This function initializes a static variable used by the C language in npt
and declares it ready to be used.  
The corresponding release process is the following functions.

```c
void lisp_free(void);
```

After the initialization process is executed,
the function pointer can be registered.
The function pointer registration is a process using the following functions, 
for example.

```c
void lisp_compiled_rest(int index, lisp_calltype_rest call);
```

Once the function pointer is executed,
it persists until it is released with `lisp_free`.


# 7.3 Reading Command Arguments

The command argument is the `argc`, `argv`, and `env` of the `main` function.  
The function used for reading is as follows.

```c
struct lispargv *lispargv_main(int argc, char *argv[], char *env[]);
```

In case of an error, null is returned.  
Since the memory is dynamically allocated,
the following release process is required at the time of exit.

```c
void lispargv_free(struct lispargv *ptr);
```

An example is shown below.

```c
int main(int argc, char *argv[], char *env[])
{
    struct lispargv *args;

    lisp_init();
    args = lispargv_main(argc, argv, env);
    if (args == NULL) {
        fprintf(stderr, "argv error\n");
        return 1;
    }

    /* Here's the code for npt. */

    /* free */
    lispargv_free(args);
    lisp_free();

    return 0;
}
```

The function `lispargv_main` ignores `argc`, `argv`, and `env` on Windows,
and forces the following functions to be executed instead.

```c
struct lispargv *lispargv_windows(void);
```

This function is equivalent to the `lispargv_main`, but it gets the arguments and environment variables from the WIN32API.  
If the function `WinMain` is used, the above function should be used
because there is no argument like `argc`.  
However, it is possible to start the program
from the `main` function even on Windows.  
Even in such a case, it forces the `lispargv_windows` function to be used
to process the character code correctly.  
If you want to run the function from the `main` function
instead of `lispargv_windows`, use the following function.

```c
struct lispargv *lispargv_main_force(int argc, char *argv[], char *env[]);
```

This function forces reading of arguments, even on Windows.

If `argc`, `argv`, and `env` are not present
or you do not want to specify them, perform as follows.

```c
args = lispargv_main_force(0, NULL, NULL);
```


# 7.4 Npt settings

This section explains how to set up Npt manually instead of from arguments.  
Here's an example of a typical `--standalone` mode configuration

```c
args->mode_core = 0;
args->mode_degrade = 0;
args->mode_standalone = 1;
args->nocore = 1;
args->noinit = 1;
args->debugger = 1;
args->debuggerp = 0;
args->quit = 1;
```

To change the size of the memory, do the following

```c
args->heap = 1024UL * 1024UL*1024UL;  /* 1G */
args->local = 256UL * 1024UL*1024UL;  /* 256M */
```

`heap` is the size of the heap area.  
`local` is the size of the stack region.  
Both are allocated by `malloc` in the initial stage of npt execution.

To summarize the above, the following is a typical `main` function.

```c
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

Next, the function `main_argv` is created.


# 7.5 Show Help

If the argument is something about the display, such as ``--help``,
it prints the information and exits the execution.

```c
static int main_argv(struct lispargv *args)
{
    if (args->mode_help)
        return lisp_main_help(stdout);
    if (args->mode_version)
        return lisp_main_version(args, stdout);
    if (args->mode_degrade)
        return lisp_main_degrade(args);
 
   /* Create the content here in the next chapter. */

    return 0
}
```

The following arguments are now supported

- `--help`
- `--version`
- `--version-script`
- `--degrade`


# 7.6 Executing `main_lisp`

The next step is to build the npt environment.  
The following functions are used.

```c
int lisp_argv_init(struct lispargv *ptr);
int lisp_argv_run(struct lispargv *ptr);
```

The `lisp_argv_init` function is used to prepare
and run Lisp by the `lisp_argv_run` function.  
As shown in the previous examples, set the `main_lisp` function to `args->call`.  
The following is an example.

```c
 int main_lisp(void *call_ptr)
{
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
    args->call_ptr = NULL;
    lisp_argv_init(args);
    lisp_argv_run(args);

    return lisp_code? 1: lisp_result;
}
```

The return value of `lisp_argv_init` and `lisp_argv_run` is returned 
whether an error occurred or not.
However, since the variable `lisp_code` is set to show
whether an error occurred or not, you only have to check this value.


The `main_lisp` function registered at `args->call` is called
with `args->call_ptr` as an argument.  
The reason why you need to register them with function pointers is that
you need to set various settings before executing them
in order for them to work properly as Common Lisp.  
One example is the handling of `handler-bind` used in the `warn` function.

The `main_lisp` function is an escape function,
which is a little different from the usual escape function.
If the function returns a non-zero value on non-escape,
it assumes an abnormal exit and ignores all subsequent operations
and exits with the exit code `1`.


# 7.7 Summary

To summarize the content,
here's the first example sentence [1. Using Npt in C](B1_Using.html).

```c
int main_lisp(void *call_ptr)
{
    return lisp_format8_(NULL, "Hello~%", NULL);
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
    args->call_ptr = NULL;
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

The results are as follows.

```
Hello
```
