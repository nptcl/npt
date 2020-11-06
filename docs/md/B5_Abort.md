% LISP ABORT

Npt documentation.

Reference: [ANSI Common Lisp npt](index.html)  
Prev: [4. Registering Functions](B4_Registering.html)  
Next: [6. Escape Operations](B6_Operations.html)


# 5.1 LISP ABORT

`LISP ABORT` is a forced termination due to a system error.  
The C language `exit` function is called with the exit code `1`
to terminate the process.

If you want to run abort manually, run the following function.

```c
lisp_abort();
```

The result is as follows.

```
$ ./a.out


**************
  LISP ABORT
**************
$ echo $?
1
$
```

When the statement `LISP ABORT` is printed, as in the runtime example,
it means that the process has been forced to terminate.  
The exit code is `1`.  
The message at the end can be added with the following functions.

```c
void lisp_abortf(const char *fmt, ...);
void lisp_abort8(const void *fmt, ...);
void lisp_abort16(const void *fmt, ...);
void lisp_abort32(const void *fmt, ...);
```

Functions with 8, 16, or 32 in their names are used to create messages with the `lisp_string8_` function.  
Because it goes through `format`, Common Lisp must be running.

The function `lisp_abortf` takes a format of `printf` as an argument.  
Because it does not go through the `format`, Common Lisp is not required to be running.

However, the `printf` cannot take a Lisp object as its argument.
(If you could, I think it would be about `%p`.)  


# 5.2 Handler Settings

The `lisp_abort` can control the behavior of a handler by registering them.  
For example, let's set up the following handlers.

```c
void test_handler(void)
{
	printf("Hello Handler.\n");
}
```

The function `lisp_set_abort_handler` is used to register the handler.  
For example, consider the following example.

```c
lisp_set_abort_handler(test_handler);
lisp_abort();
```

The results are shown below.

```
$ ./a.out
Hello Handler.


**************
  LISP ABORT
**************
$
```

After the handler was started, a `LISP ABORT` occurred
and the process was forced to terminate.  
Even though the handler has been registered,
the interruption process will continue and `LISP ABORT` will be executed.


# 5.3 Capturing `LISP ABORT`

The `LISP ABORT` can be continued
without terminating the process by using `setjmp`.   
To capture and continue, use the following instructions

- `lisp_set_abort_setjmp_handler`
- `Lisp_abort_Begin`
- `Lisp_abort_End`

`lisp_set_abort_setjmp_handler` sets the handler for capture.  
`Lisp_abort_Begin` and `Lisp_abort_End` are macros to enclose the capturing process.

The following is an example of capturing `LISP ABORT`.

```c
int main(void)
{
    lisp_set_abort_setjmp_handler();
    Lisp_abort_Begin {
        printf("Start\n");
        lisp_abort();
        printf("End\n");
    }
    Lisp_abort_End;

    printf("Return\n");

    return 0;
}
```

The results are as follows.

```
$ ./a.out
Start
Return
$
```

The `Start` and `Return` are shown in the execution result,
but the `End` is not shown.  
This is because the `lisp_abort` function caused
the abort and the process jumped to `Lisp_abort_End`.  
With the above method, it's hard to tell if the process is a normal end,
or if it's a `LISP ABORT` occurrence.  

So, change it as follows

```c
void main_call(void)
{
    printf("Start\n");
    lisp_abort();
    printf("End\n");
}

int main(void)
{
    int finish;

    lisp_set_abort_setjmp_handler();
    finish = 0;
    Lisp_abort_Begin {
        main_call();
        finish = 1;
    }
    Lisp_abort_End;

    if (finish == 0)
        printf("Lisp Abort\n");

    return 0;
}
```

A variable `finish` is prepared and it is assumed to be finished successfully
if the value is changed at the end of `Lisp_abort_Begin`.  
The results are shown below.

```
$ ./a.out
Start
Lisp Abort
$
```


# Use in C++

The handler for `LISP ABORT` is `setjmp` in C,
but when compiled in C++ it is changed to `try`/`atch`.  
The reason for the change is that the destructor will not be invoked
if it is `setjmp`.  
As an example, let's try to run the following statement in C++.

```cpp
class destruct
{
public:
    destruct() { printf("Constructor\n"); };
    ~destruct() { printf("Destructor\n"); };
};

void main_call(void)
{
    destruct x;

    printf("Start\n");
    lisp_abort();
    printf("End\n");
}

int main(void)
{
    int finish;

    lisp_set_abort_setjmp_handler();
    finish = 0;
    Lisp_abort_Begin {
        main_call();
        finish = 1;
    }
    Lisp_abort_End;

    if (finish == 0)
        printf("Lisp Abort\n");

    return 0;
}
```

An example is shown below.

```
$ ./a.out
Constructor
Start
Destructor
Lisp Abort
$
```

In this example, the destructor is invoked
because `try`/`catch` is used instead of `setjmp`.  
However, if `LISP_ABORT_SETJMP` is defined at compile time,
C++ compilation will also use `setjmp`.  
The following is the result of executing the above example with `setjmp`.

```
$ ./a.out
Constructor
Start
Lisp Abort
$
```

Since the `lisp_abort` function is executed,
the destructor is ignored because it transitions directly to `Lisp_abort_End`.
