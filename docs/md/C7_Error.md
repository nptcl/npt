% C Function: Error

Npt documentation.

Reference: [ANSI Common Lisp npt](index.html)  


# Function Specification

The following function specifications are described in `lisp.h`.

- [1. abort](#error-1)

```c
void lisp_abort(void);
void lisp_abortf(const char *fmt, ...);
void lisp_abort8(const void *fmt, ...);
void lisp_abort16(const void *fmt, ...);
void lisp_abort32(const void *fmt, ...);
lisp_abort_calltype lisp_set_abort_handler(lisp_abort_calltype call);
lisp_abort_calltype lisp_set_abort_setjmp_handler(void);
Lisp_abort_Begin
Lisp_abort_End
Lisp_abort_throw
```

- [2. signal](#error-2)

```c
int lisp_signal_(addr condition);
int lisp_error_(addr condition);
```
- [3. error](#error-3)

```c
int lisp_error8_(const void *str, ...);
int lisp_error16_(const void *str, ...);
int lisp_error32_(const void *str, ...);
```

- [4. warn](#error-4)
   
```c
int lisp_warn8_(const void *str, ...);
int lisp_warn16_(const void *str, ...);
int lisp_warn32_(const void *str, ...);
```

# <a id="error-1">1. abort</a>

Functions of the `LISP ABORT`.

```c
void lisp_abort(void);
void lisp_abortf(const char *fmt, ...);
void lisp_abort8(const void *fmt, ...);
void lisp_abort16(const void *fmt, ...);
void lisp_abort32(const void *fmt, ...);
lisp_abort_calltype lisp_set_abort_handler(lisp_abort_calltype call);
lisp_abort_calltype lisp_set_abort_setjmp_handler(void);
Lisp_abort_Begin
Lisp_abort_End
Lisp_abort_throw
```


## Function `lisp_abort`

```c
void lisp_abort(void);
```

Executes `LISP ABORT`.  
When `LISP ABORT` is executed, the following behavior is performed.

- Running the Handler
- Output of the string `LISP ABORT`.
- Execution of `exit(1)` (process termination)


## Function `lisp_abortf`

```c
void lisp_abortf(const char *fmt, ...);

Input: fmt, `printf` format.
```

Print a message to standard error and then `LISP ABORT`.  
No Lisp is used, so it can be run at any time.


## Function `lisp_abort8`

```c
void lisp_abort8(const void *fmt, ...);
void lisp_abort16(const void *fmt, ...);
void lisp_abort32(const void *fmt, ...);

Input: fmt, unicode string.
```

Output error messages to `error-output`
using the `lisp_format_` function,and then `LISP ABORT`.  
Since the process is executed via Lisp, Common Lisp must be executable.  
See the `lisp_string8_` function for details on Unicode strings.


## Function `lisp_abort16`

See `lisp_abort8`.


## Function `lisp_abort32`

See `lisp_abort8`.


## Function `lisp_set_abort_handler`

```c
lisp_abort_calltype lisp_set_abort_handler(lisp_abort_calltype call);

Input: call, handler, void (*)(void).
Return: Handler before configuration.
```

Specifies a handler to be called when `LISP ABORT` is executed.  
If the argument is NULL, the handler will be removed.


## Function `lisp_set_abort_setjmp_handler`

```c
lisp_abort_calltype lisp_set_abort_setjmp_handler(void);

Return: Handler before configuration.
```

Set the handler for `setjmp`.


## Macro `Lisp_abort_Begin`

```c
#define Lisp_abort_Begin ...
#define Lisp_abort_End ...
#define Lisp_abort_throw() ...
```

Capture LISP ABORT.  
To use this mechanism, you need to run
`lisp_set_abort_setjmp_handler` to set the `setjmp` handler.

Use `Lisp_abort_Begin` and `Lisp_abort_End` for capturing.  
Examples are shown below.

```c
Lisp_abort_Begin {
    /* code */
}
Lisp_abort_End;
```
If `Lisp_abort_throw()` is executed,
the control jumps to `Lisp_abort_End` by `longjmp`.

The handler registered by `lisp_set_abort_setjmp_handler`
is simply a code that `Lisp_abort_throw()` will be executed,
for example, the following function.

```c
void abort_setjmp_handler(void)
{
    Lisp_abort_throw();
}
```

Normally, macros are expanded to `setjmp`/`longjmp`,
but if you have a C++ compiler and `LISP_ABORT_SETJMP` is not defined,
macros are expanded to `try`/`catch`.

A typical usage example is shown below.

```c
int main(void)
{
    int finish;
    lisp_abort_calltype handler;

    handler = lisp_set_abort_setjmp_handler();
    finish = 0;
    Lisp_abort_Begin {
        lisp_abort();
        finish = 1;
    }
    Lisp_abort_End;
    lisp_set_abort_handler(handler);

    if (finish == 0)
        printf("LISP ABORT\n");

    return 0;
}
```


## Macro `Lisp_abort_End`

See `Lisp_abort_Begin`.


## Macro `Lisp_abort_throw`

See `Lisp_abort_Begin`.


# <a id="error-2">2. signal</a>

Function to generate a condition.

```c
int lisp_signal_(addr condition);
int lisp_error_(addr condition);
```


## Escape Function `lisp_signal_`

```c
int lisp_signal_(addr condition);

Input: condition, instance.
Return: Non-zero when escaping.
```

Passes the `condition` argument to `signal`.  
It is the same as `signal` in Common Lisp.  
If `condition` is a hold variable, its contents will be used.


## Escape Function `lisp_error_`

```c
int lisp_error_(addr condition);

Input: condition, instance.
Return: Non-zero.
```

Passes the `condition` argument to `error`.  
It is the same as `error` in Common Lisp.  
If `condition` is a hold variable, its contents will be used.


# <a id="error-3">3. error</a>

Functions of the `error`.

```c
int lisp_error8_(const void *str, ...);
int lisp_error16_(const void *str, ...);
int lisp_error32_(const void *str, ...);
```


## Escape Function `lisp_error8_`

```c
int lisp_error8_(const void *str, ...);
int lisp_error16_(const void *str, ...);
int lisp_error32_(const void *str, ...);

Input: str, unicode string.
Return: Non-zero.
```

Run the `simple-error` condition.  
It is the same as `error` in Common Lisp.  
See the `lisp_string8_` function for details on Unicode strings.


## Escape Function `lisp_error16_`

See `lisp_error8_`.


## Escape Function `lisp_error32_`

See `lisp_error8_`.


# <a id="error-4">4. warn</a>

Functions of the `warn`.

```c
int lisp_warn8_(const void *str, ...);
int lisp_warn16_(const void *str, ...);
int lisp_warn32_(const void *str, ...);
```


## Escape Function `lisp_warn8_`

```c
int lisp_warn8_(const void *str, ...);
int lisp_warn16_(const void *str, ...);
int lisp_warn32_(const void *str, ...);

Input: str, unicode string.
Return: Non-zero when escaping.
```

Run the `simple-warning` condition.  
It is the same as `warn` in Common Lisp.  
See the `lisp_string8_` function for details on Unicode strings.


## Escape Function `lisp_warn16_`
 
See `lisp_warn8_`.


## Escape Function `lisp_warn32_`

See `lisp_warn8_`.
