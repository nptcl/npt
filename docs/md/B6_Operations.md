% Escape Operations

Npt documentation.

Reference: [ANSI Common Lisp npt](index.html)  
Prev: [5. LISP ABORT](B5_Abort.html)  
Next: [7. Start Up](B7_StartUp.html)


# 6.1 Escape Operations

This section describes the operation of the escape.  
In the previous explanation, there were five reasons for escaping

- `tagbody`/`go`
- `block`/`return-from`
- `catch`/`throw`
- `handler-case`
- `restart-case`

The `tagbody`/`go` and `block`/`return-from` are intended for
use in lexical environments and are not available in C.

The remaining three operations can be implemented in C.  
The escape operation must be considered not only to generate and capture,
but also to interrupt.  
Escape interrupt is `unwind-protect`.

In summary, this chapter describes the following features.

- `catch`/`throw`
- `handler-case`
- `restart-case`
- `unwind-protect`


# 6.2 `catch`/`throw`

The easiest escape operation is `catch`/`throw`.  
The `catch` registers a `symbol` to a stack frame.  
In other words, `lisp_push_control` must be executed.  
The following function is used for registration.

```c
void lisp_catch(addr symbol);
```

The following function is used for `throw`.

```c
int lisp_throw_(addr symbol);
```

The `throw` function goes backward from the current stack frame and
escape if the corresponding `catch` argument is present.  
If there is no `catch` for the argument, an error occurs.  
In both cases, the return value is non-zero without judgment.

The following function is used to check
if the current stack frame has reached the escape point.

```c
int lisp_break_control(void);
```

True (non-zero) is returned if the current escape is in progress
and the current stack frame is at the destination point.  
In case of `catch`/`throw`, when the function is true,
the function switches to non-escape and the `throw` is completed.

The function to switch to non-escape is as follows.

```c
void lisp_reset_control(void);
```

The summary of the procedure is as follows.

- Create a stack frame by `lisp_push_control`.
- Register a `symbol` by `lisp_catch`.
- Escape by `lisp_throw_`
- Checking for the reach by `lisp_break_control`.
- If it has been reached, `lisp_reset_control` makes it a non-escape

The following statement is an example.

```lisp
(catch 'hello
  (throw 'hello 222)
  333)
```

Since `throw` returns `222`, `333` is ignored
and the total return value is `222`.

```c
int throw_hello_222_(addr x)
{
    lisp_fixnum(x, 222);
    lisp_set_result_control(x);
    if (lisp_intern8_(x, NULL, "HELLO"))
        return 1;

    return lisp_throw_(x);
}

int main_call_(addr x)
{
    addr control;

    lisp_push_control(&control);
    /* (catch 'hello ...) */
    if (lisp_intern8_(x, NULL, "HELLO"))
        goto escape;
    lisp_catch(x);
    /* (throw 'hello 222) */
    if (throw_hello_222_(x))
        goto escape;
    /* (values 333) */
    lisp_fixnum(x, 333);
    lisp_set_result_control(x);
escape:
    if (lisp_break_control())
        lisp_reset_control();
    return lisp_pop_control_(control);
}

int main_lisp(void *ignore)
{
    addr control, x;

    lisp_push_control(&control);
    x = Lisp_hold();
    if (main_call_(x))
        goto escape;
    lisp_result_control(x);
    if (lisp_format8_(NULL, "RESULT: ~A~%", x, NULL))
        goto escape;
escape:
    return lisp_pop_control_(control);
}
```

The result is `RESULT: 222`.


# 6.3 `handler-case`

The basic construction is the same as `catch`/`throw`.  
The following function is used to register the `condition`.

```c
int lisp_handler_case_(addr name, addr call);
```

And `signal` or `error` corresponds to `throw`.  
The following is an example.

```c
int main_call_(addr x)
{
    addr control, y;

    lisp_push_control(&control);
    y = Lisp_hold();
    /* handler-case */
    if (lisp_intern8_(x, NULL, "AAA"))
        goto escape;
    if (lisp_eval8_(y, "(lambda (c) (declare (ignore c)) 222)"))
        goto escape;
    if (lisp_handler_case_(x, y))
        goto escape;
    /* (signal 'aaa) */
    if (lisp_eval8_(NULL, "(signal 'aaa)"))
        goto escape;
    /* (values 333) */
    lisp_fixnum(x, 333);
    lisp_set_result_control(x);
escape:
    if (lisp_break_control())
        lisp_reset_control();
    return lisp_pop_control_(control);
}

int main_lisp(void *ignore)
{
    addr control, x;

    lisp_push_control(&control);
    x = Lisp_hold();
    if (lisp_eval8_(NULL, "(define-condition aaa () ())"))
        goto escape;
    if (main_call_(x))
        goto escape;
    lisp_result_control(x);
    if (lisp_format8_(NULL, "RESULT: ~A~%", x, NULL))
        goto escape;
escape:
    return lisp_pop_control_(control);
}
```

The result is `RESULT: 222`.



# 6.4 `restart-case`

Same as `handler-case`.

Create a `restart` object and register it to the current stack frame.  
The following functions are used to create the `restart` object.

```c
void lisp_restart_make(addr x, addr name, addr call, int casep);
```

The following function is used in order to register the `restart`.

```c
void lisp_restart_push(addr restart);
```

Examples of use are as follows.

```c
int main_call_(addr x)
{
    addr control, y;

    lisp_push_control(&control);
    y = Lisp_hold();
    /* restart-case */
    if (lisp_intern8_(x, NULL, "HELLO"))
        goto escape;
    if (lisp_eval8_(y, "(lambda () 222)"))
        goto escape;
    lisp_restart_make(x, x, y, 1);
    lisp_restart_push(x);
    /* (invoke_restart 'hello) */
    if (lisp_eval8_(NULL, "(invoke-restart 'hello)"))
        goto escape;
    /* (values 333) */
    lisp_fixnum(x, 333);
    lisp_set_result_control(x);
escape:
    if (lisp_break_control())
        lisp_reset_control();
    return lisp_pop_control_(control);
}

int main_lisp(void *ignore)
{
    addr control, x;

    lisp_push_control(&control);
    x = Lisp_hold();
    if (main_call_(x))
        goto escape;
    lisp_result_control(x);
    if (lisp_format8_(NULL, "RESULT: ~A~%", x, NULL))
        goto escape;
escape:
    return lisp_pop_control_(control);
}
```

The result is `RESULT: 222`.


# 6.5 `unwind-protect`

There are two ways to achieve `unwind-protect`.  
One is to register a function for `cleanup` to a stack frame,
and the other is to detect escape mode and do it by itself.

*The method of registering to the stack frame will be removed.  
The description will also be removed later.*


## obsolete: Registering `unwind-protect`.

*This method is obsolete.*

Normal usage of `unwind-protect` is to register a stack frame.  
The following function is used in order to register a `cleanup` form.

```c
void lisp_unwind_protect_deprecated(addr clean);
```

The argument `clean` specifies a function with no argument.  
The registered function will be called
when the function is executed with `lisp_pop_control_`.

The following is an example.

```c
int main_call_(addr x)
{
    addr control;

    lisp_push_control(&control);
    /* unwind-protect */
    if (lisp_eval8_(x, "(lambda () (setf (symbol-value 'hello) 200))"))
        goto escape;
    lisp_unwind_protect_deprecated(x);
escape:
    return lisp_pop_control_(control);
}

int main_lisp(void *ignore)
{
    addr control, x;

    lisp_push_control(&control);
    x = Lisp_hold();
    lisp_fixnum(x, 100);
    if (lisp_push_special8_("HELLO", x))
        goto escape;
    if (main_call_(x))
        goto escape;
    if (lisp_get_special8_(x, "HELLO"))
        goto escape;
    if (lisp_format8_(NULL, "RESULT: ~A~%", x, NULL))
        goto escape;
escape:
    return lisp_pop_control_(control);
}
```

The special variable `hello` is set to 100
and then set to 200 in the `cleanup` form of `unwind-protect`.  
The result is `RESULT: 200`.


## Manual execution of `unwind-protect`

The following values are required for manual execution.

- Escape information
- Returned Value Information

Escape information is the information of the current escape mode
and the stack frame of the destination.  
Returned value information is a value obtained by `list_result_control`
and is the value of `values` itself.
The value of `values` is not one, but 0 or multiple values.

Before executing `unwind-protect`, the current state is evacuated at once
and the state is rolled back after the `cleanup` form is executed.

The following functions are used to save the escape information and the return value at the same time.

```c
void lisp_save_control(addr *ret);
```

The information is stored using a stack frame as well as the hold variable.  
After saving the information,
the following functions are used to transition to non-escape.

```c
void lisp_reset_control(void);
```

This function can be executed even if it is a non-escape.

When the save is done, the `cleanup` process is carried out.  
If a new escape is created during the `cleanup` process,
the information saved by `lisp_save_control` is ignored
and the latest escape process takes precedence.

When the `cleanup` process is successfully completed,
the following function will be used to restore it.

```c
void lisp_rollback_control(addr value);
```

Finally, the command `lisp_pop_control_` is executed
to discard the stored information.

Here is an example.  
The following example shows that the returned values are saved and restored.

```c
int main_lisp(void *ignore)
{
    addr control, save, x;

    lisp_push_control(&control);
    x = Lisp_hold();
    /* 100 */
    lisp_fixnum(x, 100);
    lisp_set_result_control(x);
    /* save */
    lisp_save_control(&save);
    lisp_reset_control();
    /* 200 */
    lisp_fixnum(x, 200);
    lisp_set_result_control(x);
    /* rollback */
    lisp_rollback_control(save);
    lisp_result_control(x);
    lisp_format8_(NULL, "RESULT: ~A~%", x, NULL);
    return lisp_pop_control_(control);
}
```

The result is `RESULT: 100`.
