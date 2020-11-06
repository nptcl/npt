% Escape Function

Npt documentation.

Reference: [ANSI Common Lisp npt](index.html)  
Prev: [2. Hold Variable](B2_Hold.html)  
Next: [4. Registering Functions](B4_Registering.html)


# 3.1 Escape Function

An escape function is a function from which an escape may take place.
In the previous chapter, the escape function is a function that may cause `error`.
But escape does not mean only `error`.

An escape is an instruction that interrupts the current execution
and forces the stack frame to terminate.
Five factors exist for escaping

- `tagbody` / `go`
- `block` / `return-from`
- `catch` / `throw`
- `handler-case`
- `restart-case`

In other words, a major change in the flow of execution
and a jump to another stack frame is called an escape.  
Escape is like `setjmp`/`longjmp` in C or `try`/`catch` in C++.  
However, npt does not use these operators, it just exits the escape function.

An escape function is not referring to some technique,
but rather a rule for creating functions in C.  
Common Lisp behavior is achieved by following the rules described below.


# 3.2 Creating an Escape Function

Here is an example of how to create an escape function.

```c
int test(void)
{
    lisp_format8_(NULL, "Start~%", NULL);
    lisp_funcall8_(NULL, "TEST-THROW", NULL);
    lisp_format8_(NULL, "End~%", NULL);

    return 0;
}
```

The name of Escape functions end with an underscore `_`,
so the `lisp_format8_` and `lisp_funcall8_`
in the example statement are escape functions.  
The `test` function ignores all the return values of the escape function,
but it should be handled correctly.
The following is a rewrite of the correct escape function.


```c
int test_(void)
{
    if (lisp_format8_(NULL, "Start~%", NULL))
        return 1;
    if (lisp_funcall8_(NULL, "TEST-THROW", NULL))
        return 1;
    if (lisp_format8_(NULL, "End~%", NULL))
        return 1;

    return 0;
}
```

By executing `return 1` when each of the escape functions returned a value,
the correct escape function `test_` was processed.

Suppose that the `test-throw` function is defined as follows.


```lisp
(defun test-throw ()
  (throw 'hello 999))
```

When `test_` executes `lisp_funcall8_` and calls `test-throw`,
the `lisp_funcall8_` function returns `1`
because `throw` is executed if the corresponding `catch` is present.


Then the `test_` function is executed as it is and the `return 1` is executed,
so the next `format` statement is not executed and the `test_` function ends.

This is how the escape is achieved.


# 3.3 Escape with `lisp_push_control`

Suppose that `lisp_push_control` is used in the usual function `test`.

```c
int test(void)
{
    addr control, v;

    lisp_push_control(&control);
    v = Lisp_hold();
    lisp_format8_(NULL, "Start~%", NULL);
    lisp_funcall8_(v, "TEST-THROW", NULL);
    lisp_format8_(NULL, "End: ~A~%", v, NULL);
    lisp_pop_control_(control);

    return 0;
}
```

When rewriting the escape function, statements surrounded by
`push` / pop' should always be popped to free
the stack frame rather than `return 1` immediately if an escape occurs.  

Here's an example of a fix.

```c
int test_(void)
{
    addr control, v;

    lisp_push_control(&control);
    v = Lisp_hold();
    if (lisp_format8_(NULL, "Start~%", NULL))
        goto escape;
    if (lisp_funcall8_(v, "TEST-THROW", NULL))
        goto escape;
    if (lisp_format8_(NULL, "End: ~A~%", v, NULL))
        goto escape;
escape:
    return lisp_pop_control_(control);
}
```

Although `goto` is easy to understand in a simple case like the example,
it becomes difficult to understand in a complex syntax.  

So, we used the following rewrite in the development of npt.


```c
static int test_call_(void)
{
    addr v;

    v = Lisp_hold();
    if (lisp_format8_(NULL, "Start~%", NULL))
        return 1;
    if (lisp_funcall8_(v, "TEST-THROW", NULL))
        return 1;
    if (lisp_format8_(NULL, "End: ~A~%", v, NULL))
        return 1;

    return 0;
}

int test_(void)
{
    addr control;

    lisp_push_control(&control);
    (void)test_call_();
    return lisp_pop_control_(control);
}
```

The example sentence makes extensive use of the following syntax to determine escape.

```c
if (...)
    return 1;
```

Because this syntax is used in so many places, we created the following macro

```c
#define Return(x) {if (x) return 1;}
```

The function `test_call_` can be rewritten with this macro as follows.

```c
static int test_call_(void)
{
    addr v;

    v = Lisp_hold();
    Return(lisp_format8_(NULL, "Start~%", NULL));
    Return(lisp_funcall8_(v, "TEST-THROW", NULL));
    Return(lisp_format8_(NULL, "End: ~A~%", v, NULL));

    return 0;
}
```

If you plan to create a lot of escape functions,
you may want to consider using this simple macro, as it is very easy to use.

In summary, an escape function is one that imposes the following rule.

- Name ends with an underbar `_` (not required)
- The type of the return value of the function is `int`.
- The return value is `1` on escape, normally `0`.
- Make sure to execute the pop corresponding to push.


# 3.4 Rewrite the factorial example.

In the previous chapter [2. Hold Variable](B2_Hold.html),
we created a function to output a factorial.  
However, for the sake of explanation, we did not deal with error handling,
so we ignored all return values of the escape function.


The following is a rewrite of the correct escape function.

```c
static int fact_(addr x, addr value)
{
    addr control, y;

    if (! lisp_plus_p(value)) {
        lisp_fixnum(x, 1);
        return 0;
    }

    lisp_push_control(&control);
    y = Lisp_hold();
    if (lisp_funcall8_(y, "1-", value, NULL))
        goto escape;
    if (fact_(y, y))
        goto escape;
    if (lisp_funcall8_(x, "*", value, y, NULL))
        goto escape;
escape:
    return lisp_pop_control_(control);
}

int main_lisp(void *ignore)
{
    addr control, x, y;

    lisp_push_control(&control);
    x = Lisp_hold();
    y = Lisp_hold();
    lisp_fixnum(y, 123);
    if (fact_(x, y))
        goto escape;
    if (lisp_format8_(NULL, "fact: ~A! = ~A~%", y, x, NULL))
        goto escape;
escape:
    return lisp_pop_control_(control);
}
```

The `fact_` function in the example sentence will be complete without any further modification.
