% Hold Variable

Npt documentation.

Reference: [ANSI Common Lisp npt](index.html)  
Prev: [1. Using Npt in C](B1_Using.html)  
Next: [3. Escape Function](B3_Escape.html)


# 2.1 Basic Creation Methods

This chapter shows the basics of developing in C using the npt module.  
The main content is the use of the Hold Variable.


# 2.2 About Errors

Errors are a part of development that cannot be ignored.  
However, dealing with errors is difficult and
should be explained in detail outside of this chapter.  
This chapter covers only the basics of creation and
does not deal with error handling.  
Let's see what happens if an error occurs.  
As an example, here is the following statement

```c
int main_lisp(void *ignore)
{
	lisp_error8_("Hello", NULL);
	return 0;
}
```

The `lisp_error8_` function executes a `simple-error` condition,
which is almost equivalent to the following statement in Common Lisp.

```lisp
(error "Hello")
```

The results of execution are as follows.

```
$ ./a.out
ERROR: SIMPLE-ERROR
Hello

There is no restarts, abort.


**************
  LISP ABORT
**************
$
```

A `SIMPLE-ERROR` was detected, a message was printed,
and `restart` was not found, so it was `ABORT`.  
`LISP ABORT` means that the process has been killed.
`SIMPLE-ERROR` should be handled properly, but that's enough for this chapter.
For the rest of the chapter, it is assumed that
an error is `LISP ABORT`ed when it occurs.


# 2.3 Explanation with examples

To illustrate the basic idea of development,
here is a function of the factorial as an example.  
A factorial is one that looks like this

```
6! = 6*5*4*3*2*1
```

The factorial of 6 is 720.  
In Common Lisp, you can easily create the following

```lisp
(defun fact (x)
  (if (not (plusp x))
    1
    (* x (fact (1- x)))))
```

It can be implemented in C.

```c
int fact(int x)
{
    if (x <= 0)
        return 1;
    else
        return x * fact(x - 1);
}
```

But because there is no `bignum` in C, for example,
`fact(123)` cannot be executed correctly.  
For reference, the factorial of 123 is as follows.

```
1214630436702532967576624324188129585545421708848338231532891816182923
5892362167668831156960612640202170735835221294047782591091570411651472
186029519906261646730733907419814952960000000000000000000000000000
```

The purpose of this chapter is to create a code that
also supports `bignum` in C and to show how to use the npt module.


# 2.4 Hold Variables

Common Lisp objects use an object called a "hold variable" to handle their values.  
A hold variable is like a lexical variable,
a Lisp object designed to be easy to handle in C. 
The purpose here is to learn how to handle these variables.

To use the Hold Variable, declare a start and end,
allocate and release the stack frame. 
The following functions are used.

- `lisp_push_control`
- `lisp_pop_control_`

The `lisp_push_control` declares the start of the hold variable
and creates a new region on the stack frame.  
The `lisp_pop_control_` declares the end of the hold variable
and releases the corresponding hold variable.

An example use case is shown below.

```c
/* variable declaration */
addr control, x, y;

lisp_push_control(&control);
x = Lisp_hold();  /* Allocate the hold variable */
y = Lisp_hold();  /* Allocate the hold variable */
lisp_pop_control_(control);
```

As shown in the example, the `Lisp_hold` function is used to
allocate the `x` and `y` hold variables.
The following example shows the code to display the resulting value,
assuming that the function `fact_` to find the factorial has already been created.

```c
int main_lisp(void *ignore)
{
    addr control, x, y;

    lisp_push_control(&control);
    x = Lisp_hold();
    y = Lisp_hold();
    lisp_fixnum(y, 123);
    fact_(x, y);
    lisp_format8_(NULL, "fact: ~A! = ~A~%", y, x, NULL);
    lisp_pop_control_(control);

    return 0;
}
```

The function `Lisp_hold` assigns the value of the hold variable to the variable `x` and `y`.  
The `lisp_fixnum` function assigns the value `123` to the hold variable `y`.  
The `fact_` function assigns the hold variable `x` to the factorial of `123`.  
The function `lisp_format8_` executes a `format` statement.  
The next section describes how to create the function `fact_`.


# 2.5 Creating `fact_` Functions

Before creating functions, here are some rules for function names.  
Some function names may or may not end in an underscore,
such as `fact_` or `lisp_format8_`.

Simply put, it has an underscore for a possible `error`.  
Such a function is called "escape function".  
A detailed explanation of escape is given in another chapter [3. Escape Function](B3_Escape.html).  
Since the naming rules are voluntary, it is optional whether or not to add an underbar.

Here is the contents of the `fact_` function.

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
    lisp_funcall8_(y, "1-", value, NULL);
    fact_(y, y);
    lisp_funcall8_(x, "*", value, y, NULL);
    lisp_pop_control_(control);

    return 0;
}
```

In the first `lisp_plus_p` statement, if the value of the variable `value` is `0` or less, the hold variable `x` is assigned `1` and then the function is terminated.

If the value of the variable `value` is greater than or equal to `1`, then `lisp_push_control` and thereafter are executed.

The hold variable `y` is allocated and the value is subtracted by `1` from `value`.  
The function `lisp_funcall8_` executes a function represented by a string.

Then the function `fact_` is recursively called using the hold variable `y` and the result is assigned to the variable `y` itself.

The result is multiplied by the value of the variable `value`, and the result is returned as the return value of `fact_`.

Finally, the hold variable is released by `lisp_pop_control_`.


# Executing the Code of Factorials

In summary, it is as follows.

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
    lisp_funcall8_(y, "1-", value, NULL);
    fact_(y, y);
    lisp_funcall8_(x, "*", value, y, NULL);
    lisp_pop_control_(control);

    return 0;
}

int main_lisp(void *ignore)
{
    addr control, x, y;

    lisp_push_control(&control);
    x = Lisp_hold();
    y = Lisp_hold();
    lisp_fixnum(y, 123);
    fact_(x, y);
    lisp_format8_(NULL, "fact: ~A! = ~A~%", y, x, NULL);
    lisp_pop_control_(control);

    return 0;
}
```

The results are shown below.

```
$ ./a.out
fact: 123! = 121463043670253296757662432418812958554542170884833823153
2891816182923589236216766883115696061264020217073583522129404778259109
1570411651472186029519906261646730733907419814952960000000000000000000
000000000
```

That's all about how to use the hold variable by `lisp_push_control`,
`lisp_pop_control_` and `Lisp_hold`.
