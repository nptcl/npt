% Registering Functions

Npt documentation.

Reference: [ANSI Common Lisp npt](index.html)  
Prev: [3. Escape Function](B3_Escape.html)  
Next: [5. LISP ABORT](B5_Abort.html)


# 4.1 Registering Functions

This chapter describes how to implement Common Lisp functions in C alone.

Usually, functions are created by `lambda` or `defun`,
but in this chapter, we will create functions only in C.
However, the function type is different: it is not a `FUNCTION` type,
but a `COMPILED-FUNCTION` type like standard functions like `car`.

The type of the function is checked as follows.

```
* (lambda ())
#<FUNCTION LAMBDA #x801256a00>
* #'car
#<COMPILED-FUNCTION CAR>
*
```


# 4.2 Registering function pointers

To use a C language function,
it is necessary to register a function pointer with a number.
The number of function pointers that can be registered is up to 32.  
If it's not enough, define `LISP_POINTER_EXTEND` at compile time.
As an example, here's how to run a compilation with 128 extensions

```
$ cc -DLISP_POINTER_EXTEND=128 src/*.c -lm
```

The instruction to register the function pointer is shown below.

```c
void lisp_compiled_rest(int index, lisp_calltype_rest call);
void lisp_compiled_empty(int index, lisp_calltype_empty call);
void lisp_compiled_var1(int index, lisp_calltype_var1 call);
void lisp_compiled_var2(int index, lisp_calltype_var2 call);
void lisp_compiled_var3(int index, lisp_calltype_var3 call);
```

The argument `index` is a number to register, usually from 0 to 31.  
The argument `call` is the pointer to the function of the escape function.  
The function to be used depends on the argument.

The generic one is `lisp_compiled_rest`,
and `lisp_calltype_rest` is of type `int (*)(addr)`.  
The type `rest` means `(&rest list)` of lambda list.  
The type `empty` means no argument, `var1` means one,
`var2` means two, and `var3` means three arguments.  
It is an escape function to register.


# 4.3 Creating a Function Object

The instructions for creating a function object are as follows

```c
int lisp_compiled_function_(addr x, int index, addr symbol);
int lisp_compiled_function8_(addr x, int index, const void *str);
int lisp_compiled_function16_(addr x, int index, const void *str);
int lisp_compiled_function32_(addr x, int index, const void *str);
```

Because these functions are executed on Common Lisp,
they are different from function pointer registration functions
and cannot be executed until control is passed to the `main_lisp` function.

As an example, consider the following escape function in Common Lisp.

```c
int function_test_(addr list)
{
    return lisp_format8_(NULL, "TEST = ~A~%", list, NULL);
}
```

At first, the function pointer of `function_test_` is registered in function number 0.

```c
lisp_compiled_rest(0, function_test_);
```

Now that a function can be created,
the steps to create a function object are as follows

```c
lisp_compiled_function_(x, 0, NULL);
```

Create the function `test_output_` to run and check it.

```c
int test_output_(void)
{
    addr control, x;

    lisp_push_control(&control);
    x = Lisp_hold();
    lisp_compiled_function_(x, 0, NULL);
    return lisp_pop_control_(control);
}
```

The function object is now stored in the `x` hold variable.  
Further, the function object is now executed with `funcall`.

```c
int test_output_(void)
{
    addr control, x, y, z;

    lisp_push_control(&control);
    x = Lisp_hold();
    y = Lisp_hold();
    z = Lisp_hold();
    lisp_compiled_function_(x, 0, NULL);
    lisp_fixnum(y, 10);
    lisp_fixnum(z, 20);
    lisp_funcall_(NULL, x, y, z, NULL);
    return lisp_pop_control_(control);
}
```

Escape is ignored.

The function `test_output_` is expressed in Common Lisp as follows.

```lisp
(funcall #<COMPILED-FUNCTION NIL> 10 20)
```

The results are as follows.

```
TEST = (10 20)
```

The registered function works.


# 4.4 Registering a function object

The following is an example of the behavior of `defun`.  
Register a function object in the `symbol-function`.

The functions used for registration are as follows

```c
int lisp_compiled_defun_(int index, addr symbol);
int lisp_compiled_defun8_(int index, const void *str);
int lisp_compiled_defun16_(int index, const void *str);
int lisp_compiled_defun32_(int index, const void *str);
```

As an example, register the function `fact_`,
which is a factorial function created previously, under the name `FACT`.  
The function `fact_`, completed in the previous chapter, is as follows.

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
```

Although the function `fact_` is completed as an escape function,
it cannot be registered as it is because it is not made for registration.  
So, here is the function `function_fact_` for registration.  
The function calls the function `fact_` and sets the return value.

```c
int function_fact_(addr var)
{
    addr control, x;

    lisp_push_control(&control);
    x = Lisp_hold();
    if (fact_(x, var))
        goto escape;
    lisp_set_result_control(x);
escape:
    return lisp_pop_control_(control);
}
```

The function `lisp_set_result_control` is
used to set the return value of the function.  
The next step is the registration process.

```c
int main_lisp(void *ignore)
{
    lisp_compiled_var1(1, function_fact_);
    lisp_compiled_defun8_(1, "FACT");

    return lisp_eval_loop_();
}
```

The registration is done using `lisp_compiled_var1`
so that it takes only one argument.  
The function number is 1.  
Next, it runs `lisp_compiled_defun8_`
and registers the function under the name `FACT`.  
Finally, run `lisp_eval_loop_` and call eval-loop for confirmation.

When prompted, run the `fact` function.

```
$ ./a.out
* (fact 123)
1214630436702532967576624324188129585545421708848338231532891816182923
5892362167668831156960612640202170735835221294047782591091570411651472
186029519906261646730733907419814952960000000000000000000000000000
* (/ (fact 123) (fact 121))
15006
* ^D
$
```

Since the function `fact` works as a function of Lisp,
it can be incorporated into the expression.


# 4.5 Use a closure.

The closure here is the ability to store values in a function object.  
A function object can have a single value.  
To set the value, use the following instructions

```c
void lisp_compiled_setvalue(addr pos, addr value);
```

Here is an example of storing the value 10 in a closure
after creating a function object

```c
lisp_compiled_function_(x, 0, NULL);
lisp_fixnum(value, 10);
lisp_compiled_setvalue(x, value);
```

The value can be obtained by using the following instructions
when the function that registered the function pointer is called.

```c
void lisp_compiled_getvalue(addr *ret);
```

This function has a few things to be careful about.  
The value must be fetched before doing `lisp_push_control`.  
Since this function does not allow to specify a hold variable,
it receives the object directly.

The following is an example of storing a value in the hold variable

```c
int function_test_(addr list)
{
    addr control, x;

    lisp_compiled_getvalue(&x);
    lisp_push_control(&control);
    lisp_hold(&x, x);
    ...
    return lisp_pop_control_(control);
}
```


# 4.6 Why assign to a number?

We explained that when the function pointer is registered,
it is assigned to a number from 0 to 31.  

For example, do the following.

```c
lisp_compiled_rest(0, function_test_);
```

So why do we assign them to numbers?  
The reason is to read and write the core file.

Initially, the core file was supposed to
contain the function pointer values as they are.  
However, due to the recent security situation,
a feature called ASLR (Address Space Layout Randomization)
was implemented in the operating system,
which allows the function pointer to change randomly
every time a process is started.  
If a function pointer value is written to the core file,
it will not work properly when the next process is
started because it will point to a completely different address.

Numbering is also used inside npt,
where the `lisp_init` function sets the numbering of all functions,
including the Common Lisp functions.  
You can register function pointers at any time before the function is executed,
but it is better to register them as soon as possible.

On the other hand, the following function

```c
lisp_compiled_defun8_(0, "TEST");
```

This is a feature of Common Lisp to create a `compiled-function` object.  
Because it is not initialization,
it can only be executed after the `main_lisp` function.
