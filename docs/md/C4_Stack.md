% C Function: Stack Frame

Npt documentation.

Reference: [ANSI Common Lisp npt](index.html)  


# Function Specification

The following function specifications are described in `lisp.h`.

- [1. Stack Frame](#stack-1)

```c
void lisp_push_control(addr *ret);
int lisp_pop_control_(addr control);
```

- [2. Special Variables](#stack-2)

```c
int lisp_push_special_(addr symbol, addr value);
int lisp_push_special8_(const void *name, addr value);
int lisp_push_special16_(const void *name, addr value);
int lisp_push_special32_(const void *name, addr value);
int lisp_get_special_(addr x, addr symbol);
int lisp_get_special8_(addr x, const void *name);
int lisp_get_special16_(addr x, const void *name);
int lisp_get_special32_(addr x, const void *name);
int lisp_set_special_(addr symbol, addr value);
int lisp_set_special8_(const void *name, addr value);
int lisp_set_special16_(const void *name, addr value);
int lisp_set_special32_(const void *name, addr value);
```

- [3. defvar](#stack-3)

```c
int lisp_defvar_(addr symbol);
int lisp_defvar8_(const void *str);
int lisp_defvar16_(const void *str);
int lisp_defvar32_(const void *str);
```

- [4. catch / throw](#stack-4)

```c
void lisp_catch(addr symbol);
int lisp_throw_(addr symbol);
```

- [5. handler](#stack-5)

```c
int lisp_handler_bind_(addr name, addr call);
int lisp_handler_case_(addr name, addr call);
void lisp_handler_reverse(void);
```

- [6. restart](#stack-6)

```c
void lisp_restart_make(addr x, addr name, addr call, int casep);
void lisp_restart_interactive(addr restart, addr call);
void lisp_restart_report(addr restart, addr call);
void lisp_restart_test(addr restart, addr call);
void lisp_restart_push(addr restart);
void lisp_restart_reverse(void);
```


# <a id="stack-1">1. Stack Frame</a>

Functions for allocating and releasing stack frame space.

```c
void lisp_push_control(addr *ret);
int lisp_pop_control_(addr control);
```


## Function `lisp_push_control`

```c
void lisp_push_control(addr *ret);

Output: ret, New stack frame
```

Allocates a new stack frame.  
The stack frame is mainly used for hold variables.  


## Escape Function `lisp_pop_control_`

```c
int lisp_pop_control_(addr control);

Input: control, Stack frames to be released.
Return: Non-zero when escaping.
```

Releases the stack frame.  
The current stack frame is also held by the execution environment,
and it is an error if the argument `control` is different from
the current stack frame.  

This function can also be used for escape.  
It is an error if `control` is a hold variable.


# <a id="stack-2">2. Special Variables</a>

Functions for manipulating special variables.

```c
int lisp_push_special_(addr symbol, addr value);
int lisp_push_special8_(const void *name, addr value);
int lisp_push_special16_(const void *name, addr value);
int lisp_push_special32_(const void *name, addr value);
int lisp_get_special_(addr x, addr symbol);
int lisp_get_special8_(addr x, const void *name);
int lisp_get_special16_(addr x, const void *name);
int lisp_get_special32_(addr x, const void *name);
int lisp_set_special_(addr symbol, addr value);
int lisp_set_special8_(const void *name, addr value);
int lisp_set_special16_(const void *name, addr value);
int lisp_set_special32_(const void *name, addr value);
```

## Escape Function `lisp_push_special_`

```c
int lisp_push_special_(addr symbol, addr value);

Input: symbol, symbol.
Input: value, Object.
Return: Non-zero when escaping.
```

Adds a special variable of `symbol` to the current stack frame.  
The `value` is the initial value of the special variable,
which is `unbound` if it is null.  
If `symbol` and `value` are hold variables, the stored value will be used.


## Escape Function `lisp_push_special8_`

```c
int lisp_push_special8_(const void *name, addr value);
int lisp_push_special16_(const void *name, addr value);
int lisp_push_special32_(const void *name, addr value);

Input: name, Unicode string.
Input: value, Object.
Return: Non-zero when escaping.
```

Find the symbol `name` in the current package and add a special variable.  
The `value` is the initial value of the special variable,
which is `unbound` if it is null.  
If `symbol` and `value` are hold variables, the stored value will be used.  
See the `lisp_string8_` function for details on Unicode strings.


## Escape Function `lisp_push_special16_`

See `lisp_push_special8_`.


## Escape Function `lisp_push_special32_`

See `lisp_push_special8_`.


## Escape Function `lisp_get_special_`

```c
int lisp_get_special_(addr x, addr symbol);

Input: symbol, symbol
Output: x, Hold variable.
Return: Non-zero when escaping.
```

Get the value of the special variable.  
If the retrieved value is `unbound`, NULL is stored.  
The NULL value stored in the hold variable can be checked
with the `lisp_null_p` function.  
If `symbol` is a hold variable, its content will be used.


## Escape Function `lisp_get_special8_`

```c
int lisp_get_special8_(addr x, const void *name);
int lisp_get_special16_(addr x, const void *name);
int lisp_get_special32_(addr x, const void *name);

Input: name, Unicode string.
Output: x, Hold variable.
```

Find the symbol `name` in the current package and
get the value of the special variable.  
If the retrieved value is `unbound`, NULL is stored.  
The NULL value stored in the hold variable can be checked
with the `lisp_null_p` function.  
See the `lisp_string8_` function for details on Unicode strings.


## Escape Function `lisp_get_special16_`

See `lisp_get_special8_`.


## Escape Function `lisp_get_special32_`

See `lisp_get_special8_`.


## Escape Function `lisp_set_special_`

```c
int lisp_set_special_(addr symbol, addr value);

Input: symbol, symbol.
Input: value, Object.
Return: Non-zero when escaping.
```

Sets the value to the special variable.  
If `value` is null, `unbound` is set.  
If `symbol` or `value` is a hold variable, its content will be used.


## Escape Function `lisp_set_special8_`

```c
int lisp_set_special8_(const void *name, addr value);
int lisp_set_special16_(const void *name, addr value);
int lisp_set_special32_(const void *name, addr value);

Input: name, Unicode string.
Input: value, Object.
Return: Non-zero when escaping.
```

Find the symbol `name` in the current package and add a special variable.  
If `value` is null, `unbound` is set.  
See the `lisp_string8_` function for details on Unicode strings.


## Escape Function `lisp_set_special16_`

See `lisp_set_special8_`.


## Escape Function `lisp_set_special32_`

See `lisp_set_special8_`.


# <a id="stack-3">3. defvar</a>

Functions of the `defvar`.

```c
int lisp_defvar_(addr symbol);
int lisp_defvar8_(const void *str);
int lisp_defvar16_(const void *str);
int lisp_defvar32_(const void *str);
```


## Escape Function `lisp_defvar_`

```c
int lisp_defvar_(addr symbol);

Input: symbol, symbol.
Return: Non-zero when escaping.
```

Make `symbol` a special variable.  
If `symbol` or `value` is a hold variable, its content will be used.


## Escape Function `lisp_defvar8_`

```c
int lisp_defvar8_(const void *str);
int lisp_defvar16_(const void *str);
int lisp_defvar32_(const void *str);

Input: str, Unicode string.
Return: Non-zero when escaping.
```

Make the symbol represented by `str` a special variable.  
See the `lisp_string8_` function for details on Unicode strings.


## Escape Function `lisp_defvar16_`

See `lisp_defvar8_`.


## Escape Function `lisp_defvar32_`

See `lisp_defvar8_`.


# <a id="stack-4">4. catch / throw</a>

Functions of `catch` and `throw`.

```c
void lisp_catch(addr symbol);
int lisp_throw_(addr symbol);
```

## Function `lisp_catch`

```c
void lisp_catch(addr symbol);

Input: symbol, symbol.
```

Register a symbol for `catch` in the current stack frame.  
If `symbol` or `value` is a hold variable, its content will be used.


## Escape Function `lisp_throw_`

```c
int lisp_throw_(addr symbol);

Input: symbol, symbol.
Return: Non-zero.
```

Execute `throw` with the `symbol` argument.  
It will search back through the stack frame and
start escaping if it finds a `symbol` for `catch`.  
If `symbol` is not found, `error` will be raised.  
If `symbol` or `value` is a hold variable, its content will be used.



# <a id="stack-5">5. handler</a>

The `handler-bind` and `handler-case` functions.

```c
int lisp_handler_bind_(addr name, addr call);
int lisp_handler_case_(addr name, addr call);
void lisp_handler_reverse(void);
```


## Escape Function `lisp_handler_bind_`

```c
int lisp_handler_bind_(addr name, addr call);

Input: name, Symbol or condition.
Input: call, Function object.
Return: Non-zero when escaping.
```

Register the code for `handler-bind` in the current stack frame.  
If `name` is `symbol`, `find-class` will be called.  
`call` specifies a function object that takes one argument.  
If `name` and `call` are hold variables, the contents will be used.


## Escape Function `lisp_handler_case_`

```c
int lisp_handler_case_(addr name, addr call);

Input: name, Symbol or condition.
Input: call, Function object.
Return: Non-zero when escaping.
```

Register the code for `handler-case` in the current stack frame.  
If `name` is `symbol`, `find-class` will be called.  
`call` specifies a function object that takes one argument.  
If `name` and `call` are hold variables, the contents will be used.


## Function `lisp_handler_reverse`

```c
void lisp_handler_reverse(void);
```

Put the handler list in reverse order.  
You can register multiple `lisp_handler_bind_` and `lisp_handler_case_`,
but since they are added to the current stack frame with `push`,
the evaluation order will be reversed.
However, since they are added to the current stack frame by `push`,
the order of evaluation is not in the order of registration
but in the reverse order.  
Therefore, this function can be used to reverse the order of the handler list.


# <a id="stack-6">6. restart</a>

Functions of `restart`.

```c
void lisp_restart_make(addr x, addr name, addr call, int casep);
void lisp_restart_interactive(addr restart, addr call);
void lisp_restart_report(addr restart, addr call);
void lisp_restart_test(addr restart, addr call);
void lisp_restart_push(addr restart);
void lisp_restart_reverse(void);
```


## Function `lisp_restart_make`

```c
void lisp_restart_make(addr x, addr name, addr call, int casep);

Input: name, Symbol or NULL.
Input: call, Function object.
Input: casep, `restart-bind` is 1, `restart-case` is non-zero.
Output: x, Hold variable.
```

Creates a `restart` object.  
`name` is the name, or `NIL` if it is `NULL`.  
`call` specifies a function object.  
`casep` is an argument to distinguish between `restart-bind` and `restart-case`,
and should be non-zero if you want to specify `restart-case`.  
This function creates a `restart` object,
but does not register it into the stack frame.  
If `name` or `call` is a hold variable, its content will be used.


## Function `lisp_restart_interactive`

```c
void lisp_restart_interactive(addr restart, addr call);

Input: restart, Restart object.
Input: call, Function object.
```

Sets the `restart` object to the interactive code.  
`call` specifies a function to call with no arguments.  
If `call` is `NULL`, it is not specified.  
If `restart` and `call` are hold variables, the contents will be used.


## Function `lisp_restart_report`

```c
void lisp_restart_report(addr restart, addr call);

Input: restart, Restart object.
Input: call, Function object.
```

Set the `restart` object to the report code.  
`call` specifies the function to call with one argument.  
If `call` is `NULL`, it is not specified.  
If `restart` and `call` are hold variables, the contents will be used.


## Function `lisp_restart_test`

```c
void lisp_restart_test(addr restart, addr call);

Input: restart, Restart object.
Input: call, Function object.
```

Sets the `restart` object to the test code.  
`call` specifies the function to call with one argument.  
If `call` is `NULL`, it is not specified.  
If `restart` and `call` are hold variables, the contents will be used.


## Function `lisp_restart_push`

```c
void lisp_restart_push(addr restart);

Input: restart, Restart object.
```

Registers `restart` with the current stack frame.  
If `restart` is a hold variable, use its contents.


## Function `lisp_restart_reverse`

```c
void lisp_restart_reverse(void);
```

Put the restart list in reverse order.  
Although multiple `lisp_restart_push` can be registered,
they are added to the current stack frame with `push`.
However, since they are added to the current stack frame by `push`,
the order of evaluation is not in the order of registration
but in the reverse order.  
Therefore, this function can be used to reverse the order of the restart list.
