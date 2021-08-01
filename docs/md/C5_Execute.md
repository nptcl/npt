% C Function: Execute

Npt documentation.

Reference: [ANSI Common Lisp npt](index.html)  


# Function Specification

The following function specifications are described in `lisp.h`.

- [1. eval](#execute-1)

```c
int lisp_eval_(addr x, addr pos);
int lisp_eval8_(addr x, const void *str);
int lisp_eval16_(addr x, const void *str);
int lisp_eval32_(addr x, const void *str);
```

- [2. funcall](#execute-2)

```c
int lisp_funcall_(addr x, addr call, ...);
int lisp_funcall8_(addr x, const void *str, ...);
int lisp_funcall16_(addr x, const void *str, ...);
int lisp_funcall32_(addr x, const void *str, ...);
```

- [3. apply](#execute-3)

```c
int lisp_apply_(addr x, addr call, ...);
int lisp_apply8_(addr x, const void *str, ...);
int lisp_apply16_(addr x, const void *str, ...);
int lisp_apply32_(addr x, const void *str, ...);
```

- [4. Low-level execution](#execute-4)

```c
int lisp_eval_control_(addr eval);
int lisp_eval_string_control_(addr eval);
int lisp_funcall_control_(addr call, ...);
int lisp_apply_control_(addr call, ...);
```

- [5. values](#execute-5)

```c
void lisp_result_control(addr x);
void lisp_result2_control(addr x, addr y);
void lisp_values_control(addr x);
void lisp_nth_value_control(addr x, size_t index);
void lisp_set_result_control(addr value);
void lisp_set_values_control(addr first, ...);
void lisp_set_values_nil_control(void);
void lisp_set_values_list_control(addr list);
```

- [6. Escape operation](#execute-6)

```c
int lisp_break_control(void);
int lisp_escape_control(void);
void lisp_reset_control(void);
enum lisp_escape lisp_escape_type_control(void);
void lisp_save_control(addr *ret);
void lisp_rollback_control(addr value);
```

- [7. Others](#execute-7)

```c
int lisp_eval_loop_(void);
```


# <a id="execute-1">1. eval</a>

Functiuons of the `eval`.

```c
int lisp_eval_(addr x, addr pos);
int lisp_eval8_(addr x, const void *str);
int lisp_eval16_(addr x, const void *str);
int lisp_eval32_(addr x, const void *str);
```


## Escape Function `lisp_eval_`

```c
int lisp_eval_(addr x, addr pos);

Input: pos, object.
Output: x, hold variable or NULL.
Return: Non-zero when escaping.
```

Execute `eval` with `pos` as a Lisp expression.  
If `pos` is a hold variable, the content is used.  
`x` is assigned to the first returned value of the `eval` execution result.  
If `x` is `NULL`, the returned value will be ignored.


## Escape Function `lisp_eval8_`

```c
int lisp_eval8_(addr x, const void *str);
int lisp_eval16_(addr x, const void *str);
int lisp_eval32_(addr x, const void *str);

Input: str, unicode string.
Output: x, hold variable or NULL.
Return: Non-zero when escaping.
```

Convert `str` to an object with the reader, and then run `eval`. 
`x` is assigned to the first returned value of the `eval` execution result.  
If `x` is `NULL`, the returned value will be ignored.
See the `lisp_string8_` function for details on Unicode strings.


## Escape Function `lisp_eval16_`

See `lisp_eval8_`.


## Escape Function `lisp_eval32_`

See `lisp_eval8_`.


# <a id="execute-2">2. funcall</a>

Functions of the `funcall`.

```c
int lisp_funcall_(addr x, addr call, ...);
int lisp_funcall8_(addr x, const void *str, ...);
int lisp_funcall16_(addr x, const void *str, ...);
int lisp_funcall32_(addr x, const void *str, ...);
```


## Escape Function `lisp_funcall_`

```c
int lisp_funcall_(addr x, addr call, ...);

Input: call, symbol or function.
Input: ..., object.
Output: x, hold variable.
Return: Non-zero when escaping.
```

This function executes the function specified by `call`.  
It is equivalent to `funcall` in Common Lisp.  
Variable arguments `...` are terminated with `NULL`.  
`x` is assigned to the first returned value of the function execution.  
If `x` is `NULL`, the returned value will be ignored.
If `call` and variable arguments are hold variables, their contents are used.


## Escape Function `lisp_funcall8_`

```c
int lisp_funcall8_(addr x, const void *str, ...);
int lisp_funcall16_(addr x, const void *str, ...);
int lisp_funcall32_(addr x, const void *str, ...);

Input: str, unicode string.
Input: ..., object.
Output: x, hold variable.
Return: Non-zero when escaping.
```

Execute the function named `str`. 
It is equivalent to `funcall` in Common Lisp.  
Variable arguments `...` are terminated with `NULL`.  
`x` is assigned to the first returned value of the function execution.  
If `x` is `NULL`, the returned value will be ignored.
If variable arguments are hold variables, their contents are used.
See the `lisp_string8_` function for details on Unicode strings.

An example of how to do this is shown below.

```c
lisp_fixnum(x, 10);
lisp_fixnum(y, 20);
lisp_fixnum(z, 30);
lisp_funcall8_(x, "+", x, y, z, NULL);

x -> 60
```


## Escape Function `lisp_funcall16_`

See `lisp_funcall8_`.


## Escape Function `lisp_funcall32_`

See `lisp_funcall8_`.


# <a id="execute-3">3. apply</a>

Functions of the `apply`.

```c
int lisp_apply_(addr x, addr call, ...);
int lisp_apply8_(addr x, const void *str, ...);
int lisp_apply16_(addr x, const void *str, ...);
int lisp_apply32_(addr x, const void *str, ...);
```


## Escape Function `lisp_apply_`

```c
int lisp_apply_(addr x, addr call, ...);

Input: call, symbol or function.
Input: ..., object.
Output: x, hold variable.
Return: Non-zero when escaping.
```

This function executes the function specified by `call`.  
It is equivalent to `apply` in Common Lisp.  
Variable arguments are terminated with `NULL` and
the last element becomes the `cdr` part.  
`x` is assigned to the first returned value of the function execution.  
If `x` is `NULL`, the returned value will be ignored.
If `call` and variable arguments are hold variables, their contents are used.


## Escape Function `lisp_apply8_`

```c
int lisp_apply8_(addr x, const void *str, ...);
int lisp_apply16_(addr x, const void *str, ...);
int lisp_apply32_(addr x, const void *str, ...);

Input: str, unicode string.
Input: ..., object.
Output: x, hold variable.
Return: Non-zero when escaping.
```

Execute the function named `str`. 
It is equivalent to `apply` in Common Lisp.  
Variable arguments are terminated with `NULL` and
the last element becomes the `cdr` part.  
`x` is assigned to the first returned value of the function execution.  
If `x` is `NULL`, the returned value will be ignored.
If variable arguments are hold variables, their contents are used.
See the `lisp_string8_` function for details on Unicode strings.

An example of how to do this is shown below.

```c
lisp_fixnum(x, 10);
lisp_fixnum(y, 20);
lisp_fixnum(z, 30);
lisp_list_(x, x, y, z, NULL);
lisp_apply8_(x, "+", x, NULL);

x -> 60
```


## Escape Function `lisp_apply16_`

See `lisp_apply8_`.


## Escape Function `lisp_apply32_`

See `lisp_apply8_`.


# <a id="execute-4">4. Low-level execution</a>

Functions for low-level operations of execution.

```c
int lisp_eval_control_(addr eval);
int lisp_eval_string_control_(addr eval);
int lisp_funcall_control_(addr call, ...);
int lisp_apply_control_(addr call, ...);
```


## Escape Function `lisp_eval_control_`

```c
int lisp_eval_control_(addr eval);

Input: eval, object.
Return: Non-zero when escaping.
```

Execute `eval` with the argument `eval`.  
To get the return value, use the return value access function.


## Escape Function `lisp_eval_string_control_`

```c
int lisp_eval_string_control_(addr eval);

Input: eval, string.
Return: Non-zero when escaping.
```

Convert `eval` to an object with the reader, and then run `eval`. 
To get the return value, use the return value access function.


## Escape Function `lisp_funcall_control_`

```c
int lisp_funcall_control_(addr call, ...);

Input: call, symbol or function.
Input: ..., object.
Return: Non-zero when escaping.
```

This function executes the function specified by `call`.  
It is equivalent to `funcall` in Common Lisp.  
Variable arguments `...` are terminated with `NULL`.  
If `call` and variable arguments are hold variables, their contents are used.
To get the return value, use the return value access function.


## Escape Function `lisp_apply_control_`

```c
int lisp_apply_control_(addr call, ...);

Input: call, symbol or function.
Input: ..., object.
Return: Non-zero when escaping.
```

This function executes the function specified by `call`.  
It is equivalent to `apply` in Common Lisp.  
Variable arguments are terminated with `NULL` and
the last element becomes the `cdr` part.  
If `call` and variable arguments are hold variables, their contents are used.
To get the return value, use the return value access function.


# <a id="execute-5">5. values</a>

Functions for manipulating the return value.

```c
void lisp_result_control(addr x);
void lisp_result2_control(addr x, addr y);
void lisp_values_control(addr x);
void lisp_nth_value_control(addr x, size_t index);
void lisp_set_result_control(addr value);
void lisp_set_values_control(addr first, ...);
void lisp_set_values_nil_control(void);
void lisp_set_values_list_control(addr list);
```


## Function `lisp_result_control`

```c
void lisp_result_control(addr x);

Output: x, hold variable.
```

Get the first returned value.  
If the number of returned values is zero, it is `NIL`.


## Function `lisp_result2_control`

```c
void lisp_result2_control(addr x, addr y);

Output: x, y, hold variables.
```

Get the first return value as `x` and the second return value as `y`.  
If the corresponding return value does not exist, `NIL` will be returned.


## Function `lisp_values_control`

```c
void lisp_values_control(addr x);

Output: x, hold variable.
```

Get the return value as a list.  
This is the same as `multiple-value-list` in Common Lisp.


## Function `lisp_nth_value_control`

```c
void lisp_nth_value_control(addr x, size_t index);

Input: index.
Output: x, hold variable.
```

Get the `index`th returned value.  
It is almost the same as `nth-value` in Common Lisp.  
If the corresponding value does not exist, `NIL` will be returned.


# <a id="execute-6">6. Escape operation</a>

Functions of the escape.

```c
int lisp_break_control(void);
int lisp_escape_control(void);
void lisp_reset_control(void);
enum lisp_escape lisp_escape_type_control(void);
void lisp_save_control(addr *ret);
void lisp_rollback_control(addr value);
```


## Function `lisp_break_control`

```c
int lisp_break_control(void);

Return: bool.
```

This function determines when to switch from escape to non-escape mode.  
Returns true if the program is currently in escape mode and
the current stack frame is the escape destination stack frame.  
It can also be executed in non-escape mode, but it always returns 0.  


## Function `lisp_escape_control`

```c
int lisp_escape_control(void);

Return: Non-zero when escaping.
```

Returns whether or not escape is currently in progress.  
Returns 0 if normal, non-zero if in escape.  


## Function `lisp_reset_control`

```c
void lisp_reset_control(void);
```

Aborts the escape and switches to normal time (non-escape).  
The reason for the escape is discarded.


## Function `lisp_escape_type_control`

```c
enum lisp_escape lisp_escape_type_control(void);

Return: Escape Mode
```

Returns the mode of escape.  
The return value is as follows.

| Return | Mode |
| --- | --- |
| lisp_escape_normal | Non-escape |
| lisp_escape_tagbody | Escaping by `tagbody`/`go` |
| lisp_escape_block | Escaping by `block`/`return-from` |
| lisp_escape_catch | Escaping by `catch`/`throw` |
| lisp_escape_handler_case | Escaping by `handler-case` |
| lisp_escape_restart_case | Escaping by `restart-case` |


## Function `lisp_save_control`

```c
void lisp_save_control(addr *ret);

Output: ret, saving object.
```

Evacuate the escape information and return value.  
Create a saving object for evacuation and place it on the current stack frame.  

It is used to execute the `cleanup` form of `unwind-protect`.


## Function `lisp_rollback_control`

```c
void lisp_rollback_control(addr value);

Input: value, saving object.
```

Update escape information and return value using
the save object created by `lisp_save_control`.  
It is an error if `value` is a hold variable.


# <a id="execute-7">7. Others</a>

Other operation functions.

```c
int lisp_eval_loop_(void);
```


## Escape Function `lisp_eval_loop_`

```c
int lisp_eval_loop_(void);

Return: Non-zero when escaping.
```

Run `eval-loop`.
