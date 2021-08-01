% C Function: Function

Npt documentation.

Reference: [ANSI Common Lisp npt](index.html)  


# Function Specification

The following function specifications are described in `lisp.h`.

- [1. function](#function-1)

```c
void lisp_get_function(addr x, addr symbol);
void lisp_get_setf(addr x, addr symbol);
int lisp_get_function_(addr x, addr value);
int lisp_get_function8_(addr x, const void *str);
int lisp_get_function16_(addr x, const void *str);
int lisp_get_function32_(addr x, const void *str);
int lisp_get_setf_(addr x, addr value);
int lisp_get_setf8_(addr x, const void *str);
int lisp_get_setf16_(addr x, const void *str);
int lisp_get_setf32_(addr x, const void *str);
```

- [2. compiled-function](#function-2)

```c
void lisp_compiled_dynamic(int index, lisp_calltype_dynamic call);
void lisp_compiled_rest(int index, lisp_calltype_rest call);
void lisp_compiled_empty(int index, lisp_calltype_empty call);
void lisp_compiled_var1(int index, lisp_calltype_var1 call);
void lisp_compiled_var2(int index, lisp_calltype_var2 call);
void lisp_compiled_var3(int index, lisp_calltype_var3 call);

int lisp_compiled_function_(addr x, int index, addr symbol);
int lisp_compiled_function8_(addr x, int index, const void *str);
int lisp_compiled_function16_(addr x, int index, const void *str);
int lisp_compiled_function32_(addr x, int index, const void *str);
int lisp_compiled_defun_(int index, addr symbol);
int lisp_compiled_defun8_(int index, const void *str);
int lisp_compiled_defun16_(int index, const void *str);
int lisp_compiled_defun32_(int index, const void *str);
int lisp_compiled_defun_setf_(int index, addr symbol);
int lisp_compiled_defun_setf8_(int index, const void *str);
int lisp_compiled_defun_setf16_(int index, const void *str);
int lisp_compiled_defun_setf32_(int index, const void *str);

void lisp_compiled_setvalue(addr pos, addr value);
void lisp_compiled_getvalue(addr *ret);
```


# <a id="function-1">1. function</a>

Function to get a function object from symbol.

```c
void lisp_get_function(addr x, addr symbol);
void lisp_get_setf(addr x, addr symbol);
int lisp_get_function_(addr x, addr value);
int lisp_get_function8_(addr x, const void *str);
int lisp_get_function16_(addr x, const void *str);
int lisp_get_function32_(addr x, const void *str);
int lisp_get_setf_(addr x, addr value);
int lisp_get_setf8_(addr x, const void *str);
int lisp_get_setf16_(addr x, const void *str);
int lisp_get_setf32_(addr x, const void *str);
```


## Function `lisp_get_function`

```c
void lisp_get_function(addr x, addr symbol);

Input: symbol, symbol object.
Output: x, hold variable.[
```

Get a function object from symbol.  
This is the same as `symbol-function` in Common Lisp.  
If `value` is a hold variable, the content is used.  
If the function is `unbound`, `NULL` will be returned.  
If the argument is not a symbol type, `LISP ABORT`.


## Function `lisp_get_setf`

```c
void lisp_get_setf(addr x, addr symbol);

Input: symbol, symbol object.
Output: x, hold variable.[
```

Get a `setf` function object from symbol.  
This is the same as `(fdefinition (list 'setf symbol))` in Common Lisp.  
If `value` is a hold variable, the content is used.  
If the function is `unbound`, `NULL` will be returned.  
If the argument is not a symbol type, `LISP ABORT`.


## Escape Function `lisp_get_function_`

```c
int lisp_get_function_(addr x, addr value);

Input: value, object.
Output: x, hold variable.
Return: Non-zero when escaping.
```

If `value` is a symbol, get the function.  
If `value` is a function type, the value is returned as is.  
If `value` is a hold variable, the content is used.  
If the function is `unbound`, `NULL` is returned.  


## Escape Function `lisp_get_function8_`

```c
int lisp_get_function8_(addr x, const void *str);
int lisp_get_function16_(addr x, const void *str);
int lisp_get_function32_(addr x, const void *str);

input: str, unicode string.
Output: x, hold variable.
Return: Non-zero when escaping.
```

Get a function with `str` as the symbol name.  
If the function is `unbound`, `NULL` is returned.  
See the `lisp_string8_` function for details on Unicode strings.


## Escape Function `lisp_get_function16_`

See `lisp_get_function8_`.


## Escape Function `lisp_get_function32_`

See `lisp_get_function8_`.


## Escape Function `lisp_get_setf_`

```c
int lisp_get_setf_(addr x, addr value);

Input: value, object.
Output: x, hold variable.
Return: Non-zero when escaping.
```

If `value` is a symbol, get the `setf` function.  
If `value` is a function type, the value is returned as is.  
If `value` is a hold variable, the content is used.  
If the function is `unbound`, `NULL` is returned.  


## Escape Function `lisp_get_setf8_`

```c
int lisp_get_setf8_(addr x, const void *str);
int lisp_get_setf16_(addr x, const void *str);
int lisp_get_setf32_(addr x, const void *str);

input: str, unicode string.
Output: x, hold variable.
Return: Non-zero when escaping.
```

Get a `setf` function with `str` as the symbol name.  
If the function is `unbound`, `NULL` is returned.  
See the `lisp_string8_` function for details on Unicode strings.


## Escape Function `lisp_get_setf16_`

See `lisp_get_setf8_`.


## Escape Function `lisp_get_setf32_`

See `lisp_get_setf8_`.


# <a id="function-2">2. compiled-function</a>

Function for creating a function.

```c
void lisp_compiled_dynamic(int index, lisp_calltype_dynamic call);
void lisp_compiled_rest(int index, lisp_calltype_rest call);
void lisp_compiled_empty(int index, lisp_calltype_empty call);
void lisp_compiled_var1(int index, lisp_calltype_var1 call);
void lisp_compiled_var2(int index, lisp_calltype_var2 call);
void lisp_compiled_var3(int index, lisp_calltype_var3 call);

int lisp_compiled_function_(addr x, int index, addr symbol);
int lisp_compiled_function8_(addr x, int index, const void *str);
int lisp_compiled_function16_(addr x, int index, const void *str);
int lisp_compiled_function32_(addr x, int index, const void *str);
int lisp_compiled_defun_(int index, addr symbol);
int lisp_compiled_defun8_(int index, const void *str);
int lisp_compiled_defun16_(int index, const void *str);
int lisp_compiled_defun32_(int index, const void *str);
int lisp_compiled_defun_setf_(int index, addr symbol);
int lisp_compiled_defun_setf8_(int index, const void *str);
int lisp_compiled_defun_setf16_(int index, const void *str);
int lisp_compiled_defun_setf32_(int index, const void *str);

void lisp_compiled_setvalue(addr pos, addr value);
void lisp_compiled_getvalue(addr *ret);
```


## Function `lisp_compiled_dynamic`

```c
void lisp_compiled_dynamic(int index, lisp_calltype_dynamic call);

Input: index, function number.
Input: call, function pointer, int (*call)(addr)
```

Registers a function pointer `call` to the `index`th.  
`index` is a value between 0 and 31,
and the upper limit of 31 can be set with the define value `LISP_POINTER_EXTEND`.


When called from Lisp, the arguments are passed as a list as the first argument,
like the `&rest` specification.  
Unlike `rest`, the list is allocated with `dynamic-extent`,
so it cannot be set as the return value of the function.

A function like the following declaration will be created.

```lisp
(defun name (&rest args)
  (declare (dynamic-extent args)
  ...)
```


## Function `lisp_compiled_rest`

```c
void lisp_compiled_rest(int index, lisp_calltype_rest call);

Input: index, function number.
Input: call, function pointer, int (*call)(addr)
```

Registers a function pointer `call` to the `index`th.  
`index` is a value between 0 and 31,
and the upper limit of 31 can be set with the define value `LISP_POINTER_EXTEND`.

When called from Lisp, the arguments are passed as a list as the first argument,
like the `&rest` specification.  
Unlike `dynamic`, the list is allocated in the heap area,
so it can be set to the value returned by the function.

A function like the following declaration will be created.

```lisp
(defun name (&rest args)
  ...)
```


## Function `lisp_compiled_empty`

```c
void lisp_compiled_empty(int index, lisp_calltype_empty call);

Input: index, function number.
Input: call, function pointer, int (*call)(void)
```

Registers a function pointer `call` to the `index`th.  
`index` is a value between 0 and 31,
and the upper limit of 31 can be set with the define value `LISP_POINTER_EXTEND`.

It is called from Lisp with no arguments.  
If there is an argument, it is an error.  

A function like the following declaration will be created.

```lisp
(defun name ()
  ...)
```


## Function `lisp_compiled_var1`

```c
void lisp_compiled_var1(int index, lisp_calltype_var1 call);

Input: index, function number.
Input: call, function pointer, int (*call)(addr)
```

Registers a function pointer `call` to the `index`th.  
`index` is a value between 0 and 31,
and the upper limit of 31 can be set with the define value `LISP_POINTER_EXTEND`.

It is called from Lisp with a single argument.  
It is an error if there is not a single argument.  

A function like the following declaration will be created.

```lisp
(defun name (x)
  ...)
```


## Function `lisp_compiled_var2`

```c
void lisp_compiled_var2(int index, lisp_calltype_var2 call);

Input: index, function number.
Input: call, function pointer, int (*call)(addr, addr)
```

Registers a function pointer `call` to the `index`th.  
`index` is a value between 0 and 31,
and the upper limit of 31 can be set with the define value `LISP_POINTER_EXTEND`.

It is called from Lisp with two arguments.  
If there are not two arguments, an error occurs.  

A function like the following declaration will be created.

```lisp
(defun name (x y)
  ...)
```


## Function `lisp_compiled_var3`

```c
void lisp_compiled_var3(int index, lisp_calltype_var3 call);

Input: index, function number.
Input: call, function pointer, int (*call)(addr, addr, addr)
```

Registers a function pointer `call` to the `index`th.  
`index` is a value between 0 and 31,
and the upper limit of 31 can be set with the define value `LISP_POINTER_EXTEND`.

It is called from Lisp with three arguments.  
If there are not three arguments, an error occurs.  

A function like the following declaration will be created.

```lisp
(defun name (x y z)
  ...)
```


## Escape Function `lisp_compiled_function_`

```c
int lisp_compiled_function_(addr x, int index, addr symbol);

Input: index, function number.
Input: symbol, symbol object or NULL.
Return: Non-zero when escaping.
```

Creates a function object.  
The name to be held inside the function is specified as `symbol`.  
If `symbol` is null, `NIL` is used.  
If `symbol` is a hold variable, the content is used.  
When the object is called, the function pointer registered
in the function number will be executed.


## Escape Function `lisp_compiled_function8_`

```c
int lisp_compiled_function8_(addr x, int index, const void *str);
int lisp_compiled_function16_(addr x, int index, const void *str);
int lisp_compiled_function32_(addr x, int index, const void *str);

Input: index, function number.
Input: str, Unicode string.
Return: Non-zero when escaping.
```

Creates a function object.  
The name to be held inside the function is specified as `str`.  
When the object is called, the function pointer registered
in the function number will be executed.
See the `lisp_string8_` function for details on Unicode strings.


## Escape Function `lisp_compiled_function16_`

See `lisp_compiled_function8_`.


## Escape Function `lisp_compiled_function32_`

See `lisp_compiled_function8_`.


## Escape Function `lisp_compiled_defun_`

```c
int lisp_compiled_defun_(int index, addr symbol);

Input: index, function number.
Input: symbol, symbol object.
Return: Non-zero when escaping.
```

Register a function.  
Create an object and register it with `symbol-function`.  
If `symbol` is a hold variable, the content is used.  
When the generated function is called,
the function pointer registered in the function number will be executed.


## Escape Function `lisp_compiled_defun8_`

```c
int lisp_compiled_defun8_(int index, const void *str);
int lisp_compiled_defun16_(int index, const void *str);
int lisp_compiled_defun32_(int index, const void *str);

Input: index, function number.
Input: str, Unicode string.
Return: Non-zero when escaping.
```

Register a function.  
Create an object and register it with `symbol-function`.  
When the generated function is called,
the function pointer registered in the function number will be executed.
See the `lisp_string8_` function for details on Unicode strings.


## Escape Function `lisp_compiled_defun16_`

See `lisp_compiled_defun8_`.


## Escape Function `lisp_compiled_defun32_`

See `lisp_compiled_defun8_`.


## Escape Function `lisp_compiled_defun_setf_`


```c
int lisp_compiled_defun_setf_(int index, addr symbol);

Input: index, function number.
Input: symbol, symbol object.
Return: Non-zero when escaping.
```

Register a `setf` function.  
If `symbol` is a hold variable, the content is used.  
When the generated function is called,
the function pointer registered in the function number will be executed.


## Escape Function `lisp_compiled_defun_setf8_`

```c
int lisp_compiled_defun_setf8_(int index, const void *str);
int lisp_compiled_defun_setf16_(int index, const void *str);
int lisp_compiled_defun_setf32_(int index, const void *str);

Input: index, function number.
Input: str, Unicode string.
Return: Non-zero when escaping.
```

Register a `setf` function.  
When the generated function is called,
the function pointer registered in the function number will be executed.
See the `lisp_string8_` function for details on Unicode strings.


## Escape Function `lisp_compiled_defun_setf16_`

See `lisp_compiled_defun_setf8_`.


## Escape Function `lisp_compiled_defun_setf32_`

See `lisp_compiled_defun_setf8_`.


## Function `lisp_compiled_getvalue`

```c
void lisp_compiled_getvalue(addr *ret);

Output: ret, object.
```

Get the value of the closure.  
The value will be retrieved from the stack frame,
so do not use `lisp_push_control` before retrieving it.


## Function `lisp_compiled_setvalue`

```c
void lisp_compiled_setvalue(addr pos, addr value);

Input: pos, function object.
Input: value, object or NULL.
```

Set the closure of a function object to a value.  
If `value` is null, `NIL` will be used.  
If `value` is a hold variable, the content will be used.
