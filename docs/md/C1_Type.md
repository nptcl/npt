% C Function: Type Function

Npt documentation.

Reference: [ANSI Common Lisp npt](index.html)  


# Function Specification

The following function specifications are described in `lisp.h`.

- [1. Hold Variables](#type-1)

```c
int lisp_hold_p(addr x);
void lisp_hold_value(addr x, addr *ret);
void lisp_hold_set(addr x, addr value);
addr Lisp_holdv(addr x);
void lisp_hold(addr *ret, addr value);
addr Lisp_hold(void);
```

- [2. Boolean](#type-2)

```c
void lisp_nil(addr x);
void lisp_t(addr x);
addr Lisp_nil(void);
addr Lisp_t(void);
```

- [3. System](#type-3)

```c
int lisp_nil_p(addr x);
int lisp_t_p(addr x);
int lisp_null_p(addr x);
int lisp_character_p(addr x);
int lisp_cons_p(addr x);
int lisp_list_p(addr x);
int lisp_string_p(addr x);
int lisp_symbol_p(addr x);
int lisp_strvect_p(addr x);
int lisp_array_p(addr x);
int lisp_vector_p(addr x);
```

- [4. Number](#type-4)

```c
int lisp_fixnum_p(addr x);
int lisp_bignum_p(addr x);
int lisp_integer_p(addr x);
int lisp_ratio_p(addr x);
int lisp_rational_p(addr x);
int lisp_single_float_p(addr x);
int lisp_double_float_p(addr x);
int lisp_long_float_p(addr x);
int lisp_float_p(addr x);
int lisp_real_p(addr x);
int lisp_complex_p(addr x);
int lisp_number_p(addr x);
```

- [5. Object](#type-5)

```c
int lisp_clos_p(addr x);
int lisp_hashtable_p(addr x);
int lisp_readtable_p(addr x);
int lisp_control_p(addr x);
int lisp_callname_p(addr x);
int lisp_function_p(addr x);
int lisp_package_p(addr x);
int lisp_random_state_p(addr x);
int lisp_pathname_p(addr x);
int lisp_stream_p(addr x);
int lisp_restart_p(addr x);
int lisp_environment_p(addr x);
int lisp_bitvector_p(addr x);
int lisp_print_dispatch_p(addr x);
```


# <a id="type-1">1. Hold Variables</a>

Function of the hold variable.

```c
int lisp_hold_p(addr x);
void lisp_hold_value(addr x, addr *ret);
void lisp_hold_set(addr x, addr value);
addr Lisp_holdv(addr x);
void lisp_hold(addr *ret, addr value);
addr Lisp_hold(void);
```


## Function `lisp_hold_p`

```c
int lisp_hold_p(addr x);

Input: x Object
Return: boolean
```

If `x` is a hold variable, non-zero is returned.  


## Function `lisp_hold_value`

```c
void lisp_hold_value(addr x, addr *ret);

Input: x Object
Output: ret Object
```

If `x` is a hold variable, the referenced object is returned to `ret`.  
If `x` is not a hold variable, `x` is returned to `ret` as is.


## Function `lisp_hold_set`

```c
void lisp_hold_set(addr x, addr value);

Input: x Hold variable
Input: value Object
```

Stores the object in the `x` hold variable.  
If `value` is a hold variable, the object held by `value` is stored in `x`.


## Function `Lisp_holdv`

```c
addr Lisp_holdv(addr x);

Input: x Object
Return: Object
```

Same as the function `lisp_hold_value`, but returns an object.  
If `x` is a hold variable, the stored object is returned.  
If `x` is not a hold variable, the input is returned as is.


## Function `lisp_hold`

```c
void lisp_hold(addr *ret, addr value);

input: value Object
Output: ret Hold variable
```

Create a hold variable and store the `value` and return it to `ret`.  
If `value` is a hold variable, the value held by the function is used.


## Function `Lisp_hold`

```c
addr Lisp_hold(void);

Return: hold variable
```

Creates and returns a hold variable.  
The value of the hold variable is `NIL`.


# <a id="type-2">2. Boolean</a>

Function of the boolean.

```c
void lisp_nil(addr x);
void lisp_t(addr x);
addr Lisp_nil(void);
addr Lisp_t(void);
```


## Function `lisp_nil`

```c
void lisp_nil(addr x);

Output: x Hold variable
```

Store the `nil` object in the `x` hold variable.


## Function `lisp_t`

```c
void lisp_t(addr x);

Output: x Hold variable
```

Store the `t` object in the `x` hold variable.


## Function `Lisp_nil`

```c
addr Lisp_nil(void);

Return: nil
```

Return a `nil` object.


## Function `Lisp_t`

```c
addr Lisp_t(void);

Return: t
```

Return a `t` object.


# <a id="type-3">3. System</a>

Function of the system object.

```c
int lisp_nil_p(addr x);
int lisp_t_p(addr x);
int lisp_null_p(addr x);
int lisp_character_p(addr x);
int lisp_cons_p(addr x);
int lisp_list_p(addr x);
int lisp_string_p(addr x);
int lisp_symbol_p(addr x);
int lisp_strvect_p(addr x);
int lisp_array_p(addr x);
int lisp_vector_p(addr x);
```


## Function `lisp_nil_p`

```c
int lisp_nil_p(addr x);

Input: x Object
Return: boolean
```

If `x` is a hold variable, the value held by the variable is evaluated.  
If `x` is `nil`, a non-zero value is returned.


## Function `lisp_t_p`

```c
int lisp_t_p(addr x);

Input: x Object
Return: boolean
```

If `x` is a hold variable, the value held by the variable is evaluated.  
If `x` is `t`, a non-zero value is returned.


## Function `lisp_null_p`

```c
int lisp_null_p(addr x);

Input: x Object
Return: boolean
```

If `x` is a hold variable, the value held by the variable is evaluated.  
If `x` is a NULL pointer, a non-zero pointer is returned.  
The NULL pointer is the same as `(void *)0` in C.
It is not the same as `NIL` in Common Lisp.


## Function `lisp_character_p`

```c
int lisp_character_p(addr x);

Input: x Object
Return: boolean
```

If `x` is a hold variable, the value held by the variable is evaluated.  
If `x` is a character type, a non-zero value is returned.


## Function `lisp_cons_p`

```c
int lisp_cons_p(addr x);

Input: x Object
Return: boolean
```

If `x` is a hold variable, the value held by the variable is evaluated.  
If `x` is a cons, a non-zero value is returned.


## Function `lisp_list_p`

```c
int lisp_list_p(addr x);

Input: x Object
Return: boolean
```

If `x` is a hold variable, the value held by the variable is evaluated.  
If `x` is a list, a non-zero value is returned.
A list is a `NIL` or cons.


## Function `lisp_string_p`

```c
int lisp_string_p(addr x);

Input: x Object
Return: boolean
```

If `x` is a hold variable, the value held by the variable is evaluated.  
If `x` is a string, a non-zero value is returned.  
A string is an object of type `LISPTYPE_STRING` or
a specialized array of one dimensional characters of type `LISPTYPE_ARRAY`.


## Function `lisp_strvect_p`

```c
int lisp_strvect_p(addr x);

Input: x Object
Return: boolean
```

If `x` is a hold variable, the value held by the variable is evaluated.  
If `x` is `LISPTYPE_STRING` type, a non-zero value is returned.  
This is different from `stringp` in Common Lisp.


## Function `lisp_symbol_p`

```c
int lisp_symbol_p(addr x);

Input: x Object
Return: boolean
```

If `x` is a hold variable, the value held by the variable is evaluated.  
If `x` is a symbol, a non-zero value is returned.  


## Function `lisp_array_p`

```c
int lisp_array_p(addr x);

Input: x Object
Return: boolean
```

If `x` is a hold variable, the value held by the variable is evaluated.  
If `x` is `LISPTYPE_ARRAY` type, a non-zero value is returned.  
This is different from `arrayp` in Common Lisp.


## Function `lisp_vector_p`

```c
int lisp_vector_p(addr x);

Input: x Object
Return: boolean
```

If `x` is a hold variable, the value held by the variable is evaluated.  
If `x` is `LISPTYPE_VECTOR` type, a non-zero value is returned.  
This is different from `vectorp` in Common Lisp.


# <a id="type-4">4. Number</a>

Function of the number object.

```c
int lisp_fixnum_p(addr x);
int lisp_bignum_p(addr x);
int lisp_integer_p(addr x);
int lisp_ratio_p(addr x);
int lisp_rational_p(addr x);
int lisp_single_float_p(addr x);
int lisp_double_float_p(addr x);
int lisp_long_float_p(addr x);
int lisp_float_p(addr x);
int lisp_real_p(addr x);
int lisp_complex_p(addr x);
int lisp_number_p(addr x);
```


## Function `lisp_fixnum_p`

```c
int lisp_fixnum_p(addr x);

Input: x Object
Return: boolean
```

If `x` is a hold variable, the value held by the variable is evaluated.  
If `x` is `LISPTYPE_FIXNUM` type, a non-zero value is returned.  
This is not exactly the same as `(typep x 'fixnum)` in Common Lisp,
but it is not different in normal use.


## Function `lisp_bignum_p`

```c
int lisp_bignum_p(addr x);

Input: x Object
Return: boolean
```

If `x` is a hold variable, the value held by the variable is evaluated.  
If `x` is `LISPTYPE_BIGNUM` type, a non-zero value is returned.  


## Function `lisp_integer_p`

```c
int lisp_integer_p(addr x);

Input: x Object
Return: boolean
```

If `x` is a hold variable, the value held by the variable is evaluated.  
If `x` is `integer` type, a non-zero value is returned.  


## Function `lisp_ratio_p`

```c
int lisp_ratio_p(addr x);

Input: x Object
Return: boolean
```

If `x` is a hold variable, the value held by the variable is evaluated.  
If `x` is `ratio` type, a non-zero value is returned.  


## Function `lisp_rational_p`

```c
int lisp_rational_p(addr x);

Input: x Object
Return: boolean
```

If `x` is a hold variable, the value held by the variable is evaluated.  
If `x` is `rational` type, a non-zero value is returned.  


## Function `lisp_single_float_p`

```c
int lisp_single_float_p(addr x);

Input: x Object
Return: boolean
```

If `x` is a hold variable, the value held by the variable is evaluated.  
If `x` is `single-float` type, a non-zero value is returned.  


## Function `lisp_double_float_p`

```c
int lisp_double_float_p(addr x);

Input: x Object
Return: boolean
```

If `x` is a hold variable, the value held by the variable is evaluated.  
If `x` is `double-float` type, a non-zero value is returned.  


## Function `lisp_long_float_p`

```c
int lisp_long_float_p(addr x);

Input: x Object
Return: boolean
```

If `x` is a hold variable, the value held by the variable is evaluated.  
If `x` is `long-float` type, a non-zero value is returned.  


## Function `lisp_float_p`

```c
int lisp_float_p(addr x);

Input: x Object
Return: boolean
```

If `x` is a hold variable, the value held by the variable is evaluated.  
If `x` is `float` type, a non-zero value is returned.  


## Function `lisp_real_p`

```c
int lisp_real_p(addr x);

Input: x Object
Return: boolean
```

If `x` is a hold variable, the value held by the variable is evaluated.  
If `x` is `real` type, a non-zero value is returned.  


## Function `lisp_complex_p`

```c
int lisp_complex_p(addr x);

Input: x Object
Return: boolean
```

If `x` is a hold variable, the value held by the variable is evaluated.  
If `x` is `complex` type, a non-zero value is returned.  


## Function `lisp_number_p`

```c
int lisp_number_p(addr x);

Input: x Object
Return: boolean
```

If `x` is a hold variable, the value held by the variable is evaluated.  
If `x` is `number` type, a non-zero value is returned.  
