% C Function: Object

Npt documentation.

Reference: [ANSI Common Lisp npt](index.html)  


# Function Specification

The following function specifications are described in `lisp.h`.

- [1. Create Object](#object-1)

```c
int lisp_character_(addr x, unicode value);
void lisp_fixnum(addr x, fixnum value);
int lisp_float_(addr x, float value);
int lisp_double_(addr x, double value);
int lisp_long_double_(addr x, long double value);
```

- [2. Get Value](#object-2)

```c
int lisp_zero_p(addr value);
int lisp_plus_p(addr value);
int lisp_minus_p(addr value);
void lisp_get_character(addr pos, unicode *ret);
void lisp_get_fixnum(addr pos, fixnum *ret);
int lisp_get_float_(addr pos, float *ret);
int lisp_get_double_(addr pos, double *ret);
int lisp_get_long_double_(addr pos, long double *ret);
```

- [3. Package](#object-3)

```c
int lisp_package_(addr x, addr pos);
int lisp_package8_(addr x, const void *str);
int lisp_package16_(addr x, const void *str);
int lisp_package32_(addr x, const void *str);
```

- [4. Intern](#object-4)

```c
int lisp_intern_(addr x, addr package, addr name);
int lisp_intern8_(addr x, const void *package, const void *name);
int lisp_intern16_(addr x, const void *package, const void *name);
int lisp_intern32_(addr x, const void *package, const void *name);
```

- [5. Reader](#object-5)

```c
int lisp_reader_(addr x, addr str);
int lisp_reader8_(addr x, const void *str);
int lisp_reader16_(addr x, const void *str);
int lisp_reader32_(addr x, const void *str);
```

- [6. Pathname](#object-6)

```c
int lisp_pathname_(addr x, addr name);
int lisp_pathname8_(addr x, const void *str);
int lisp_pathname16_(addr x, const void *str);
int lisp_pathname32_(addr x, const void *str);
int lisp_namestring_(addr x, addr path);
```


# <a id="object-1">1. Create Object</a>

Function of the creating object.

```c
int lisp_character_(addr x, unicode value);
void lisp_fixnum(addr x, fixnum value);
int lisp_float_(addr x, float value);
int lisp_double_(addr x, double value);
int lisp_long_double_(addr x, long double value);
```

## Escape Function `lisp_character_`

```c
int lisp_character_(addr x, unicode value);

Input: value, unicode
Output: x, hold variable
Return: Non-zero when escaping.
```

`value` is a Unicode value and generates an object of character type.  
If `value` is a code reserved for a surrogate pair,
or is beyond the range of Unicode, an error occurs.


## Function `lisp_fixnum`

```c
void lisp_fixnum(addr x, fixnum value);

Input: value, integer
Output: x, hold variable
```

Create an integer object of type `fixnum`.


## Escape Function `lisp_float_`

```c
int lisp_float_(addr x, float value);

Input: value, floating-point number
Output: x, hold variable
Return: Non-zero when escaping.
```

Creates a `single-float` type floating-point object.  
If `value` is `inf` or `nan`, it is an error.


## Escape Function `lisp_double_`

```c
int lisp_double_(addr x, double value);

Input: value, floating-point number
Output: x, hold variable
Return: Non-zero when escaping.
```

Creates a `double-float` type floating-point object.  
If `value` is `inf` or `nan`, it is an error.


## Escape Function `lisp_long_double_`

```c
int lisp_long_double_(addr x, long double value);

Input: value, floating-point number
Output: x, hold variable
Return: Non-zero when escaping.
```

Creates a `long-float` type floating-point object.  
If `value` is `inf` or `nan`, it is an error.


# <a id="object-2">2. Get Value</a>

Function of the getting value.

```c
int lisp_zero_p(addr value);
int lisp_plus_p(addr value);
int lisp_minus_p(addr value);
void lisp_get_character(addr pos, unicode *ret);
void lisp_get_fixnum(addr pos, fixnum *ret);
int lisp_get_float_(addr pos, float *ret);
int lisp_get_double_(addr pos, double *ret);
int lisp_get_long_double_(addr pos, long double *ret);
```


## Function `lisp_zero_p`

```c
int lisp_zero_p(addr value);

Input: value, Object
Return: boolean
```

If `value` is a hold variable, its contents are used.  
If `value` is a real number and equal to `0`, non-zero is returned.


## Function `lisp_plus_p`

```c
int lisp_plus_p(addr value);

Input: value, Object
Return: boolean
```

If `value` is a hold variable, its contents are used.  
If `value` is real and greater than `0`, non-zero is returned.  
The `0` is not included.


## Function `lisp_minus_p`

```c
int lisp_minus_p(addr value);

Input: value, Object
Return: boolean
```

If `value` is a hold variable, its contents are used.  
If `value` is real and less than `0`, non-zero is returned.  
The `0` is not included.


## Function `lisp_get_character`

```c
void lisp_get_character(addr pos, unicode *ret);

Input: pos, character object
Output: ret, unicode value
```

If `pos` is a hold variable, its contents are used.  
Return the character type code to `ret`.


## Function `lisp_get_fixnum`

```c
void lisp_get_fixnum(addr pos, fixnum *ret);

Input: pos, fixnum object
Output: ret, fixnum value
```

If `pos` is a hold variable, its contents are used.  
Return the value of an object of type `LISPTYPE_FIXNUM` to `ret`.


## Escape Function `lisp_get_float_`

```c
int lisp_get_float_(addr pos, float *ret);

Input: pos, real object
Output: ret, float value
Return: Non-zero when escaping.
```

If `pos` is a hold variable, its contents are used.  
If `pos` is of `single-float` type, the value is returned as `ret`.  
In the case of other real types, the value is cast to `single-float`.


## Escape Function `lisp_get_double_`

```c
int lisp_get_double_(addr pos, double *ret);

Input: pos, real object
Output: ret, double value
Return: Non-zero when escaping.
```

If `pos` is a hold variable, its contents are used.  
If `pos` is of `double-float` type, the value is returned as `ret`.  
In the case of other real types, the value is cast to `double-float`.


## Escape Function `lisp_get_long_double_`

```c
int lisp_get_long_double_(addr pos, long double *ret);

Input: pos, real object
Output: ret, long double value
Return: Non-zero when escaping.
```

If `pos` is a hold variable, its contents are used.  
If `pos` is of `long-float` type, the value is returned as `ret`.  
In the case of other real types, the value is cast to `long-float`.


# <a id="object-3">3. Package</a>

Function of the package.

```c
int lisp_package_(addr x, addr pos);
int lisp_package8_(addr x, const void *str);
int lisp_package16_(addr x, const void *str);
int lisp_package32_(addr x, const void *str);
```

## Escape Function `lisp_package_`

```c
int lisp_package_(addr x, addr pos);

Input: pos, package-designer
Output: x, hold variable
Return: Non-zero when escaping.
```

If `pos` is a hold variable, its contents are used.  
Returns the package represented by `pos`.


## Escape Function `lisp_package8_`

```c
int lisp_package8_(addr x, const void *str);
int lisp_package16_(addr x, const void *str);
int lisp_package32_(addr x, const void *str);

Input: str, Unicode strings
Output: x, hold variable
Return: Non-zero when escaping.
```

Return the package represented by `str`.  
See the `lisp_string8_` function for details on Unicode strings.


## Escape Function `lisp_package16_`

See `lisp_package8_`.


## Escape Function `lisp_package32_`

See `lisp_package8_`.


# <a id="object-4">4. Intern</a>

Function of the intern.

```c
int lisp_intern_(addr x, addr package, addr name);
int lisp_intern8_(addr x, const void *package, const void *name);
int lisp_intern16_(addr x, const void *package, const void *name);
int lisp_intern32_(addr x, const void *package, const void *name);
```


## Escape Function `lisp_intern_`

```c
int lisp_intern_(addr x, addr package, addr name);

Input: package, package-designer or nil
Input: name, string object
Output: x, hold variable
Return: Non-zero when escaping.
```

Intern the `package` with the symbol represented by the `name`. 
If `package` is `nil` or `NULL`, the value of `*package*` is used.  
If `package`, `name` is a hold variable, its contents are used.  


## Escape Function `lisp_intern8_`

```c
int lisp_intern8_(addr x, const void *package, const void *name);
int lisp_intern16_(addr x, const void *package, const void *name);
int lisp_intern32_(addr x, const void *package, const void *name);

Input: package, Unicode strings or NULL
Input: name, Unicode strings
Output: x, hold variable
Return: Non-zero when escaping.
```

Intern the `package` with the symbol represented by the `name`. 
If `package` is `nil` or `NULL`, the value of `*package*` is used.  
See the `lisp_string8_` function for details on Unicode strings.


## Escape Function `lisp_intern16_`

See `lisp_intern8_`.


## Escape Function `lisp_intern32_`

See `lisp_intern8_`.


# <a id="object-5">5. Reader</a>

Function of the reader.

```c
int lisp_reader_(addr x, addr str);
int lisp_reader8_(addr x, const void *str);
int lisp_reader16_(addr x, const void *str);
int lisp_reader32_(addr x, const void *str);
```


## Escape Function `lisp_reader_`

```c
int lisp_reader_(addr x, addr str);

Input: str, string object
Output: x, hold variable
Return: Non-zero when escaping.
```

Reads the string `str` in the reader.  
This is equivalent to the `read-from-string` function.  
If nothing can be read, `NULL` is returned.  
The function `lisp_null_p` can be used to check whether the value is null or not.  
If `str` is a hold variable, the contents are used.


## Escape Function `lisp_reader8_`

```c
int lisp_reader8_(addr x, const void *str);
int lisp_reader16_(addr x, const void *str);
int lisp_reader32_(addr x, const void *str);

Input: str, unicode strings
Output: x, hold variable
Return: Non-zero when escaping.
```

Reads the string `str` in the reader.  
This is equivalent to the `read-from-string` function.  
If nothing can be read, `NULL` is returned.  
The function `lisp_null_p` can be used to check whether the value is null or not.  
See the `lisp_string8_` function for details on Unicode strings.


## Escape Function `lisp_reader16_`

See `lisp_reader8_`.


## Escape Function `lisp_reader32_`

See `lisp_reader8_`.


# <a id="object-6">6. Pathname</a>

Function of the pathname.

```c
int lisp_pathname_(addr x, addr name);
int lisp_pathname8_(addr x, const void *str);
int lisp_pathname16_(addr x, const void *str);
int lisp_pathname32_(addr x, const void *str);
int lisp_namestring_(addr x, addr path);
```


## Escape Function `lisp_pathname_`

```c
int lisp_pathname_(addr x, addr name);

Input: name, pathname-designer
Output: x, hold variable
Return: Non-zero when escaping.
```

Converts the string `name` to a pathname.  
This is equivalent to the `parse-namestring` function.  
If `name` is a hold variable, the contents are used.


## Escape Function `lisp_pathname8_`

```c
int lisp_pathname8_(addr x, const void *str);
int lisp_pathname16_(addr x, const void *str);
int lisp_pathname32_(addr x, const void *str);

Input: str, unicode strings
Output: x, hold variable
Return: Non-zero when escaping.
```

Converts the string `name` to a pathname.  
This is equivalent to the `parse-namestring` function.  
See the `lisp_string8_` function for details on Unicode strings.


## Escape Function `lisp_pathname16_`

See `lisp_pathname8_`.


## Escape Function `lisp_pathname32_`

See `lisp_pathname8_`.


## Escape Function `lisp_namestring_`

```c
int lisp_namestring_(addr x, addr path);

Input: path, pathname-designer
Output: x, hold variable
Return: Non-zero when escaping.
```

Convert the pathname object `path` to a string.  
This is equivalent to the `namestring` function.  
If `name` is a hold variable, the contents are used.
