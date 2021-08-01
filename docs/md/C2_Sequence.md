% C Function: Sequense

Npt documentation.

Reference: [ANSI Common Lisp npt](index.html)  


# Function Specification

The following function specifications are described in `lisp.h`.

- [1. Create Sequence](#sequence-1)

```c
void lisp_cons(addr x, addr car, addr cdr);
void lisp_vector(addr x, size_t size);
void lisp_list(addr x, ...);
void lisp_lista(addr x, ...);
```

- [2. Sequence Operation](#sequence-2)

```c
int lisp_getelt_(addr x, addr pos, size_t index);
int lisp_setelt_(addr pos, size_t index, addr value);
int lisp_length_(addr pos, size_t *ret);

int lisp_reverse_(addr x, addr pos);
int lisp_nreverse_(addr x, addr pos);
```

- [3. Cons](#sequence-3)

```c
void lisp_car(addr x, addr list);
void lisp_cdr(addr x, addr list);
void lisp_carcdr(addr x, addr y, addr list);

void lisp_setf_car(addr cons, addr value);
void lisp_setf_cdr(addr cons, addr value);
void lisp_setf_carcdr(addr cons, addr car, addr cdr);
```

- [4. String](#sequence-4)

```c
int lisp_string8_(addr x, const void *str);
int lisp_string16_(addr x, const void *str);
int lisp_string32_(addr x, const void *str);
int lisp_string_getc_(addr pos, size_t i, unicode *c);
```

- [5. Strvect](#sequence-5)

```c
int lisp_strvect_getc(addr pos, size_t i, unicode *c);
int lisp_strvect_length(addr pos, size_t *ret);
```


# <a id="sequence-1">1. Create Sequence</a>

Function of the creating sequense.

```c
void lisp_cons(addr x, addr car, addr cdr);
void lisp_vector(addr x, size_t size);
void lisp_list(addr x, ...);
void lisp_lista(addr x, ...);
```


## Function `lisp_cons`

```c
void lisp_cons(addr x, addr car, addr cdr);

Input: car, cdr, Object
Output: x, hold variable
```

Create a cons object.  
If `car` is `NULL`, `NIL` is specified.  
If `cdr` is `NULL`, `NIL` is specified.  
If the `car` is a hold variable, the content is specified.  
If the `cdr` is a hold variable, the content is specified.


## Function `lisp_vector`

```c
void lisp_vector(addr x, size_t size);

Input: size, Length of the sequence.
Output: x, hold variable
```

Create a one-dimensional array object.  
It is a special object of type `simple-vector`.


## Function `lisp_list`

```c
void lisp_list(addr x, ...);

Input: Variable arguments
Output: x, hold variable
```

Creates a list object with variable arguments as elements.  
The variable argument takes an `addr` and is terminated by `NULL`.  
If the argument is a hold variable, use the content.

Example.

```c
lisp_fixnum(x, 10);
lisp_fixnum(y, 20);
lisp_fixnum(z, 30);
lisp_list(x, x, y, z, NULL);

x -> (10 20 30)
```


## Function `lisp_lista`

```c
void lisp_lista(addr x, ...);

Input: Variable arguments
Output: x, hold variable
```

Creates a list object with variable arguments as elements.  
This is almost equivalent to `list*` in Common Lisp,
where the final element is the list cdr.  
The variable argument takes an `addr` and is terminated with NULL.  
Unlike `list*`, it returns `NIL` if no element is found.  
If the argument is a hold variable, use the content.

Example.

```c
lisp_fixnum(x, 10);
lisp_fixnum(y, 20);
lisp_fixnum(z, 30);
lisp_lista(x, x, y, z, NULL);

x -> (10 20 . 30)
```


# <a id="sequence-2">2. Sequence Operation</a>

Function of the sequense operation.

```c
int lisp_getelt_(addr x, addr pos, size_t index);
int lisp_setelt_(addr pos, size_t index, addr value);
int lisp_length_(addr pos, size_t *ret);

int lisp_reverse_(addr x, addr pos);
int lisp_nreverse_(addr x, addr pos);
```


## Escape Function `lisp_getelt_`

```c
int lisp_getelt_(addr x, addr pos, size_t index);

Input: pos, sequence
Input: index
Output: x, hold variable
Return: Non-zero when escaping.
```

Returns the `index`-th element of the sequence `pos`.  
If `pos` is a hold variable, its contents are used.  


## Escape Function `lisp_setelt_`

```c
int lisp_setelt_(addr pos, size_t index, addr value);


Input: pos, sequnece
Input: index
Input: value, hold variable
Return: Non-zero when escaping.
```

Sets the `index`-th element of the sequence `pos` to `value`.  
If `pos`, `value` is a hold variable, the content is used.  


## Escape Function `lisp_length_`

```c
int lisp_length_(addr pos, size_t *ret);

Input: pos, sequence
Output: ret, number of elements
Return: Non-zero when escaping.
```

Returns the number of elements in the sequence `pos`.  
If `pos` is a hold variable, the content is used.  


## Escape Function `lisp_reverse_`

```c
int lisp_reverse_(addr x, addr pos);

Input: pos, sequence
Output: x, hold variable
Return: Non-zero when escaping.
```

The reverse order of the `pos` is returned.  
If `pos` is a hold variable, the content is used.  
Because the sequence is newly created, the original sequence is not destroyed.


## Escape Function `lisp_nreverse_`

```c
int lisp_nreverse_(addr x, addr pos);

Input: pos, sequence
Output: x, hold variable
Return: Non-zero when escaping.

```

The reverse order of the `pos` is returned.  
If `pos` is a hold variable, the content is used.  
Destroy the sequence without creating a new one.


# <a id="sequence-3">3. Cons</a>

Function of the cons.

```c
void lisp_car(addr x, addr list);
void lisp_cdr(addr x, addr list);
void lisp_carcdr(addr x, addr y, addr list);

void lisp_setf_car(addr cons, addr value);
void lisp_setf_cdr(addr cons, addr value);
void lisp_setf_carcdr(addr cons, addr car, addr cdr);
```

## Function `lisp_car`

```c
void lisp_car(addr x, addr list);

Input: list
Output: x, hold variable
```

Get the car of the `list`.  
The `list` can be specified with cons or NIL.


## Function `lisp_cdr`

```c
void lisp_cdr(addr x, addr list);

Input: list
Output: x, hold variable
```

Get the cdr of the `list`.  
The `list` can be specified with cons or NIL.


## Function `lisp_carcdr`

```c
void lisp_carcdr(addr x, addr y, addr list);

Input: list
Output: x, y, hold variable
```

Return car of `list` to `x` and cdr to `y`.
The `list` can be specified with cons or NIL.


## Function `lisp_setf_car`

```c
void lisp_setf_car(addr cons, addr value);

Input: cons
Input: value, Object
```

If `value` is a hold variable, the content is used.  
Set the `value` to the car part of the `cons`.


## Function `lisp_setf_cdr`

```c
void lisp_setf_cdr(addr cons, addr value);

Input: cons
Input: value, Object
```

If `value` is a hold variable, the content is used.  
Set the `value` to the cdr part of the `cons`.


## Function `lisp_setf_carcdr`

```c
void lisp_setf_carcdr(addr cons, addr car, addr cdr);

Input: cons
Input: car, cdr, Object
```

If `car`, `cdr` are hold variables, their contents are used.  
Set the `car` to the car part of the `cons`.  
Set the `cdr` to the cdr part of the `cons`.


# <a id="sequence-4">4. String</a>

Function of the string.

```c
int lisp_string8_(addr x, const void *str);
int lisp_string16_(addr x, const void *str);
int lisp_string32_(addr x, const void *str);
int lisp_string_getc_(addr pos, size_t i, unicode *c);
```

# Escape Function `lisp_string8_`

```c
int lisp_string8_(addr x, const void *str);
int lisp_string16_(addr x, const void *str);
int lisp_string32_(addr x, const void *str);

Input: str, string
Output: x, hold variable
Return: Non-zero when escaping.
```

Returns a string object.  
It is a special object of type `simple-string`.  
`str` specifies a string to be stored in the object.   
The memory format of `str` is as follows.

- 8-bit, 16-bit and 32-bit arrays
- UTF-8, UTF-16 and UTF-32 encoding
- Terminate with zero.


# Escape Function `lisp_string16_`

Explained in `lisp_string8_`.


# Escape Function `lisp_string32_`

Explained in `lisp_string8_`.


## Escape Function `lisp_string_getc_`

```c
int lisp_string_getc_(addr pos, size_t i, unicode *c);

Input: pos, string
Input: i, index
Output: c, character
Return: Non-zero when escaping.
```

Get the value of a character from a `strvect` object.  
If `pos` is a hold variable, the content is used.

# <a id="sequence-5">5. Strvect</a>

Function of `strvect`, which is an object for `simple-string`.

```c
int lisp_strvect_getc(addr pos, size_t i, unicode *c);
int lisp_strvect_length(addr pos, size_t *ret);
```


## Function `lisp_strvect_getc`

```c
int lisp_strvect_getc(addr pos, size_t i, unicode *c);

Input: pos, Object
Input: i, Index
Output: c, Character
Return: Normal: 0, Error: non-zero
```

Get the value of a character from a `strvect` object.  
This function is for retrieving a character from a string without escaping.

Unlike `lisp_string_getc_`, it does not support `array` objects.  
If it is not a `strvect` type object,
the function exits with the return value `-1`.  
If `i` is more than the number of characters,
it exits with the return value `1`.  
If the character is returned to `c`,
the function exits with the return value `0`.


## Function `lisp_strvect_length`

```c
int lisp_strvect_length(addr pos, size_t *ret);

Input: pos, Object
Output: ret, Length of a string.
Return: Normal: 0, Error: non-zero
```

Get the length of a string from the `strvect` object.  
This function gets the length of a string without escaping.

Unlike `lisp_length_`, it works only with `strvect` objects.  
If it is not a `strvect` type object,
the function exits with the return value `-1`.  
If the length of the string is returned to `ret`,
the function exits with the return value `0`.
