% C Function: Print

Npt documentation.

Reference: [ANSI Common Lisp npt](index.html)  


# Function Specification

The following function specifications are described in `lisp.h`.

- [1. format](#print-1)

```c
int lisp_format8_(addr stream, const void *str, ...);
int lisp_format16_(addr stream, const void *str, ...);
int lisp_format32_(addr stream, const void *str, ...);
```

- [2. Standard Ouptut](#print-2)

```c
int lisp_stdout8_(const void *str, ...);
int lisp_stdout16_(const void *str, ...);
int lisp_stdout32_(const void *str, ...);
```

- [3. stringf](#print-3)

```c
int lisp_stringf8_(addr x, const void *str, ...);
int lisp_stringf16_(addr x, const void *str, ...);
int lisp_stringf32_(addr x, const void *str, ...);
```


# <a id="print-1">1. format</a>

Functions of the `format`.

```c
int lisp_format8_(addr stream, const void *str, ...);
int lisp_format16_(addr stream, const void *str, ...);
int lisp_format32_(addr stream, const void *str, ...);
```


## Escape Function `lisp_format8_`

```c
int lisp_format8_(addr stream, const void *str, ...);
int lisp_format16_(addr stream, const void *str, ...);
int lisp_format32_(addr stream, const void *str, ...);

Input: stream, stream or T or NULL.
Input: str, unicode string.
Return: Non-zero when escaping.
```

Call the `format` function.  
If `stream` is `NULL`, it is the same as `T`.  
If `stream` is `T`, the output will be written to `*standard-output*`.  
If `stream` is `NIL`, the string is created, but its content is discarded.  
If `stream` is a hold variable, the contents will be used.  
See the `lisp_string8_` function for details on Unicode strings.


## Escape Function `lisp_format16_`

See `lisp_format8_`.


## Escape Function `lisp_format32_`

See `lisp_format8_`.


# <a id="print-2">2. Standard Ouptut</a>

Function to output to `*standard-output*`.

```c
int lisp_stdout8_(const void *str, ...);
int lisp_stdout16_(const void *str, ...);
int lisp_stdout32_(const void *str, ...);
```


## Escape Function `lisp_stdout8_`

```c
int lisp_stdout8_(const void *str, ...);
int lisp_stdout16_(const void *str, ...);
int lisp_stdout32_(const void *str, ...);

Input: str, unicode string.
Return: Non-zero when escaping.
```

Call the `format` function with `*standard-output*`.  
It is equivalent to `lisp_format8_(NULL, str, ...)`.  
See the `lisp_string8_` function for details on Unicode strings.


## Escape Function `lisp_stdout16_`

See `lisp_stdout8_`.


## Escape Function `lisp_stdout32_`

See `lisp_stdout8_`.


# <a id="print-3">3. stringf</a>

Function returns the result of `format` as a string.

```c
int lisp_stringf8_(addr x, const void *str, ...);
int lisp_stringf16_(addr x, const void *str, ...);
int lisp_stringf32_(addr x, const void *str, ...);
```


## Escape Function `lisp_stringf8_`

```c
int lisp_stringf8_(addr x, const void *str, ...);
int lisp_stringf16_(addr x, const void *str, ...);
int lisp_stringf32_(addr x, const void *str, ...);

Input: str, unicode string.
Output: x, hold variable.
Return: Non-zero when escaping.
```


Call the `format` function to return a string.  
It is equivalent to `(format nil str ...)`.  
See the `lisp_string8_` function for details on Unicode strings.


## Escape Function `lisp_stringf16_`

See `lisp_stringf8_`.


## Escape Function `lisp_stringf32_`

See `lisp_stringf8_`.
