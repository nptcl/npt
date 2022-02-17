% Paper Object

Npt documentation.

Reference: [ANSI Common Lisp npt](index.html)  
Prev: [7. Start Up](B7_StartUp.html)


# 8.1 Paper Object

Paper object is a data format specific to npt.  
It can hold both one-dimensional arrays, such as `vector`,
and array data in byte format.  
It has the advantage of being efficient in terms of memory space,
since it creates an object that fits the implementation of npt.

This object is mainly intended to be used on the C language.  
For example, it is useful for storing your own data on the Common Lisp heap.  
Operation functions are provided for both C and Common Lisp.


# 8.2 Object format

An object in npt can be represented in one of the following three formats

- Array format
- Body format
- Array-Body format

The array format is the same as one-dimensional `simple-vector`.  
The body format has a buffer area in bytes.  
The array-body format is a combination of the array format and the body format.

The array-body format has a limitation:
the size of both array and body must be less than or equal to `#xFFFF`.  
If you want to use a larger length,
for example, allocate the array and body formats separately.

Every object has a 1-byte data area called the User value.  
You can use the User value freely,
but it is recommended to use it to classify Paper objects.  
The default value of the User value is `0`.


# 8.3 Array format

This section explains how to use the array format.  
The array format is the same as `simple-vector`,
which is a one-dimensional general array.  
To create an array, do the following.

```lisp
(make-paper size 0)
```

The first argument, `size`, is the number of elements in the array.  
For example, to create a paper with 10 elements, the following is used.

```lisp
(make-paper 10 0)
```

The initial value is `nil` for all elements.

To get the value of an array, do the following

```lisp
(array-paper paper index)
```

To set the value, do the following

```lisp
(array-paper paper index value)
```

Compare the following execution with the `vector` operation.

```lisp
(setq x (make-paper 10 0))
(array-paper x 3)
(array-paper x 3 "Hello")
```

In `vector`, it is represented as follows.

```lisp
(setq x (make-array 10))
(aref x 3)
(setf (aref x 3) "Hello")
```

Next, we will do the same thing in C.  
To create an object, use the following function

```c
int lisp_paper_(addr x, size_t array, size_t body);
```

The following is an example of execution.

```c
static int main_call_(void)
{
    addr x;

    x = Lisp_hold();
    if (lisp_paper_(x, 10, 0))
        return 1;

    return 0;
}

int main_lisp(void *ignore)
{
    addr control;

    lisp_push_control(&control);
    (void)main_call_();
    return lisp_pop_control_(control);
}
```

To execute it correctly, you need to do the above.  
However, since the return value is not necessary for the explanation,
it is omitted and represented as follows

```c
addr x;

x = Lisp_hold();
lisp_paper_(x, 10, 0);
```

The following functions are used to get and set the array.

```c
int lisp_paper_getarray_(addr x, addr pos, size_t index);
int lisp_paper_setarray_(addr x, size_t index, addr value);
```

The following example is expressed in C.

```lisp
(setq x (make-paper 10 0))
(array-paper x 3)
(array-paper x 3 "Hello")
```

```c
addr x, v;

x = Lisp_hold();
v = Lisp_hold();
lisp_paper_(x, 10, 0);
lisp_paper_getarray_(v, x, 3);
lisp_string8_(v, "Hello");
lisp_paper_setarray_(x, 3, v);
```


# 8.4 Body format

This section explains how to use the body format.  
The body format is the same as a one-dimensional specialized array
of the form `(unsigned-byte 8)`.  
To create a body, do the following

```lisp
(make-paper 0 size)
```

The second argument, `size`, is the number of bytes of the buffer to allocate.  
For example, to create a 10-byte Paper object, the following is used.

```lisp
(make-paper 0 10)
```

The initial value is undefined.  
Unlike the array format, the body format does not initialize
the content of the value.  
If you need to initialize it, use the `:fill` argument.

In the following example, we initialize it with `#x00` in the allocated body part.

```lisp
(make-paper 0 10 :fill t)
```

Alternatively, you can specify the initial value as follows

```lisp
(make-paper 0 10 :fill #xFF)
```

The range of values is between `0` and `#xFF`.  
To get the value of an array, do the following

```lisp
(body-paper paper index)
```

The returned value is an integer between `0` and `#xFF`.  
To set the value, do the following

```lisp
(body-paper paper index value)
```

The `value` must be an integer between `0` and `#xFF`.  
Use the following functions to get and set the buffer.

```c
int lisp_paper_getbody_(addr x, size_t index, byte *ret);
int lisp_paper_setbody_(addr x, size_t index, byte value);
```

The following example is expressed in C.

```lisp
(setq x (make-paper 0 10))
(body-paper x 3)
(body-paper x 3 #xFF)
```

```c
byte value;
addr x;

x = Lisp_hold();
lisp_paper_(x, 0, 10);
lisp_paper_getbody_(x, 3, &value);
lisp_paper_setbody_(x, 3, 0xFF);
```

# 8.5 Array-Body format

This section explains how to use the array-body format.

```lisp
(make-paper size1 size2)
```

The first argument, `size1`, is the number of array arrays.  
The second argument, `size2`, is the number of bytes in the body buffer.  
For example, if you want to create a Paper object with 10 elements in array
and 20 bytes in body, the following will be used.

```lisp
(make-paper 10 20)
```

When creating an array-body format,
both `size1` and `size2` must be less than or equal to `#xFFFF`.  
If violated, a `simple-error` condition will be raised.


# 8.6 `info-array` function

The `info-array` function performs the following operations on the Paper object.

- Getting and Setting the User Value
- Get all contents
- Getting the length

## 8.6.1 Getting and Setting the User Value

User value is a 1-byte information held by an object.  
If `type` is specified in the `info-paper` function, the User value will be returned.

```lisp
(setq x (make-paper 3 4))
-> #<PAPER 0 #x801699220>

(info-array paper 'type)
-> 0
```

User values can be specified with the `:type` argument of `make-array`.

```lisp
(setq x (make-paper 3 4 :type 123))
-> #<PAPER 123 #x80169b258>

(info-paper x 'type)
-> 123
```

Or, you can set it with the argument of `info-array`.

```lisp
(info-paper x 'type 100)
-> 100

(info-paper x 'type)
-> 100

x
-> #<PAPER 100 #x80169b258>
```

In C language, the following functions are used.

```c
int lisp_paper_gettype_(addr x, byte *ret);
int lisp_paper_settype_(addr x, byte value);
```

The following is an example of execution.

```c
byte value;
addr x;

x = Lisp_hold();
lisp_paper_(x, 0, 10);
lisp_paper_settype_(x, 123);
lisp_paper_gettype_(x, &value);
printf("%d\n", (int)value);
```


## 8.6.2 Get all contents

The contents of array and body can be retrieved in `list` or `vector` format.  
Let's try to get the content of an array.  
To get them, specify `list` or `vector` to the `info-paper` function.

```lisp
(setq x (make-paper 3 4 :fill 7))
-> #<PAPER 0 #x80174f258>

(info-paper x 'list)
-> (NIL NIL NIL)

(info-paper x 'vector)
-> #(NIL NIL NIL)
```

If you want to retrieve the content of a body,
specify `t` as the third argument of `info-paper`.

```lisp
(setq x (make-paper 3 4 :fill 7))
-> #<PAPER 0 #x801752c28>

(info-paper x 'list t)
-> (7 7 7 7)

(info-paper x 'vector t)
-> #(7 7 7 7)
```

The `vector` of the body part is returned
as a specialized array of `(unsigned-byte 8)`.


## 8.6.3 Getting the length

Get the length of array and body.  
To get them, specify `length` to the `info-paper` function.

```lisp
(setq x (make-paper 3 4))
-> #<PAPER 0 #x801756418>

(info-paper x 'length)
-> 3

(info-paper x 'length t)
-> 4
```

In C language, the following functions are used.

```c
int lisp_paper_lenarray_(addr x, size_t *ret);
int lisp_paper_lenbody_(addr x, size_t *ret);
```

The following is an example of execution.

```c
addr x;
size_t array, body;

x = Lisp_hold();
lisp_paper_(x, 5, 6);
lisp_paper_lenarray_(x, &array);
lisp_paper_lenbody_(x, &body);
printf("%d, %d\n", (int)array, (int)body);
```


# 8.7 Other contents

The following contents will be explained.

- Paper objects and types
- handling in `compile-file`
- Other C language functions


## 8.7.1 Paper objects and types

The Paper object is a normal Lisp object in npt.  
It can be evaluated with `eval` and has its own type.

The evaluation of the type is done as follows.

```lisp
(typep x 'npt-system::paper)
```

There are also classes that belong to the `built-in-class`.

```lisp
(find-class 'npt-system::paper)
-> #<BUILT-IN-CLASS PAPER>
```


## 8.7.2 handling in `compile-file`

Paper objects can be saved with `compile-file`.  
The array part is treated like a `vector` object.  
The body part writes the buffer contents directly to the fasl file.  
User values are saved in the same way.


## 8.7.3 Other C language functions

The following functions are provided.

```c
int lisp_paper_getmemory_(addr x, size_t a, size_t b, void *output, size_t *ret);
int lisp_paper_setmemory_(addr x, size_t a, size_t b, const void *input, size_t *ret);
```

The function `lisp_paper_getmemory_` gets the contents of a body part at once.  
The function `lisp_paper_setmemory_` sets the contents of a body part at once.

The arguments `a` and `b` indicate the position of the body part.  
They indicate the contents from the `a`th to just before the `b`th.  
As with the `subseq` function, the position of the `b`th byte is not included.  
It starts at the `a`th position and processes the contents of `(- b a)` bytes.

The values of `a` and `b` may exceed the size of the body part.  
The number of bytes processed will be stored in the argument `ret`.  
If the argument `ret` is `NULL`, it is ignored.
