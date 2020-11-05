% Lisp Function: System Function

Npt documentation.

Reference: [ANSI Common Lisp npt](index.html)


# Lisp Function Specification

The following functions of the `npt-system` package are described.

- [1. System Function](#system-develop)

```lisp
defun gc
defun savecore
defun exit
defun quit
```


- [2. Object Type](#object-check)

```lisp
defun specialp
defun array-general-p
defun array-specialized-p
defun closp
defun fixnump
defun bignump
defun ratiop
defun short-float-p
defun single-float-p
defun double-float-p
defun long-float-p
defun callnamep
```


- [3. Object Creation](#object-make)

```lisp
defun make-character
defun make-fixnum
defun make-bignum
defun make-ratio
defun make-complex
defun make-callname
```


- [4. Type Check](#type-check)

```lisp
defun subtypep-result
defun parse-type
defun type-object
```


- [5. Memory Stream](#memory-stream)

```lisp
defun make-memory-input-stream
defun make-memory-output-stream
defun make-memory-io-stream
defmacro with-input-from-memory
defmacro with-output-to-memory
defun get-output-stream-memory
defun memory-stream-p
defun (setf memory-stream-p)
```


- [6. Sort](#sort)

```lisp
defun simple-sort
defun bubble-sort
defun quick-sort
defun merge-sort
```


- [7. Other Functions](#others)

```lisp
defun package-export-list
defun large-number
defun equal-random-state
defun remove-file
defun remove-directory
defun byte-integer
defun eastasian-set
defun eastasian-get
defun eastasian-width
```


# <a id="system-develop">1. System Function</a>

These are function specifications for system functions in the `npt-system` package.

```lisp
defun gc
defun savecore
defun exit
defun quit
```


## The Function `gc`

Launches the garbage collector.

```lisp
(defun gc (&key full) ...) -> null

Input: full general-boolean
Output: null return nil
```

This function requests the garbage collector.  
This function does not execute the garbage collector in the function,
but at a convenient time for the implementation.  
The `:full` argument is ignored for now.  
The garbage collector is checked with the `room` function.

### Example

```lisp
* (room)
...
GC count:           1                   [times]
...
NIL
* (npt-system:gc)
NIL
* (room)
...
GC count:           2                   [times]
...
NIL
*
```


## The Function `savecore`

Quit Lisp and then create the core file.

```lisp
(defun savecore (pathname-designer) ...) -> null

Input: pathname-designer  output path
Output: null No return value
```

The core file is a memory image file. 
The core file is read from the core file if the `--core` and `--corefile` arguments are specified at the startup of npt.

The function `savecore` executes the `savecore condition` and exits Lisp.  
The `handler` can be caught in the middle.
If this function works correctly, Lisp exits,
so the process itself is usually terminated.
If it is used as a module, control is returned from
the `lisp_argv_run` function of the C language. 

### Example

```lisp
$ npt
* (defvar *hello* 1234)
*HELLO*
* *hello*
1234
* (npt-system:savecore #p"hello-core-image.core")
Core file: hello-core-image.core
$ ls
hello-core-image.core
$ npt --core --corefile hello-core-image.core
* *hello*
1234
*
```

## The Function `exit`
## The Function `quit`

Exit Lisp.

```lisp
(defun exit (&optional code) ...) -> none
(defun quit (&optional code) ...) -> none

Input: code exit-code, default is 0.
```

The function of `exit` and `quit` is the same.

Because this function terminates Lisp, there is no return value of the expression.  
The exit code of the argument is set to the exit code of the process usually.  
If it is used as a module, control is returned from
the `lisp_argv_run` function of the C language. 
The return value is stored in the `lisp_result` variable.

The function `exit`/`quit` executes the `exit condition` and exits Lisp.  
The `handler` can be caught in the middle.

The `exit` and `quit` `symbol` begins with
imported into the `common-lisp-user` package.

### Example

```lisp
$ npt
* (quit)
$ echo $?
0
$ npt --eval '(exit 22)'
$ echo $?
22
$
```


# <a id="object-check">2. Object Type</a>

These are function specifications for object checks in the `npt-system` package.

```lisp
defun specialp
defun array-general-p
defun array-specialized-p
defun closp
defun fixnump
defun bignump
defun ratiop
defun short-float-p
defun single-float-p
defun double-float-p
defun long-float-p
defun callnamep
```

## The Function `specialp`

Examine whether the global variable is special or not.

```lisp
(defun specialp (symbol) ...) -> boolean

Input: symbol
Output: boolean
```

Returns whether the variable is special or not,
without considering the lexical situation.
That is, the variable is `t` if it becomes a special variable
by the execution of `defvar`, `defparameter`, `declaim`, or `proclaim`.

### Example

```lisp
* (specialp 'aaa)
NIL
* (defvar bbb)
BBB
* (specialp 'bbb)
T
* (let (ccc) (declare (special ccc)) (specialp 'ccc))
NIL
```


## The Fuction `array-general-p`

Checks if the argument is a general array.

```lisp
(defun array-general-p (object) ...) -> boolean

Input: object
Output: boolean
```

If the input is an `array` object and it is a general array, the function returns `t`.  
Unlike `typep`, the decision is based on the object type in npt.  
If the input is generated by `make-array` 
and `element-type` is `t`, then `t` is returned.
For example, `#(10 20 30)` is a `vector` object and `t` is not returned.

### Example

```lisp
* (array-general-p (make-array 10))
T
* (array-general-p #(10 20 30))
NIL
* (typep (make-array 10) '(array t))
T
* (typep #(10 20 30) '(array t))
T
```


## The Fuction `array-specialized-p`

Checks if the argument is a specialized array.

```lisp
(defun array-specialized-p (object) ...) -> boolean

Input: object
Output: boolean
```

If the input is an `array` object and
the `element-type` is not `t`, `t` is returned.


## The Fuction `closp`

Checks if the argument is a `clos` object.

```lisp
(defun closp (object) ...) -> boolean

Input: object
Output: boolean
```


If the input is a `clos` object, `t` is returned.  
In Common Lisp, since all CLOS objects belong to the `standard-object`,
this should normally be the same as the following command.

```lisp
(typep object 'standard-object)
```


## The Fuction `fixnump`

Checks if the argument is a `fixnum` object.

```lisp
(defun fixnump (object) ...) -> boolean

Input: object
Output: boolean
```

If the input is a `fixnum` object, `t` is returned.  
Normally, every `integer` that belongs to the `fixnum` type should be a `fixnum` object.  
However, in development, it is possible to have a small `integer`, say `10` or `20`, even though it is of type `bignum`, for example.  
This function is for investigating the differences between the above.


### Example

```lisp
* 10
10
* (fixnump 10)
T
* (make-bignum 20)
20
* (fixnump (make-bignum 20))
NIL
```


## The Fuction `bignump`

Checks if the argument is a `bignum` object.

```lisp
(defun bignump (object) ...) -> boolean

input: object
Output: boolean
```

If the input is a `bignum` object, `t` is returned.  

### Example

```lisp
* 10
10
* (bignump 10)
NIL
* (make-bignum 20)
20
* (bignump (make-bignum 20))
T
```


## The Fuction `ratiop`

Checks if the argument is a `ratio` object.

```lisp
(defun ratiop (object) ...) -> boolean

Input: object
Output: boolean
```

If the input is a `ratio` object, `t` is returned.  
Normally, fractions with a denominator of `1` should be of type `integer`.  
But in development, for example, `100/1` may exist in a `ratio` object.  
This function is for investigating the differences between the above.


### Example

```lisp
* 10/5
2
* (ratiop 10/5)
NIL
* (make-ratio 10 5)
10/5
* (ratiop (make-ratio 10 5))
T
```


## The Fuction `short-float-p`

Checks if the argument is a `short-float` object.

```lisp
(defun short-float-p (object) ...) -> boolean

Input: object
Output: boolean
```

Currently, there is no way to create a `short-float` object in npt.


## The Fuction `single-float-p`

Checks if the argument is a `single-float` object.

```lisp
(defun single-float-p (object) ...) -> boolean

Input: object
Output: boolean
```


## The Fuction `double-float-p`

Checks if the argument is a `double-float` object.

```lisp
(defun double-float-p (object) ...) -> boolean

Input: object
Output: boolean
```


## The Fuction `long-float-p`

Checks if the argument is a `long-float` object.

```lisp
(defun long-float-p (object) ...) -> boolean

Input: object
Output: boolean
```


## The Fuction `callnamep`

Checks if the argument is a `callname` object.

```lisp
(defun callnamep (object) ...) -> boolean

Input: object
Output: boolean
```

The `callname` is an object for the name of a function,
which is a combination of the usual `symbol`,
called `car`, and the `(setf car)`type for the setf function.  
There is no way to create a `callname` object in Common Lisp.


# <a id="object-make">3. Object Creation</a>

These are function specifications for object creation in the `npt-system` package.

```lisp
defun make-character
defun make-fixnum
defun make-bignum
defun make-ratio
defun make-complex
defun make-callname
```


## The Fuction `make-character`

Duplicate a `character` object.

```lisp
(defun make-character (character) ...) -> character

Input: character
Output: character
```

Duplicate a character object received on input and return it.  
The purpose of this function is to create a new object, avoiding the cache.  
In npt, if the character code is less than `#x80`, the same object is returned.  
Therefore, `eq` returns `t`.

```lisp
* (eq #\A #\A)
T
* (eq #\A (read-from-string "#\\A"))
T
```

If a different object is needed, this function will duplicate it.

```lisp
* (eq #\A (make-character #\A))
NIL
* (eql #\A (make-character #\A))
T
```


## The Fuction `make-fixnum`

Duplicate a `fixnum` object.

```lisp
(defun make-fixnum (fixnum) ...) -> fixnum

Input: fixnum
Output: fixnum
```

Duplicate a fixnum object received on input and return it.  
The purpose of this function is to create a new object, avoiding the cache.  
In npt, if the value is an integer between `-1024` and `+1024`,
the same object is returned.  
Therefore, `eq` returns `t`.


```lisp
* (eq 11 11)
T
* (eq 11 (read-from-string "11"))
T
```

If a different object is needed, this function will duplicate it.

```lisp
* (eq 11 (make-fixnum 11))
NIL
* (eql 11 (make-fixnum 11))
T
```


## The Fuction `make-bignum`

Create a `bignum` object.

```lisp
(defun make-bignum (integer) ...) -> bignum

Input: integer
Output: bignum
```

Creates a `bignum` object from an integer received at the input.  
The purpose of this function is to create a `bignum` object with a small integer value.  
Normally, an integer, say `10` or `2000`, will be returned as a `fixnum` object.  
This function is used to force the creation of a `bignum` object. 

Caution.  
Npt does not expect that integers originally
in the range of `fixnum` are passed by `bignum` objects.  
All results of using this function are undefined.

### Example

```lisp
* 10
10
* (make-bignum 10)
10
* (fixnump 10)
T
* (bignump 10)
NIL
* (fixnump (make-bignum 10))
NIL
* (bignump (make-bignum 10))
T
```


## The Fuction `make-ratio`

Create a `ratio` object.

```lisp
(defun make-ratio (numer denom) ...) -> ratio

Input: numer An integer object representing the numerator
Input: denom An integer object representing the denominator
Output: ratio
```

Creates a `ratio` object from an integer `numer`, `denom` received at the input.  
The purpose of this function is to create a `ratio` object ignoring the divisor.  
Normally, the fraction, say `10/5`, is divided by the divisor
and the `fixnum` object, say `2`, is returned.  
This function is used to force the creation of a `ratio` object. 

Caution.  
Npt does not expect what is supposed to be an `integer`
to be passed as a `ratio` object.  
All results of using this function are undefined.

### Example

```lisp
* 10/5
2
* (ratiop 10/5)
NIL
* (make-ratio 10 5)
10/5
* (ratiop (make-ratio 10 5))
T
```


## The Fuction `make-complex`

Create a `complex` object.

```lisp
(defun make-complex (real imag) ...) -> complex

Input: real A real object representing a real number
Input: imag A real object representing an imaginary number
Output: complex
```

Creates a `complex` object from an integer `real`, `imag` received at the input.  
The purpose of this function is to create a `complex` object of an integer type
with an imaginary number of `0`.  
Normally, the number `#c(10 0)`, for example,
would return a `fixnum` object of `10`.  
This function is used to force the creation of a `complex` object. 

Caution.  
Npt does not expect what is supposed to be an `integer`
to be passed as a `complex` object.  
All results of using this function are undefined.

### Example

```lisp
* #c(10 0)
10
* (complexp #c(10 0))
NIL
* (make-complex 10 0)
#C(10 0)
* (complexp (make-complex 10 0))
T
```


## The Fuction `make-callname`

Create a `callname` object.

```lisp
(defun make-callname (x) ...) -> callname

Input: x function-name
Output: callname
```

Create a `callname` object for the function name.  
The input can be of type `symbol` or `(setf symbol)`.


# <a id="type-check">4. Type Check</a>

These are function specifications for type checking in the `npt-system` package.

```lisp
defun subtypep-result
defun parse-type
defun type-object
```


## The Fuction `subtypep-result`

Get the result of `subtypep` with `symbol`.

```lisp
(defun subtypep-result (left right) ...) -> symbol

Input: left type-specifier
Input: right type-specifier
Output: symbol
```

Unlike `subtypep`, this function checks whether the type is exclusive or not.  
The return values are as follows.

- Return `npt-system::include` if included.
- Return `npt-system::exclude` if exclusived.
- Return `npt-system::false` if it is not included and there are overlapping parts.
- Return `npt-system::invalid` if it cannot be determinant.


## The Fuction `parse-type`

Parsing the type-specifier.

```lisp
(defun parse-type (object) ...) -> type

Input: object type-specifier
Output: type
```

The main purpose is to investigate whether the type is of the correct form.  
The return value is a type object,
but we don't use type objects in normal Common Lisp.


## The Fuction `type-object`

Creates a Lisp object from a type object.

```lisp
(defun type-object (type) ...) -> result

Input: type
Output: result (or cons symbol)
```

Generates a type name from the type object of the input.


# <a id="memory-stream">5. Memory Stream</a>

These are function specifications for memory stream in the `npt-system` package.

```lisp
defun make-memory-input-stream
defun make-memory-output-stream
defun make-memory-io-stream
defmacro with-input-from-memory
defmacro with-output-to-memory
defun get-output-stream-memory
defun memory-stream-p
defun (setf memory-stream-p)
```

## Explanation

`memory-stream` is an alternative stream for files.  
As usual files have `byte` as the unit of data,
this stream has `(unsigned-byte 8)` as the basic unit of data.  
Because it is a binary stream,
string-type stream functions such as the `read-char` function cannot be used.
Binary manipulation functions such as the `read-byte` function are available.

The `memory-stream` can be used for the `filespec` of the `open` function.

```lisp
(with-open-stream (file (make-memory-input-stream #(#x48 #x65 #x6C #x6C #x6F)))
  (with-open-file (stream file)
    (read-line stream)))
-> "Hello", T
```

Although two streams are duplicated,
the file pointer can be used only with the `open` one.
The file pointer of `memory-stream` is manipulated by the `open` function.

In the above case, `(open file)`'s `:input` operation,
which is executed in the `with-open-file`,
executes first `(file-position file :start)` for the
`memory-stream` stored in the `file`.  
When reading data by the `read-line` function occurs,
the file pointer of `memory-stream` is moved forward at the same time
when the file pointer of `stream` is moved forward.

The `memory-stream` has the following four parameters.

- `:input` Initial value
- `:size` The number of bytes in the internal buffer.
- `:array` Initialization of the number of internal buffers.
- `:cache` Cache usage.

The `:input` is the initial value of `memory-stream`.  
It must be a `sequence` of `(unsigned-byte 8)`.

The `:size` is the size of the internal buffer of the `memory-sequence`.  
If it is omitted, the size is `64` bytes
if `LISP_DEBUG` is specified at compile time, otherwise it is `4096` bytes.  
The specified number of buffers is created in the `memory-stream`.

The `:array` is the first number of internal buffer arrays to hold.  
If omitted, it is `4` if `LISP_DEBUG` is specified at compile time,
otherwise it is `8`.  
For example, if more than `4,096` bytes*`8` of data are required,
the number of arrays will be doubled from `8` arrays to `16` arrays,
then `32` arrays, then `64` arrays, then ......, and so on.

The `:cache` enables or disables the cache.  
This argument is for debugging in development.  
The cache has no effect on the `memory-stream`,
but when it is passed to the `open` function,
the generated `file-stream` is selected to use the cache or not.  
Unlike normal files, all the data of `memory-stream` is located in memory,
so there is no need to `ON` caching.  
The default is `T` (enabled) if `LISP_DEBUG` is specified at compile time,
otherwise it is `NIL` (disabled).

There are three types of `memory-stream`: input, output, and input/output.
Because the same object is used internally,
you can change the type of memory stream at any time.


## The Fuction `make-memory-input-stream`

Creates a `memory-stream` for input only.

```lisp
(defun make-memory-input-stream (sequence &key size array cache) ...) -> stream

Input: sequence
Input: size (or null (integer 1 *))
Input: array (or null (integer 1 *))
Input: cache t  ;; boolean
Output: stream input-memory-stream
```

`sequence`is used as the initial value in the stream.    
`size`is the size of the internal buffer, and the initial value is `4096` bytes.    
`array`is the initial number of buffers, and the initial value is `8`.  
`cache` is for development.


## The Fuction `make-memory-output-stream`

Creates a `memory-stream` for output only.

```lisp
(defun make-memroy-output-stream (&key input size array cache) ...) -> stream

Input: input sequence
Input: size (or null (integer 1 *))
Input: array (or null (integer 1 *))
Input: cache t  ;; boolean
Output: stream output-memory-stream
```

`sequence`is used as the initial value in the stream.    
`size`is the size of the internal buffer, and the initial value is `4096` bytes.    
`array`is the initial number of buffers, and the initial value is `8`.  
`cache` is for development.


## The Fuction `make-memory-io-stream`

Create a `memory stream` for input and output.

```lisp
(defun make-memroy-io-stream (&key input size array cache) ...) -> stream

Input: input sequence
Input: size (or null (integer 1 *))
Input: array (or null (integer 1 *))
Input: cache t  ;; boolean
Output: stream io-memory-stream
```

`sequence`is used as the initial value in the stream.    
`size`is the size of the internal buffer, and the initial value is `4096` bytes.    
`array`is the initial number of buffers, and the initial value is `8`.  
`cache` is for development.


## The Macro `with-input-from-memory`

The `memory-stream` version of the macro `with-input-from-string`.

```lisp
(defmacro with-input-from-memory
  ((stream vector &key size array) declaration* form*) ...)
  -> result
```

Unlike `string-stream`, there is no argument `index`.


## The Macro `with-output-to-memory`

The `memory-stream` version of the macro `with-output-to-string`.

```lisp
(defmacro with-output-to-memory
  ((var &key input size array) declaration* form*) ...)
  -> result
```

The return value of `get-output-stream-memory` is used.  
Unlike `string-stream`, the second argument, `array`, is not supported.  
The return value is an array of `(array (unsigned-byte 8))`.


## The Fuction `get-output-stream-memory`

Return an array of all data held by `memory-stream`.  

```lisp
(defun get-output-stream-memory (stream) ...) -> vector

Input: stream memory-stream
Output: vector (array (unsigned-byte 8))
```

The argument is not only the `memory-stream` for output,
but also accepts input and output.  Unlike `string-stream`, the content is not deleted after outputting a value.  
The return value is an array of `(array (unsigned-byte 8))`.


## The Accessor `memory-stream-p`

Check whether the argument is `memory-stream` or not.

```lisp
(defun memory-stream-p (object) ...) -> result

Input: object
Output: result (member :input :output :io nil)
```

If it is not `memory-stream`, `nil` is returned.  
If it is an `input-memory-stream`, `:input` is returned.  
If it is an `output-memory-stream`, `:output` is returned.  
If it is an `io-memory-stream`, `:io` is returned.


## The Accessor `(setf memory-stream-p)`

Change the type of `memory-stream`.

```lisp
(defun (setf memory-stream-p) (result stream) ...) -> result
Input: stream memory-stream
Input: result  (member :input :output :io)
```

For example, change an input `memory-stream` to an input/output `memory-stream`.  
A value of `:input` is input, `:output` is output, and `:io` is input/output.

Each `memory-stream` has the same kind of object.
Setting the type changes the availability of the
input and output functions of the `stream` (e.g. `read-byte`).

### Example

```lisp
* (setq x (make-memory-output-stream))
#<STREAM MEMORY-OUTPUT #x8012801e0>
* (write-sequence '(65 66 67) x)
(65 66 67)
* (file-position x :start)
T
* (with-open-file (s x) (read-line s))
  -> error
* (memory-stream-p x)
:OUTPUT
* (setf (memory-stream-p x) :input)
:INPUT
* (with-open-file (s x) (read-line s))
"ABC"
T
*
```


# <a id="sort">6. Sort</a>

These are function specifications for sorting in the `npt-system` package.

```lisp
defun simple-sort
defun bubble-sort
defun quick-sort
defun merge-sort
```

## The Fuction `simple-sort`

Sort selection.

```lisp
(defun simple-sort (sequence call &key key) ...) -> result

Input: sequence
Input: call Function
Input: key Function
Output: result sequence
```

It is not stable and does an `O(n^2)` sort.


## The Fuction `bubble-sort`

Bubble sort.

```lisp
(defun bubble-sort (sequence call &key key) ...) -> result

Input: sequence
Input: call Function
Input: key Function
Output: result sequence
```

It is stable and does an `O(n^2)` sort.


## The Fuction `quick-sort`

Quick sort.

```lisp
(defun quick-sort (sequence call &key key) ...) -> result

Input: sequence
Input: call Function
Input: key Function
Output: result sequence
```

It is not stable and does an `O(n log n)` sort.


## The Fuction `merge-sort`

Merge sort.

```lisp
(defun merge-sort (sequence call &key key) ...) -> result

Input: sequence
Input: call Function
Input: key Function
Output: result sequence
```

It is stable and does an `O(n log n)` sort.


# <a id="others">7. Other Functions</a>

These are function specifications for other function in the `npt-system` package.

```lisp
defun package-export-list
defun large-number
defun equal-random-state
defun remove-file
defun remove-directory
defun byte-integer
defun eastasian-set
defun eastasian-get
defun eastasian-width
```

## The Fuction `package-export-list`

Get the `export` list of the `package`.

```lisp
(defun package-export-list (package-designer) ...) -> list

Input: package-designer
Output: list
```

Get the list of names of symbols which are exported in the package.


## The Fuction `large-number`

Get the notation for a comma-separated list of 3-digit numbers in English.

```lisp
(defun large-number (value &optional (cardinal t)) ...) -> string

Input: value integer
Input: cardinal boolean
Output: string
```

Separate each number by three digits and
return the `n-1`th string from the right.  

Considering the following numbers

```
1,000,000
```

The argument 0 corresponds to the comma on the right, so it is `thousand`.  
The argument 1 corresponds to the second comma from the right, so it is `million`.  
The argument `cardinal` is `nil` and it becomes ordinal number.

### Example

```lisp
* (large-number 0)
"thousand"
* (large-number 1)
"million"
* (large-number 1000)
"millinillion"
* (large-number 5555)
"quintilliquinquinquagintaquingentillion"
* (large-number 5555 nil)
"quintilliquinquinquagintaquingentillionth"
```


## The Fuction `equal-random-state`

Check whether `random-state` is equal.

```lisp
(defun equal-random-state (a b) ...) -> boolean

Input: a random-state
Input: b random-state
Output: boolean
```


## The Fuction `remove-file`

Remove the file.

```lisp
(defun remove-file (pathname &optional (error t)) ...) -> boolean

Input: pathname
Input: errorp
Output: boolean
```

Unlike the standard function `delete-file`,
the `errorp` argument can handle failure cases.
By default, `errorp` is `t`, which is the same as `delete-file`.  
If `errorp` is `nil`, the standard function returns `t`
if the deletion succeeds, and `nil` if it fails.


## The Fuction `remove-directory`

Remove the directory.

```lisp
(defun remove-directory (pathname &optional (error t)) ...) -> boolean

Input: pathname
Input: errorp
Output: boolean
```

If `errorp` is `t`, the `error condition` is executed when the deletion fails.  
If `errorp` is `nil`, `t` is returned when the deletion succeeds,
and `nil` is returned when the deletion fails.


## The Fuction `byte-integer`

Considering the endianness of the CPU, concatenate the values of
`unsigned-byte` and return an integer.

```lisp
(defun byte-integer (&rest args) ...) -> result

Input: args (unsigned-byte 8)
Output: result (integer 0 *)
```

For example, it is used to get the number of bytes `#x00 #x01`
for an unsigned 16-bit integer `uint16_t`.  
If the CPU is big endian, the upper byte is `#x00` and
the lower byte is `#x01`, so the return value is `#x01=1`.  
On the other hand, if the CPU is a little endian,
the return value is `#x0100=256`.


## The Fuction `eastasian-set`

Set the number of characters for each of the East Asian Width categories.

```lisp
(defun eastasian-set (string-designer intplus &optional error) ...) -> boolean) */

Input: string-designer
Input: intplus (integer 0 *)
Input: error boolean
Output: boolean
```

EastAsianWidth is a representation of Unicode character width.  
The input `string-designer` receives six categories,
`N`, `A`, `H`, `W`, `F`, and `NA`.  
For each category, the number of characters specified in the `intplus` is set.
If `error` is `t`, an error occurs if the category does not exist.  
If `error` is `t`, an error is returned if the category does not exist.


## The Fuction `eastasian-get`

Get the number of characters corresponding to the East Asian Width category.

```lisp
(defun eastasian-get (string-designer) ...) -> (values IntplusNull symbol)

Input: string-designer
Output: intplusNull length of string or nil
Output: symbol category
```

EastAsianWidth is a representation of Unicode character width.  
The input `string-designer` receives six categories,
`N`, `A`, `H`, `W`, `F`, and `NA`.  
The number of characters for each category is returned.  
In case of an error, `NIL` is returned.


## The Fuction `eastasian-width`

A length that takes into account the East Asian Width will be returned.

```lisp
(defun eastasian-width (var) ...) -> (values IntplusNull boolean)

Input: var  (or integer character string)
Output: IntplusNull length of string or nil
Output: boolean
```

The input `var` can be a character, a integer, or a string.  
If it is a integer, it is assumed to be a character code.  

### Example

```lisp
* (eastasian-width #\A)
1
T
* (eastasian-width #\u3042)
2
T
* (eastasian-width #x3044)
2
T
```
