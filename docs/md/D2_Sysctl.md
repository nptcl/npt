% Lisp Function: sysctl

Npt documentation.

Reference: [ANSI Common Lisp npt](index.html)


# Lisp Function Specification

The following functions of the `npt-system` package are described.

- [sysctl](#sysctl-1)

```lisp
defun sysctl
```

- [sysctl: memory-stream](#sysctl-2)

```lisp
sysctl: memory-stream, size
sysctl: memory-stream, array
sysctl: memory-stream, cache
```

- [sysctl: clos](#sysctl-3)

```lisp
sysctl: clos, slots
```

- [sysctl: recovery](#sysctl-4)

```lisp
sysctl: recovery, no-applicable-method
sysctl: recovery, no-next-method
```

- [sysctl: structure](#sysctl-5)

```lisp
sysctl: structure, check
sysctl: structure, delete
sysctl: structure, type
```

- [sysctl: random-state](#sysctl-6)

```lisp
sysctl: random-state, integer
sysctl: random-state, make
sysctl: random-state, write
```


# <a id="sysctl-1">Function `sysctl`</a>

This function performs the following operations.

- Getting and setting the system state
- Getting and setting the state of an object

```lisp
(defun sysctl (type &rest args) ...) -> *

Input: type, object
Input: args
Output: *
```

The `type` can be any of the following

- `memory-stream`
- `clos`
- `recovery`
- `structure`
- `random-state`


# <a id="sysctl-2">Function `sysctl`: `memory-stream`</a>

Retrieves information from a `memory-stream` object.  
Execution is done as follows.

```lisp
* (setq x (make-memory-io-stream ...))
* (sysctl x ...)
```

Continue accepting the next argument.

- `size`
- `array`
- `cache`


## `sysctl`: `memory-stream`, `size`

Get the `size` of `memory-stream`.  
`size` is the number of bytes of the internal buffer.  
An example of execution is shown below.

```lisp
* (setq x (make-memory-io-stream :size 10))
* (sysctl x 'size)
10
T
```


## `sysctl`: `memory-stream`, `array`

Get the `array` of `memory-stream`.  
`array` is the number of internal buffers.  
An example of execution is shown below.

```lisp
* (setq x (make-memory-io-stream :array 20))
* (sysctl x 'array)
20
T
```


## `sysctl`: `memory-stream`, `cache`

Get the `cache` of `memory-stream`.  
The `cache` is whether the `open` function should use the cache or not.  
An example of execution is shown below.

```lisp
* (setq x (make-memory-io-stream :cache t))
* (sysctl x 'cache)
T
T
```


# <a id="sysctl-3">Function `sysctl`: `clos`</a>

Get information from a `clos` object.  
A `clos` object is anything created by `make-instance`,
including instances of `structure-object`, `structure-object`.  
It also includes instances of `standard-class`, `structure-class`,
and `built-in-class`.  
However, corresponding instances of class of `built-in-class` are
not included because they are Lisp objects.

Execution is done as follows.

```lisp
* (setq x (make-instance ...))
* (sysctl x ...)
```

Continue accepting the next argument.

- `slots`


## `sysctl`: `clos`, `slots`

Get all slots.  
An execution example is shown below.

```lisp
* (sysctl (find-class 'class) 'slots)
(NPT-CLOS::NAME NPT-CLOS::DIRECT-SLOTS NPT-CLOS::DIRECT-SUBCLASSES
 NPT-CLOS::DIRECT-SUPERCLASSES NPT-CLOS:CLASS-PRECEDENCE-LIST NPT-CLOS::EFFECTIVE-SLOTS
 NPT-CLOS::FINALIZED-P NPT-CLOS::PROTOTYPE NPT-CLOS::DEFAULT-INITARGS
 NPT-CLOS::DIRECT-DEFAULT-INITARGS NPT-CLOS::VERSION NPT-CLOS::DOCUMENTATION
 NPT-CLOS::REDEFINED-CLASS)
T
```


# <a id="sysctl-4">Function `sysctl`: `recovery`</a>

`recovery` restores the specified content to its initial state.  
The execution is done as follows.

```lisp
* (sysctl 'recovery ...)
```

The `'recovery` argument is treated as a `string` and is checked with `equalp`.


It will continue to accept the next argument.

- `no-applicable-method`
- `no-next-method`


## `sysctl`: `recovery`, `no-applicable-method`

Restore the generic function `no-applicable-method` to its initial state.  
The execution is as follows.

```lisp
* (sysctl 'recovery 'no-applicable-method)
T
T
```

## `sysctl`: `recovery`, `no-next-method`

Restore the generic function `no-next-method` to its initial state.  
The execution is as follows.

```lisp
* (sysctl 'recovery 'no-next-method)
T
T
```


# <a id="sysctl-5">Function `sysctl`: `structure`</a>

`structure` performs operations on structures.  
Execution is done as follows.

```lisp
* (sysctl 'structure ...)
```

The `'structure` argument is treated as a `string` and is checked with `equalp`.

It continues to accept the next argument.

- `check`
- `delete`
- `type`


## `sysctl`: `structure`, `check`

Checks for the existence of a structure with the specified name
that does not belong to the `structure-class`.

When you define a structure, a class belonging
to the `structure-class` is normally generated.  
However, if you specify `list` or `vector` as the `:type` argument of `defstruct`,
a structure different from the class system will be generated,
which cannot be checked by functions such as `find-class`.  
`check` will check for the existence of such a structure.

The execution is as follows.

```lisp
* (defstruct (aaa (:type list)))
* (sysctl 'structure 'check 'aaa)
T
T
* (defstruct bbb)
* (sysctl 'structure 'check 'bbb)
NIL
T
```


## `sysctl`: `structure`, `delete`

Deletes the structure with the specified name.

Removes both structures of `structure-class` and structures
that do not belong to it.  
Remove access functions, `constructor`, `copier`,
and `print-object` for structures.

The following is an example of execution.

```lisp
* (defstruct aaa)
* (sysctl 'structure 'delete 'aaa)
T
T
* (find-class 'aaa nil)
NIL

* (defstruct (bbb (:type vector)))
* (sysctl 'structure 'delete 'bbb)
T
T
* (sysctl 'structure 'check 'bbb)
NIL
T
* (fboundp 'make-bbb)
NIL
```


## `sysctl`: `structure`, `type`

Returns the type of the structure with the specified name.

The returned value is as follows

- `class`
- `list`
- `vector`
- (derivative of vector)

An example of how to do this is shown below.

```lisp
* (defstruct aaa)
* (defstruct (bbb (:type (vector (unsigned-byte 8)))))
* (sysctl 'structure 'type 'aaa)
CLASS
T
* (sysctl 'structure 'type 'bbb)
(VECTOR (UNSIGNED-BYTE 8))
T
```


# <a id="sysctl-6">Function `sysctl`: `random-state`</a>

Performs operations on `random-state` objects.  
Execution is done as follows.

```lisp
* (sysctl 'random-state ...)
```

The `'random-state` argument is treated as a `string` and is checked with `equalp`.

It continues to accept the next argument.

- `integer`
- `make`
- `write`


## `sysctl`: `random-state`, `integer`

Output the internal state of `random-state` as an integer.

npt implements `xorshift` as a random number algorithm,
and has 128-bit integer in the internal state of `random-state`.  
This function outputs the internal state as an integer.  
The output internal state can be restored by `make` and `write`.

The following is an example of execution.

```lisp
* (setq *print-base* 16)
* (setq x (make-random-state t))
#<RANDOM-STATE #xF3F85807E6E2837033526396D518DAD1>
* (sysctl 'random-state 'integer x)
F3F85807E6E2837033526396D518DAD1
T
```

## `sysctl`: `random-state`, `make`

Generates a `random-state` with the specified argument set to the internal state.

Since the argument is obtained by 128 bits with `ldb`, negative values can be set.

```lisp
* (setq *print-base* 16)
* (setq x (sysctl 'random-state 'make #xABCD))
#<RANDOM-STATE #xABCD>
T
* (sysctl 'random-state 'integer x)
ABCD
T
```


## `sysctl`: `random-state`, `write`

Specifies the internal state of a `random-state` object.

Since the argument is obtained by 128 bits with `ldb`, negative values can be set.

```lisp
* (setq *random-state* (make-random-state t))
#<RANDOM-STATE #x14F392860E2329DE919C083F0B764EC5>
* (setq y (sysctl 'random-state 'integer *random-state*))
27849259905073490890992780948155027141
T
* (random 10)
3
* (random 10)
6
* (random 10)
7
* (sysctl 'random-state 'write *random-state* y)
#<RANDOM-STATE #x14F392860E2329DE919C083F0B764EC5>
T
* (random 10)
3
* (random 10)
6
* (random 10)
7
```
