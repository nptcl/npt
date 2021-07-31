% Specific Features

Npt documentation.

Reference: [ANSI Common Lisp npt](index.html)  
Prev: [4. Amalgamation](A4_Amalgamation.html)


# 5.1 Npt-specific features

This chapter introduces some of the features specific to npt.
The functions and variables used by npt are available in the `npt-system` package.

This section describes the following.

- [5.2 Command Arguments](#specific-2)
- [5.3 Environment Variables](#specific-3)
- [5.4 Running the Garbage Collector](#specific-4)
- [5.5 Saving Core Files](#specific-5)
- [5.6 End of process](#specific-6)
- [5.7 Invoking the editor with the `ed` function](#specific-7)
- [5.8 Action of the `require` function](#specific-8)
- [5.9 Operation of EastAsianWidth](#specific-9)
- [5.10 Loading `load-logical-pathname-translations`](#specific-10)
- [5.11 Using the `pathname`](#specific-11)
- [5.12 Initial value of `random-state`](#specific-12)
- [5.13 Arguments of `load` function](#specific-13)



# <a id="specific-2">5.2 Command Arguments</a>

The arguments of the npt command are stored in the following variable.

```lisp
npt-system::*arguments*
```

The value is an array, where the first element is the command `npt`
and the second and subsequent arguments are the arguments of the command.

As an example, suppose you execute the following command.


```
$ npt -- 10 20 30
*
```

The argument ``--`` specifies that the following arguments are
to be used in the npt command.  

The values are as follows

```
* npt-system::*arguments*
#("npt" "10" "20" "30")
```


# <a id="specific-3">5.3 Environment Variables</a>

Environment variables are stored in the following variable.

```lisp
npt-system::*environment*
```

The value is a hash table.  
For example, the following command is
used to get the value of a home directory `$HOME`.

```
$ npt
* (gethash "HOME" npt-system::*environment*)
"/home/username"
T
*
```


## <a id="specific-4">5.4 Running the Garbage Collector</a>

Use the following command to start the garbage collector.

```lisp
(npt-system:gc)
```

The garbage collector is automatically run if the heap area is overwhelmed,
but depending on the timing, it may not be judged in time and you may run out of memory.  
If there is an out-of-memory situation, a `LISP ABORT` is triggered.

Here is the function specification.  
[Lisp Function: System Function](D1_System.html#system-develop)


## <a id="specific-5">5.5 Saving Core Files</a>

The following command will create the core file.

```lisp
(npt-system:savecore file)
```

This instruction executes the `savecore` condition of the `serious-condition`
to terminate the npt.  
When it finishes with the correct `savecore` condition,
it starts to save the core file.

To read the core file you created, use the `--core` argument of the npt command.

Here is the function specification.  
[Lisp Function: System Function](D1_System.html#system-develop)

## <a id="specific-6">5.6 End of process</a>

Exit npt with the following command

```lisp
(npt-system:exit code)
(npt-system:quit code)
```

The above two functions are exactly the same.   
They are `imported` into the `common-lisp-user` package by default so that you can run `(exit)` and `(quit)` at the npt prompt.   
This instruction executes the `exit` condition of the `serious-condition`
to terminate the npt.  

When it finishes with the correct `exit` condition,
the argument is set to the exit code to finish the process.

Here is the function specification.  
[Lisp Function: System Function](D1_System.html#system-develop)


## <a id="specific-7">5.7 Invoking the editor with the `ed` function</a>

The Common Lisp function `ed` invokes an editor.  
This feature is supported by FreeBSD, Linux and Windows.

It is configurable to get the command names of the editors in the following order.

1. get a string from `npt-system::*ed-program*`.
2. get a string from the `EDITOR` environment variable
3. get the standard editor name

The standard editor is `vi` for FreeBSD/Linux,
`notepad.exe` for Windows, or `ed` for others.


## <a id="specific-8">5.8 Action of the `require` function</a>

The function `require` in Common Lisp can read a module from its name.  
The implementation in npt is almost the same as in sbcl.

When the `require` is executed, the variable
`npt-system::*module-provider-functions*` is read as a list of functions,
and the functions are executed with the `require` arguments in order from the first one.  

If any function returns a value other than `NIL`,
it considers the loading to be successful and exits.

Here is the function specification.  
[Lisp Function: System Function](D1_System.html#system-develop)

```lisp
(defun require-tmp (var)
  (load (merge-pathnames
          (format nil "~(~A~).lisp" var)
          #p"/tmp/")))

(push #'require-tmp npt-system::*module-provider-functions*)

(require 'aaa)
```

The following are the results of the execution. (but a failure example)

```
ERROR: NPT-SYSTEM::SIMPLE-FILE-ERROR
Cannot open file #P"/tmp/aaa.lisp".

 0. ABORT            Return to eval-loop.
[1]* 0
```

It shows that `require` tried to read the file `/tmp/aaa.lisp` and failed.


## <a id="specific-9">5.9 Operation of EastAsianWidth</a>

EastAsianWidth is a representation of Unicode character width.  
For example, in the case of Japanese, the width of a character may be treated as two letters of the alphabet.
The width of a character is used to format the output of `format` or Pretty Printing.

The following three functions are related to EastAsianWidth.

```lisp
(defun eastasian-width (var) ...)
(defun eastasian-get (var) ...)
(defun eastasian-set (var size &optional errorp) ...)
```

Here is the function specification.  
[Lisp Function: System Function](D1_System.html#others)

### The function `eastasian-width`

Returns the size of EastAsianWidth from an object.

If the argument `var` is an integer, its size is returned as a Unicode character.  
If the argument `var` is a character, the size of the character is returned.  
If the argument `var` is a string, the total size of all characters in the string is returned.

The first returned value is the size of EastAsianWidth.  
The second return value is a boolean value to show whether the size is correct or not.


### The function `eastasian-get`

EastAsianWidth categorizes characters into
6 categories: `N`, `A`, `H`, `W`, `F`, and `NA`.
This function gets the number of characters in each category.
The argument `var` is a `string-designer` and represents the name of the category.

The first returned value is the size of EastAsianWidth.  
The second return value is a symbol for the category, and the error is `NIL`.

Example.

```lisp
* (npt-system:eastasian-get :na)
1
NPT-SYSTEM::NA

* (npt-system:eastasian-get :hello)
0
NIL
*
```


### The function `eastasian-set`

Set the size of a category of EastAsianWidth.

The argument `var` is a symbol for the category.  
The argument `size` is the size of the category.  
The argument `&optional errorp` is whether the `error` condition occurs or not.


## <a id="specific-10">5.10 Loading `load-logical-pathname-translations`</a>

The function `load-logical-pathname-translations` is a feature to load the settings of a logical pathname from a file.  
The location of the file is implementation-dependent.  
npt searches for files based on the following special variables.

```lisp
npt-system::*load-logical-pathname-translations*
```

The function `load-logical-pathname-translations` takes
a string as an argument and loads the corresponding configuration file.  
For example, suppose that the following is executed

```lisp
(load-logical-pathname-translations "hello")
```

Suppose the configuration files are located in

```
/tmp/host/hello.txt
```

In this case, the value of the special variable should be set as follows

```lisp
(setq npt-system::*load-logical-pathname-translations* #p"/tmp/host/*.txt")
```

Let's check the operation.  
First, create a configuration file.
Here are the contents of the file.

```lisp
("*.*" "/home/lisp/")
("path;to;*.*" "/home/path/")
```

Create a configuration file.

```
$ cd /tmp
$ mkdir host
$ cd host
$ vi hello.txt
Enter the above information
$
```

npt to execute the following commands

```lisp
(load-logical-pathname-translations "hello")
-> t

(translate-logical-pathname "hello:name.txt")
-> #P"/home/lisp/name.txt"


(translate-logical-pathname "hello:path;to;name.txt")
-> #P"/home/path/name.txt"
```

It is converted correctly.


## <a id="specific-11">5.11 Using the `pathname`</a>

The `pathmame` is an object for the name of a file.

The `pathname-host` specifies the environment to which the file name belongs.  
The following values are used by npt

| `pathname-host` | Environment |
| --- | --- |
| `npt-system::unix` | Unix |
| `npt-system::windows` | Windows |
| string-tyoe | logical-pathname |

The default is the value of `host` set to
the `pathname` object of the `*default-pathname-defaults*`.
The `parse-namestring` function determines how to analyze
a string according to the value of `host`.  
Because the behavior is determined by the value of `host`,
for example, even if the system is running on a Unix system,
you can recognize Windows file names.

For example, the following.

```lisp
* (parse-namestring "C:\\Windows\\" 'npt-system::windows)
#P"C:\\Windows\\"
11
* (pathname-directory *)
(:ABSOLUTE "Windows")
```

The function `equal` allows you to check whether the `pathname` is the same or not.  
If the `namestring` is the same but the `host` is different, they are not the same.

```lisp
* (parse-namestring "notepad.exe" 'npt-system::windows)
#P"notepad.exe"
11
* (parse-namestring "notepad.exe" 'npt-system::unix)
#P"notepad.exe"
11
* (equal ** *)
NIL
```

The `equal` function checks the value of `host` and evaluates the case of a string.  
On Unix, the value of `host` is considered
to be different from the case of the string,
and on Windows, it is considered to be the same.

```lisp
* (equal
    (parse-namestring "Hello.TXT" 'npt-system::unix)
    (parse-namestring "hello.txt" 'npt-system::unix))
NIL
* (equal
    (parse-namestring "Hello.TXT" 'npt-system::windows)
    (parse-namestring "hello.txt" 'npt-system::windows))
T
```

The `pathname-device` is ignored for Unix and logical paths.  
On Windows, it is set to the type of the file or the drive letter.

```lisp
* (pathname-device (parse-namestring "C:\\Windows\\" 'npt-system::windows))
"C"
* (pathname-device (parse-namestring "notepad.exe" 'npt-system::windows))
NIL
* (pathname-device (parse-namestring "\\\\.\\COM1" 'npt-system::windows))
NPT-SYSTEM::DEVICE
```

The `pathname-version` is ignored on Unix and Windows,
and is used by rules for logical paths.  

```lisp
* (pathname-version (logical-pathname "test:hello.txt"))
:NEWEST
* (pathname-version (logical-pathname "test:hello.txt.999"))
999
```


## <a id="specific-12">5.12 Initial value of `random-state`</a>

The `random-state` is an object used to generate a random number.  
npt uses a random number generator called xorshift,
which holds a 128-bit internal state in the `random-state`.

The initial value can be set to as random as possible
by passing the argument `t` to the function `make-random-state`.  
For example

```lisp
* (make-random-state t)
#<RANDOM-STATE #x85A5B416389D9716A81B4F43F76E922A>
* (make-random-state t)
#<RANDOM-STATE #xE51C94EB01856FEAC5B1EC8C90E3107E>
```

Determining the initial value of a random number is not easy,
so basically the operating system sets the initial value.  
This chapter describes how to get the initial value of a random number.


### FreeBSD / Linux

If the compilation mode is FreeBSD or Linux,
get the initial value of the random number from the file `/dev/urandom`.

Note that the device to be read is not `/dev/random`.  
On FreeBSD, there is no difference between the two,
but on Linux, `/dev/random` is more secure.  
npt has chosen to use `/dev/urandom` for convenience rather than safety.

Takes 256 bytes of data from `/dev/urandom` and sends it to MD5 hash
and set the internal state of MD5 to the internal state of xorshift as it is.  

If reading `/dev/urandom` fails, it is an error.


### Windows

If the compilation mode is on Windows,
get the initial value from the function `SystemFunction036`,
commonly known as the `RtlGenRandom` function of `Advapi32.dll`.

Takes 256 bytes of data from `RtlGenRandom` and sends it to MD5 hash
and set the internal state of MD5 to the internal state of xorshift as it is.  

If reading `RtlGenRandom` fails, it is an error.


### ANSI-C

Attempt to read the file `/dev/urandom`.  
If the value is obtained, it is used as the initial value.  
If the value can't be obtained, it is given up and
the time and so on are used as the initial value of the random number.

Unlike FreeBSD, Linux, and Windows, it continues without error
even if the device could not be loaded for the initial value.


## <a id="specific-13">5.13 Arguments of `load` function</a>

The `load` function can load either a text file containing Lisp expressions
or a binary file generated by the `compile-file` function
(commonly known as a FASL file).  
Which one to load is determined by whether `pathname-type` is `FASL`
or not, but the type can also be specified by the
`:type` argument of the `load` function.

To load a text file.

```lisp
(load file :type :lisp)
```

To load a fasl file.

```lisp
(load file :type :fasl)
```

The first argument of the `load` function is not only `pathname`,
but can also be `memory-stream`.  
However, since `memory-stream` does not have a `pathname`,
you need to specify the `:type` argument.

Here is an example of execution.

```lisp
(let ((input (npt-system:make-memory-io-stream))
      (output (npt-system:make-memory-io-stream)))
  (with-open-file (stream input :direction :output)
    (format stream "(format t \"Hello~~%\")"))
  (with-open-file (stream input :direction :input)
    (compile-file stream :output-file output))
  (file-position output :start)
  (load output :type :fasl))
```

Here are the execution results.

```
Hello
T
```

In this example, a text file is written to the `input` variable,
and a fasl file is output to the `output` variable
by the `compile-file` function.  
After returning the file pointer of the output to the top,
execute the fasl file generated by the `load` function.
