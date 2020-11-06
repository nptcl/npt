% Amalgamation

Npt documentation.

Reference: [ANSI Common Lisp npt](index.html)  
Prev: [3. Arguments](A3_Arguments.html)  
Next: [5. Specific Features](A5_Features.html)


# 4.1 Npt Amalgamation

The npt amalgamation is a collection of npt sources into a few pieces.  
Currently, there are over 600 npt source files,
but npt amalgamation combines them into the following three sources.

- `lisp.c`
- `lisp.h`
- `shell.c`

There is no difference in functionality between regular npt
and amalgamation as it is simply combined.
It was originally created by imitating 
[amalgamation](https://www.sqlite.org/amalgamation.html)
from [sqlite](https://www.sqlite.org/).  
In some cases, it is easier to handle
because the number of source files is reduced.

A drawback exists, the file `lisp.c` is so large that a C language debugger,
for example gdb, would take a long time to load the source.  

According to the sqlite page, some software cannot read
the source files if they are too large.

npt-amalgamation is available at the following github

https://github.com/nptcl/npt-amalgamation  
https://github.com/nptcl/npt-amalgamation.git


However, the above page does not always reflect the latest source.  
So, the following is how to make an amalgamation using the source from github/npt.

First, go to the github/npt directory.

```
$ cd github/npt
```

Go to the amalgamation directory.

```
$ cd develop/amalgamation
```

Run the program `amalgamation.lisp` to create the file.

```
$ npt --script amalgamation.lisp
```

This `lisp` file can be executed by any Common Lisp implementation.  
If the `npt` command is not available, you can use another implementation.  
Here is an example.

```
$ sbcl --script amalgamation.lisp
$ ccl -l amalgamation.lisp
$ clisp amalgamation.lisp
```

The generated files are as follows

- `lisp.c`
- `lisp.h`
- `shell.c`

Compilation is the same as usual.  
Here's an example of compiling on FreeBSD

```
$ cc -o npt -DLISP_FREEBSD lisp.c shell.c -lm
$ ./npt --version-script | grep amalgamation
amalgamation    true
$
```
