% Amalgamation

Npt documentation.

Reference: [ANSI Common Lisp npt](index.html)  
Prev: [3. Arguments](A3_Arguments.html)  
Next: [5. Specific Features](A5_Features.html)


# 4.1 Npt Amalgamation

The npt amalgamation is a collection of npt sources into a few pieces.  
Currently, there are over 800 npt source files,
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
for example `gdb`, would take a long time to load the source.  
According to the sqlite page, some software cannot read
the source files if they are too large.

Therefore, after merging the sources,
we have also prepared a mode to split them into several files.
It depends on the size of the source,
but at the moment it can be combined into 12 files.

npt-amalgamation is available at the following github

https://github.com/nptcl/npt-amalgamation  

However, the above page does not always reflect the latest source.  
So, the following is how to make an amalgamation using the source from github/npt.

# 4.2 Creation

First, go to the github/npt directory.

```
$ cd github/npt
```

Go to the amalgamation directory.

```
$ cd develop/amalgamation
```

Run the program to create the file.

```
$ npt --script amalgamation-single.lisp
Name: npt
Output: lisp.c
Output: lisp.h
Output: shell.c
```

This `lisp` file can be executed by any Common Lisp implementation.  
If the `npt` command is not available, you can use another implementation.  
Here is an example.

```
$ sbcl --script amalgamation.lisp
$ ccl -l amalgamation.lisp
$ clisp amalgamation.lisp
```

With the three generated files, npt can be built.  
Compilation is the same as usual.  
Here's an example of compiling on FreeBSD

```
$ cc -o npt -DLISP_FREEBSD lisp.c shell.c -lm
$ ./npt --version-script | grep amalgamation
amalgamation    true
$
```

The following is an example of splitting into multiple parts.
This is a separate operation from the previous one,
so be careful not to continue working on it.

```
$ npt --script amalgamation-header.lisp
Name: npt
Output: lisp_file.h
Output: lisp_file_01.c
Output: lisp_file_02.c
Output: lisp_file_03.c
Output: lisp_file_04.c
Output: lisp_file_05.c
Output: lisp_file_06.c
Output: lisp_file_07.c
Output: lisp_file_08.c
Output: lisp_file_09.c
Output: lisp.h
Output: shell.c
```

The compilation is as follows.

```
$ cc -o npt -DLISP_FREEBSD lisp_file_*.c shell.c -lm
$ ./npt --version-script | grep amalgamation
amalgamation    true
$
```

`lisp_file.h` is used when compiling the npt source.  
On the other hand, `lisp.h` is a file that is required
when developing npt and is not needed when compiling npt.
