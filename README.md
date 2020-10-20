# Npt
Npt is an ANSI Common Lisp Programming Language.


# Make (script)
## FreeBSD
```
$ ./bsd_release.sh
...
$ ./npt
*
```

## Linux
```
$ ./linux_release.sh
...
$ ./npt
*
```

## Install
```
$ su
# cp -i npt /usr/local/bin/.
```


# configure (autoconf, automake)
```
$ ./bootstrap.sh
$ ./configure
$ make
$ make install
```


# Example
## Prompt
```
$ ./npt
* (format t "Hello~%")
Hello
NIL
* ^D
$
```

## Command Line
```
$ ./npt --quit --eval '(format t "Hello~%")'
Hello
$
```


# Compile
## FreeBSD
```
$ cc -O3 -o npt -DLISP_FREEBSD -DLISP_PROMPT_EDITLINE -lm -ledit src/*.c
$ ./npt
*
```

## Linux
```
$ cc -O3 -o npt -DLISP_LINUX -DLISP_PROMPT_READLINE -lm -lreadline src/*.c
$ ./npt
*
```

## Windows
```
$ cc -o npt.exe -DLISP_WINDOWS -lm src/*.c
$ ./npt.exe
*
```


# License
[The Unlicense](LICENSE)


# Distribution
https://github.com/nptcl/npt
