# Npt
Npt is an ANSI Common Lisp Programming Language.


# 1. Make (script)
## FreeBSD
```
$ ./freebsd_release.sh
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


# 2. Make (autoconf, automake)
```
$ ./bootstrap.sh
$ ./configure
$ make
$ make install
```


# 3. Example
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


# 4. Compile
## FreeBSD
```
$ cc -O3 -o npt -DLISP_FREEBSD -lm src/*.c
$ ./npt
*
```

## Linux
```
$ cc -O3 -o npt -DLISP_LINUX -lm src/*.c
$ ./npt
*
```

## Windows
```
$ cc -o npt.exe -DLISP_WINDOWS -lm src/*.c
$ ./npt.exe
*
```


# 5. Using readline / editline
## FreeBSD (editline)
- Script  
  `$ ./build/freebsd_editline_release.sh`
- Compile  
  `$ cc -O3 -o npt -DLISP_FREEBSD -DLISP_EDITLINE -lm -ledit src/*.c`

## Linux (readline)
- Script  
  `$ ./build/linux_readline_release.sh`
- Compile  
  `$ cc -O3 -o npt -DLISP_LINUX -DLISP_READLINE -lm -lreadline src/*.c`


# Documentation

[https://nptcl.github.io/npt/docs/md/index.html](https://nptcl.github.io/npt/docs/md/index.html)  
[https://nptcl.github.io/npt/docs/index.html](https://nptcl.github.io/npt/docs/index.html)


# License
[The Unlicense](LICENSE)


# Distribution
https://github.com/nptcl/npt
