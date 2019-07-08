# Npt

Npt is a small Lisp Programming Language.

## Install (configure)

```
$ tar zxf npt-x.x.x.tar.gz
$ cd npt-x.x.x
$ ./configure
$ make
$ make install
```


## Install (script)

FreeBSD
```
$ tar zxf npt-x.x.x.tar.gz
$ cd npt-x.x.x
$ ./bsd_release.sh
...
$
```

Linux
```
$ tar zxf npt-x.x.x.tar.gz
$ cd npt-x.x.x
$ ./linux_release.sh
...
$
```

Install
```
$ su
# cp -i npt /usr/local/bin/.
```

## Example

Prompt
```
$ ./npt
* (format t "Hello~%")
Hello
NIL
* ^D
$
```

Command Line
```
$ ./npt --quit --eval '(format t "Hello~%")'
Hello
$
```

## License

[The Unlicense](LICENSE)


## Distribution

https://github.com/nptcl/npt

